{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -fdefer-type-errors #-}

module Main where

import           Prelude hiding (map, sequence, mapM, mapM_, and, concat, foldl, foldr)

import           Control.Applicative
import           Control.Monad hiding (sequence, mapM, mapM_)

import           Data.Set (Set(..))
import qualified Data.Set as S

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as T

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable

import           System.IO
import           System.Environment

import           System.FilePath

import           System.FilePath.Find as F hiding (find, fold)
import qualified System.FilePath.Find as F

import           System.FilePath.Find as F hiding (find, fold)
import qualified System.FilePath.Manip as F

import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text.Lazy


main :: IO ()
main = do
  [p1,p2] <- getArgs
  withFile p1 ReadMode $ \fin ->
    withFile p2 WriteMode $ \fout ->
      T.hPutStr fout =<< (convert <$> T.hGetContents fin)

convert :: T.Text -> T.Text
convert x = TB.toLazyText
	    $ (TB.fromLazyText "___ImageBase equ 0x401000\n" <>)
	    $ (TB.fromLazyText "%include \"sym.asm\"\n" <>)
	    $ foldr (<>) (TB.fromLazyText "") $ convertLine <$> (drop 7 $ T.lines x)

choose :: a -> [Maybe a] -> a
choose x = foldr (flip fromMaybe) x

thread :: a -> [a -> Maybe a] -> a
thread x l = foldl' f x l
  where f old g = fromMaybe old $ g old

(t :: T.Text) >< (t2 :: T.Text) = t =~~ t2

convertLine :: T.Text -> TB.Builder
convertLine x = TB.singleton '\n' <> TB.fromLazyText instr' <> TB.fromLazyText " ; " <> TB.fromLazyText addr
  where (addr, instr) = T.splitAt 32 x
	instr' = thread instr [
	    \instr -> do (before, _ :: T.Text , after, [g1, g2]) <- instr >< "([0-9a-z]*) *<(.*)>"
			 return $ T.concat [before, g2, " ;RAW: ", g1, after]

	  , let f instr = do (before, _ :: T.Text, after, [g]) <- instr >< "(.s:)\\["
			     return $ T.concat [before, "[" , g, after]
	    in \instr -> return $ thread instr [f, f]

          , let f instr = do (before, _ :: T.Text, after, [g1, g2, g3]) <- instr >< "(,| )(.s:0x[0-9a-fA-F]+)(,| |$)"
                             return $ T.concat [before, g1, "[" , g2, "]", g3, after]
            in \instr -> return $ thread instr [f, f]

	  , \instr -> return $ T.replace " PTR" "" instr

	  , \instr -> do (before, _ :: T.Text, _ :: T.Text, [op]) <- instr >< "(lods|movs|stos|scas|cmps|ins|outs) "
			 (_ :: T.Text, _ :: T.Text, _ :: T.Text, [size]) <- instr >< "(BYTE|DWORD|WORD)"
			 let suffix = case T.unpack size of
			       "BYTE"  -> 'b'
			       "DWORD" -> 'd'
			       "WORD"  -> 'w'
			 return $ T.concat [before, op `T.snoc` suffix]

	  , let f instr = do (before, _ :: T.Text, after, [g]) <- instr >< "st\\((.)\\)"
			     return $ T.concat [before, "st", g, after]
	    in \instr -> return $ thread instr [f, f]
          , let f instr = do (before, _ :: T.Text, after, [g1, g2]) <- instr >< "(,| )st(,| |$)"
			     return $ T.concat [before, g1, "st0", g2, after]
	    in \instr -> return $ thread instr [f, f]
	  ]
