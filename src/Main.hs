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
import qualified Data.Text.Lazy.IO as T

import           Data.Foldable
import           Data.Maybe
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
convert x = T.unlines $ ("___ImageBase equ 0x401000":) $ convertLine <$> (drop 7 $ T.lines x)

choose :: a -> [Maybe a] -> a
choose x = foldr (flip fromMaybe) x

thread :: a -> [a -> Maybe a] -> a
thread x l = foldl' f x l
  where f old g = fromMaybe old $ g old

(><) :: T.Text -> T.Text -> Maybe (T.Text, T.Text, T.Text, [T.Text])
t >< t2 = t =~~ t2

convertLine :: T.Text -> T.Text
convertLine x = fromMaybe (error $ "bad shit: " ++ T.unpack x) $ do
  let (addr, r)      = T.splitAt 8 x
      (colon, instr) = T.splitAt 1 r
  guard $ ":" == colon

  return $ thread instr $
    [ \instr -> do (before, _, after, [g1, g2]) <- instr >< "([0-9a-z]*) *<(.*)>$"
		   return $ T.concat [before, g2, " ;RAW: ", g1, after]
    , let f instr = do (before, _, after, [g]) <- instr >< "(.s:)\\["
                       return $ T.concat [before, "[" , g, after]
      in f >=> f -- need to do again if successful
    , \instr -> return $ T.replace " PTR" "" instr
    , \instr -> do (before, _, after, [op]) <- instr >< "(lods|movs|stos|scas|cmps|ins|outs)"
		   (_, _, _, [size])        <- instr >< "(BYTE|DWORD|WORD)"
		   let suffix = case T.unpack size of
			 "BYTE"  -> 'b'
			 "DWORD" -> 'd'
			 "WORD"  -> 'w'

		   return $ T.concat [before, T.snoc op suffix, after]
    , const mzero
    ]








asmFiles :: FilePath -> IO [FilePath]
asmFiles root = F.find (always)
		       (and <$> sequence [ fileType ==? RegularFile
					 , extension ==? ".asm" ||? extension ==? ".inc"
					 ])
		       root

allFiles :: IO [FilePath]
allFiles = fmap concat $ mapM asmFiles $ ("/home/jcericso/git/engine2/" </>)
	 <$> ["shared/" , "ra2/src" , "ra2/inc" , "ts/src" , "ts/inc"]

allSyms :: IO (Set T.Text)
allSyms = fold <$> (mapM f =<< allFiles)
  where f :: FilePath -> IO (Set T.Text)
	f p = do body <- T.readFile p
		 let m  = T.words <$> T.lines body
		     m' = flip mapMaybe m $ \case
		       (a:b:_) -> do guard $ a == "cextern" || a == "cglobal"
				     return b
		       _       -> mzero
		 return $ S.fromList m'

tokenize = T.groupBy $ \c1 c2 -> not $ S.member c2 split || S.member c1 split
  where split = S.fromList "[]()<>{} ,+-/*"

prepend :: IO ()
prepend = do
  set <- allSyms
  let f :: T.Text -> T.Text
      f x = T.unlines $ fmap (i . T.concat) $ (fmap . fmap) (h . g) $ fmap tokenize $ T.lines x

      g :: T.Text -> T.Text
      g x = if S.member x set
	       || T.isPrefixOf "str_" x
	    then T.cons '_' x
	    else x

      h x
	| x == "cglobal" = "global"
	| x == "cextern" = "extern"
	| otherwise      = x

      i = T.replace "StringZ _" "StringZ "

  mapM_ (F.modifyInPlace f) =<< allFiles

instance F.Streamable T.Text where
  readAll = T.hGetContents
  writeAll = T.hPutStr
