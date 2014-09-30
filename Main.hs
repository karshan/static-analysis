module Main where

import qualified Data.ByteString.Char8 as BS (unpack, readFile)
import Data.Either (rights, isLeft)
import Control.Applicative ((<$>))

import Prelude hiding (readFile)
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)

import Language.Java.Syntax -- ()
import Language.Java.Parser -- ()

import Control.Lens

cu_PackageDecl :: Lens' CompilationUnit (Maybe PackageDecl) -- (m pd -> f m pd) -> cu -> f cu
cu_PackageDecl f (CompilationUnit a b c) = fmap (\a' -> CompilationUnit a' b c) (f a)

readFile :: FilePath -> IO String
readFile = fmap BS.unpack . BS.readFile

parseFile :: FilePath -> IO (Either ParseError CompilationUnit)
parseFile fname = parser compilationUnit <$> readFile fname

printList :: (Show a) => [a] -> IO ()
printList = putStr . unlines . map show

main :: IO ()
main = do
    args <- return ["files"] --getArgs
    filelist <- lines <$> readFile (head args)
    parseResults <- mapM (\f -> parseFile f >>= (\cu -> return (f, cu))) filelist -- [(fname, Either err compunit)]
    printList $ filter (isLeft . snd) parseResults -- errors
    let compilationUnits = rights $ map snd parseResults
    print $ (head compilationUnits) ^. cu_PackageDecl
