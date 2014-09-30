module Main where

import qualified Data.ByteString.Char8 as BS (unpack, readFile)
import Data.Either (isLeft)
import Control.Applicative ((<$>))

import Prelude hiding (readFile)
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)

import Language.Java.Syntax -- ()
import Language.Java.Parser -- ()

import Control.Lens

readFile :: FilePath -> IO String
readFile = fmap BS.unpack . BS.readFile

parseFile :: FilePath -> IO (Either ParseError CompilationUnit)
parseFile fname = parser compilationUnit <$> readFile fname

printList :: (Show a) => [a] -> IO ()
printList = putStr . unlines . map show

main :: IO ()
main = do
    args <- getArgs
    filelist <- lines <$> readFile (head args)
    cs <- mapM (\f -> parseFile f >>= (\cu -> return (f, cu))) filelist -- [(fname, Either err compunit)]
    printList $ filter (isLeft . snd) cs -- errors
