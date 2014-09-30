module Main where

import qualified Data.ByteString.Char8 as BS (unpack, readFile)
import Data.Data.Lens (template)
import Control.Applicative ((<$>))

import Prelude hiding (readFile)
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)

import Language.Java.Syntax -- ()
import Language.Java.Pretty -- ()
import qualified Language.Java.Parser as P -- ()

import Control.Lens

packageDecl :: Lens' CompilationUnit (Maybe PackageDecl) -- (m pd -> f m pd) -> cu -> f cu
packageDecl f (CompilationUnit a b c) = fmap (\a' -> CompilationUnit a' b c) (f a)

readFile :: FilePath -> IO String
readFile = fmap BS.unpack . BS.readFile

parseFile :: FilePath -> IO (Either ParseError CompilationUnit)
parseFile fname = P.parser P.compilationUnit <$> readFile fname

printList :: (Show a) => [a] -> IO ()
printList = putStr . unlines . map show

main :: IO ()
main = do
    args <- getArgs
    Right cu <- parseFile (head args)
    putStrLn $ unlines $ map prettyPrint $ findMethodCalls cu

parseFileList :: [FilePath] -> IO [(FilePath, Either ParseError CompilationUnit)]
parseFileList = mapM (\f -> do
                        a <- parseFile f
                        return (f, a))

findMethodCalls :: CompilationUnit -> [MethodInvocation]
findMethodCalls compUnit = (^..) compUnit template
