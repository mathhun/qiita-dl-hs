module Qiita
    ( run
    ) where

import Control.Applicative
import Control.Monad (when, unless)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.FilePath.Posix ((</>), takeDirectory)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Posix.Files
import Text.HTML.Scalpel

data Article = Article { aTitle :: ByteString
                       , aSnippets :: [Snippet]
                       }

data Snippet = Snippet { snptFile :: Maybe ByteString
                       , snptCode :: ByteString
                       }

allSnippets :: URL -> IO (Maybe Article)
allSnippets url = scrapeURL url snippets

snippets :: Scraper ByteString Article
snippets = Article
           <$> text "title"
           <*> chroots ("div" @: [hasClass "code-frame"]) snippet

snippet :: Scraper ByteString Snippet
snippet = Snippet <$> file' <*> code
    where file' = Just <$> file <|> return Nothing
          file = text $ "div" @: [hasClass "code-lang"]
          code = text "pre"

snptFile' :: Int -> Snippet -> ByteString
snptFile' n snpt = num `C.append` file
    where num = C.pack $ "[" ++ (show n) ++ "] "
          file = fromMaybe (C.pack "\"\"") $ snptFile snpt

printSnippetList :: Article -> IO ()
printSnippetList article = do
  C.putStrLn $ (C.pack "Title: ") `C.append` aTitle article
  mapM_ print1 $ zip [0..] $ aSnippets article
    where print1 (n, snpt) = C.putStrLn $ snptFile' n snpt `C.append` preview snpt
          preview snpt = C.pack " " `C.append` takeText (snptCode snpt)
          takeText = T.encodeUtf8 . T.replace (T.pack "\n") (T.pack "\\n") . T.take 50 . T.decodeUtf8

processSnippets :: Options -> Article -> IO ()
processSnippets opts article = do
  let snpts = aSnippets article
      c = length snpts
      n = fromMaybe (-1) (numSnippet opts)
      idx = case (c, 0 <= n || n < c) of
              (1, _) -> 0
              (_, True) -> n
              _ -> -1
  when (c == 0) $ do
    putStrLn "No snippet found"
    exitFailure
  if idx == -1 then
      printSnippetList article
  else
      writeSnippet opts snpts idx

writeSnippet :: Options -> [Snippet] -> Int -> IO ()
writeSnippet opts snpts n = do
  let snpt = snpts !! n
      file = snptFile' n snpt
      code = snptCode snpt
  case getOutputPath opts (snptFile snpt) of
    Just path -> do checkFileExists opts path
                    C.writeFile path code
                    markFileExecutable opts path
    Nothing -> mapM_ C.putStrLn [file, code]

getOutputPath :: Options -> Maybe ByteString -> Maybe FilePath
getOutputPath opts file = do
  base <- outFileName opts <|> fmap C.unpack file
  dir <- outDirName opts <|> Just ""
  return $ dir </> base

checkFileExists :: Options -> FilePath -> IO ()
checkFileExists opts path = do
  exists <- doesFileExist path
  when (exists && not (forceOutput opts)) $ do
    C.hPutStrLn stderr $ C.pack "File exists: " `C.append` C.pack path
    exitFailure
  let dir = takeDirectory path
  dirExists <- doesDirectoryExist dir
  unless (dirExists) $ createDirectory dir

markFileExecutable :: Options -> FilePath -> IO ()
markFileExecutable opts file = do
  when (markExecutable opts) $ do
    setFileMode file (stdFileMode .|. ownerExecuteMode)

--
-- Entry point
--

run :: IO ()
run = do
  opts <- execParser myParserInfo
  article <- allSnippets (qiitaUrl opts)
  maybe printError (processSnippets opts) article
    where
      printError = putStrLn "ERROR: Could not scrape the URL"

--
-- Option parser
--

data Options = Options
    { qiitaUrl :: String
    , numSnippet :: Maybe Int
    , outFileName :: Maybe String
    , outDirName :: Maybe String
    , forceOutput :: Bool
    , markExecutable :: Bool
    } deriving Show

qiitaUrlP :: Parser String
qiitaUrlP = argument str (metavar "URL")

numSnippetP :: Parser Int
numSnippetP = option auto $ mconcat [short 'n', long "num", help "# of snippet", metavar "INT"]

outFileNameP :: Parser String
outFileNameP = strOption $ mconcat [short 'o', long "output", help "output file", metavar "FILE"]

outDirNameP :: Parser String
outDirNameP = strOption $ mconcat [short 'd', long "dir", help "output directory", metavar "DIR"]

forceOutputP :: Parser Bool
forceOutputP = switch $ mconcat [short 'f', long "force", help "force output"]

executableP :: Parser Bool
executableP = switch $ mconcat [short 'x', long "executable", help "executable"]

optionsP :: Parser Options
optionsP = (<*>) helper $ Options <$> qiitaUrlP
           <*> optional numSnippetP
           <*> optional outFileNameP
           <*> optional outDirNameP
           <*> forceOutputP
           <*> executableP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat [fullDesc, progDesc "Qiita Download", header "", footer ""]
