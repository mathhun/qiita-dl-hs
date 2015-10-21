module Qiita
    ( run
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Options.Applicative
import System.Environment
import Text.HTML.Scalpel
import Text.Printf

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

printSnippetList :: Article -> IO ()
printSnippetList article = do
  C.putStrLn $ (C.pack "Title: ") `C.append` aTitle article
  mapM_ print1 $ zip [1..] $ aSnippets article
    where print1 (n, snpt) = putStrLn $ printf "[%s] %s" (show n) (C.unpack $ snptFile' snpt)

snptFile' :: Snippet -> ByteString
snptFile' = fromMaybe (C.pack "\"\"") . snptFile

runDownloadSnippets :: Article -> IO ()
runDownloadSnippets article = do
  let snpt = head $ aSnippets article
  C.putStrLn $ snptFile' snpt
  C.putStrLn $ snptCode snpt

--
-- Entry point
--

run :: IO ()
run = do
  opts <- execParser myParserInfo
  print opts
  let url = qiitaUrl opts
  article <- allSnippets url
  maybe printError processSnippets article
    where
      printError = putStrLn "ERROR: Could not scrape the URL"
      processSnippets article = do
          case length (aSnippets article) of
            0 -> putStrLn "No snippet found"
            1 -> runDownloadSnippets article
            _ -> printSnippetList article

--
-- Option parser
--

data Options = Options
    { qiitaUrl :: String
    , numSnippet :: Maybe Int
    , outFileName :: Maybe String
    , outDirName :: Maybe String
    } deriving Show

qiitaUrlP :: Parser String
qiitaUrlP = argument str (metavar "URL")

numSnippetP :: Parser Int
numSnippetP = option auto $ mconcat [short 'n', long "num", help "# of snippet", metavar "INT"]

outFileNameP :: Parser String
outFileNameP = strOption $ mconcat [short 'o', long "output", help "output file", metavar "FILE"]

outDirNameP :: Parser String
outDirNameP = strOption $ mconcat [short 'd', long "dir", help "output directory", metavar "DIR"]

optionsP :: Parser Options
optionsP = (<*>) helper $ Options <$> qiitaUrlP
           <*> optional numSnippetP
           <*> optional outFileNameP
           <*> optional outDirNameP

myParserInfo :: ParserInfo Options
myParserInfo = info optionsP $ mconcat [fullDesc, progDesc "Qiita Download", header "", footer ""]

-- option -o (filename)
-- option -n to specify snpt
-- option -d (directory)
-- option -a to download all
