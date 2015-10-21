module Qiita
    ( run
    ) where

import Control.Applicative
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Options.Applicative
import System.Environment
import System.Exit
import System.IO (stdout)
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

snptFile' :: Int -> Snippet -> ByteString
snptFile' n snpt = num `C.append` file
    where num = C.pack $ "[" ++ (show n) ++ "] "
          file = fromMaybe (C.pack "\"\"") $ snptFile snpt

printSnippetList :: Article -> IO ()
printSnippetList article = do
  C.putStrLn $ (C.pack "Title: ") `C.append` aTitle article
  mapM_ print1 $ zip [1..] $ aSnippets article
    where print1 (n, snpt) = C.putStrLn $ snptFile' n snpt

processSnippets :: Options -> Article -> IO ()
processSnippets opts article = do
  let snpt = aSnippets article
      c = length snpt
      n = fromMaybe (-1) (numSnippet opts)
      idx = case (c, 0 <= n || n < c) of
              (1, _) -> 0
              (_, True) -> n
              _ -> -1
      out = fromMaybe stdout
  when (c == 0) $ do
    putStrLn "No snippet found"
    exitFailure
  if idx == -1 then
      do printSnippetList article
  else
      do C.putStrLn $ snptFile' n $ snpt !! idx
         C.putStrLn $ snptCode $ snpt !! idx

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
