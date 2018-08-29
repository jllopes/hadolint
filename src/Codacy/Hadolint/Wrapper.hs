{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Codacy.Hadolint.Wrapper (
    wrapper,
    Configs(..)
) where

import qualified Data.ByteString.Lazy.Char8 as B
import Codacy.Hadolint.Configuration
import Data.Aeson hiding (Result)
import Data.Text (Text, pack)
import Data.List (find, (\\))

type IgnoreRule = Text
type FileName = String

data Configs = Configs
    { configurationFile :: Maybe FileName
    , ignoredRules :: [IgnoreRule]
    , filesPaths :: [String]
    } deriving (Show)

readPatternsFile :: IO B.ByteString
readPatternsFile = B.readFile "/docs/patterns.json"

parsePatternsFile :: B.ByteString -> Either String PatternList
parsePatternsFile b = eitherDecode b

readConfigFile :: IO B.ByteString 
readConfigFile = B.readFile ".codacy.json"

parseConfigFile :: B.ByteString -> Either String CodacyConfig
parseConfigFile b = eitherDecode b

convertConfig :: [DocsPattern] -> [String] -> Tool -> IO Configs
convertConfig docs files (Tool _ (Just patterns)) = return Configs 
    { configurationFile = Nothing
    , ignoredRules = ignoredFromPatterns docs patterns
    , filesPaths = files
    }
convertConfig _ files (Tool _ Nothing) = return Configs 
    { configurationFile = Just ".hadolint.yaml"
    , ignoredRules = []
    , filesPaths = files
    }

ignoredFromPatterns :: [DocsPattern] -> [Pattern] -> [IgnoreRule]
ignoredFromPatterns allPatterns configPatterns = map pack patternsToIgnore
    where
        patternsToIgnore = allPatternIds \\ configPatternIds
        allPatternIds = map (\rule -> patternId (rule :: DocsPattern)) allPatterns
        configPatternIds = map (\rule -> patternId (rule :: Pattern)) configPatterns

findTool :: [Tool] -> Maybe Tool
findTool = find (\tool -> name tool == "hadolint")

wrapper :: (Configs -> IO ()) -> IO ()
wrapper lint = do
    configFileContent <- readConfigFile
    patternsFileContent <- readPatternsFile
    let Right (CodacyConfig files tools) = parseConfigFile configFileContent
        Just tool = findTool tools
        Right (PatternList parsedPatterns) = parsePatternsFile patternsFileContent
    convertedConfig <- convertConfig parsedPatterns files tool
    lint convertedConfig
