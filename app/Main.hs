{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified OldMain as OldMain
import qualified Hadolint.Rules as Rules
import qualified Codacy.Hadolint.Wrapper as Wrapper
import qualified Data.Set as Set

main :: IO ()
main = Wrapper.wrapper lint

lint :: Wrapper.Configs -> IO ()
lint config = OldMain.lint $ convertConfigs config

convertConfigs :: Wrapper.Configs -> OldMain.LintOptions
convertConfigs Wrapper.Configs {..} = OldMain.LintOptions {
    showVersion = False
    , configFile = configurationFile
    , format = OldMain.Codacy
    , ignoreRules = ignoredRules
    , dockerfiles = filesPaths
    , rulesConfig = Rules.RulesConfig Set.empty
}