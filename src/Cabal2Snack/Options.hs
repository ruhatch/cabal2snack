module Cabal2Snack.Options
       ( Options (..)
       , optionsP
       ) where

import           Data.Semigroup ((<>))
import           Options.Applicative (Parser, ParserInfo, help, helper, info,
                     long, metavar, short, strOption, (<**>))

newtype Options = Options
  { optMainComponent :: String
  }

optionsP :: ParserInfo Options
optionsP = info (parser <**> helper) mempty
 where parser = Options <$> mainComponentP

mainComponentP :: Parser String
mainComponentP =
  strOption $ short 'm' <> long "main" <> metavar "MAIN_COMPONENT" <> help
    (  "The main component of this project. Executables should be written "
    <> "'exe-' followed by their cabal name."
    )
