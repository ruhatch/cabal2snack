module Cabal2Snack
       ( cabal2snack
       ) where

import           Data.List (partition)
import           Data.Semigroup ((<>))
import           Distribution.Compiler (AbiTag (..), buildCompilerId,
                     unknownCompilerInfo)
import           Distribution.Nixpkgs.Fetch (Hash (..), Source (..))
import           Distribution.Nixpkgs.Haskell.PackageSourceSpec (Package (..),
                     getPackage)
import           Distribution.System (buildPlatform)
import           Options.Applicative (Parser, ParserInfo, execParser, help,
                     helper, info, long, metavar, short, strOption, (<**>))
import           Snack.PackageSpec (NamedPackageSpec (..),
                     fromGenericPackageDescription)
import           Text.PrettyPrint.HughesPJClass (Pretty (..), nest, render,
                     text, vcat)

cabal2snack :: IO ()
cabal2snack = do
  opts <- execParser optionsP
  pkg <- getPackage False Nothing Nothing (Source "./." "" UnknownHash "")
  let pkgDescs = fromGenericPackageDescription
        buildPlatform
        (unknownCompilerInfo buildCompilerId NoAbiTag)
        (pkgCabal pkg)
  putStrLn $ renderLetIn (optMainComponent opts) pkgDescs

renderLetIn
  :: String
  -- ^ The name of the component to be use in the 'in' clause
  -> [NamedPackageSpec]
  -> String
renderLetIn mainComponent pkgDescs = render $ vcat
  [ text "let"
  , nest 2 $ vcat $ pPrint <$> rest
  , text "in"
  , pPrint $ npsPkgSpec mainPkgSpec
  ]
 where
  ([mainPkgSpec], rest) = partition ((== mainComponent) . npsName) pkgDescs

newtype Options = Options
  { optMainComponent :: String
  }

optionsP :: ParserInfo Options
optionsP = info (parser <**> helper) mempty
 where parser = Options <$> mainComponentP

mainComponentP :: Parser String
mainComponentP = strOption
  (  short 'm'
  <> long "main"
  <> metavar "MAIN_COMPONENT"
  <> help
    (  "The main component of this project. Executables should be written 'exe-'"
    <> " followed by their cabal name."
    )
  )
