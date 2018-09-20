module Cabal2Snack
       ( cabal2snack
       ) where

import           Data.List (partition)
import           Distribution.Compiler (AbiTag (..), buildCompilerId,
                     unknownCompilerInfo)
import           Distribution.Nixpkgs.Fetch (Hash (..), Source (..))
import           Distribution.Nixpkgs.Haskell.PackageSourceSpec (Package (..),
                     getPackage)
import           Distribution.System (buildPlatform)
import           Options.Applicative (execParser)
import           Text.PrettyPrint.HughesPJClass (Pretty (..), nest, render,
                     text, vcat)

import           Cabal2Snack.Options (Options (..), optionsP)
import           Snack.PackageSpec (NamedPackageSpec (..),
                     fromGenericPackageDescription)

cabal2snack :: IO ()
cabal2snack = do
  opts <- execParser optionsP
  pkg  <- getPackage False Nothing Nothing (Source "./." "" UnknownHash "")
  let
    pkgDescs = fromGenericPackageDescription
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
