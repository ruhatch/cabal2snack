module Snack.PackageSpec
       ( PackageSpec (..)
       , NamedPackageSpec (..)
       , fromGenericPackageDescription
       ) where

import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import           Data.Semigroup ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.PackageDescription (BuildInfo (..),
                     Executable (..), GenericPackageDescription, Library (..),
                     PackageDescription (..), mkFlagAssignment)
import           Distribution.PackageDescription.Configuration (finalizePD)
import           Distribution.Simple (CompilerFlavor (..), CompilerInfo,
                     Extension, PackageIdentifier (..), depPkgName,
                     unPackageName)
import           Distribution.System (Platform)
import           Distribution.Types.ComponentRequestedSpec
                     (ComponentRequestedSpec (..))
import           Distribution.Types.UnqualComponentName (unUnqualComponentName)
import           Language.Nix.PrettyPrinting (Doc, Pretty (..), attr, empty,
                     equals, lbrace, lbrack, nest, onlyIf, rbrace, rbrack,
                     semi, sep, text, vcat, (<+>))
import           System.FilePath (takeBaseName)

data NamedPackageSpec = NamedPackageSpec
  { npsName    :: String
  , npsPkgSpec :: PackageSpec
  }

instance Pretty NamedPackageSpec where
  pPrint nps = attrnewline (npsName nps) (pPrint $ npsPkgSpec nps)
   where attrnewline n v = vcat [text n <+> equals, nest 2 $ v <> semi]

data PackageSpec = PackageSpec
  { psSource           :: FilePath
  -- ^ This should really be a list, but snack doesn't support that yet
  , psMain             :: Maybe Text
  , psGhcOptions       :: Set String
  , psDependencies     :: Set String
  , psExtensions       :: Set Extension
  , psExtraFiles       :: Set String
  , psExtraDirectories :: Set String
  , psPackages         :: Set String
  }

instance Pretty PackageSpec where
  pPrint pkgSpec = vcat
    [ lbrace
    , nest 2 $ vcat
      [ attr "src" $ text $ psSource pkgSpec
      , maybe mempty (attr "main" . pPrint . T.unpack) $ psMain pkgSpec
      , setToListattr "ghcOpts" $ psGhcOptions pkgSpec
      , setToListattr "dependencies" $ psDependencies pkgSpec
      , setToListattr "extensions" $ psExtensions pkgSpec
      , setToListattr "extra-files" $ psExtraFiles pkgSpec
      , setToListattr "extra-directories" $ psExtraDirectories pkgSpec
      , listattr' "packages" empty . Set.toAscList $ psPackages pkgSpec
      ]
    , rbrace
    ]
   where
    listattr' :: String -> Doc -> [String] -> Doc
    listattr' n prefix vs = onlyIf (not (null vs)) $
                sep [ text n <+> equals <+> prefix <+> lbrack,
                      nest 2 $ vcat $ map text vs,
                      rbrack <> semi
                    ]
    setToListattr attrName = listattr' attrName empty . fmap show . Set.toAscList

-- | Generate a list of @PackageSpec@s, one for each component in the cabal file
fromGenericPackageDescription
  :: Platform -> CompilerInfo -> GenericPackageDescription -> [NamedPackageSpec]
fromGenericPackageDescription platform compilerInfo gPkgDesc =
  fromPackageDescription desc
 where
  desc =
    either (error . ("Missing dependencies: " <>) . show) fst $ finalizePD
      (mkFlagAssignment [])
      requestedComponents
      (const True)
      platform
      compilerInfo
      []
      gPkgDesc

  requestedComponents :: ComponentRequestedSpec
  requestedComponents = ComponentRequestedSpec False False

-- | Generate a list of @PackageSpec@s, one for each component in the cabal file
fromPackageDescription :: PackageDescription -> [NamedPackageSpec]
fromPackageDescription packageDesc =
  maybe [] ((: []) . fromLibrary packageName) (library packageDesc)
    ++ fmap (fromExecutable localDeps) (executables packageDesc)
 where
  localDeps :: Set String
  localDeps = Set.fromList $ packageName : subLibNames

  subLibNames :: [String]
  subLibNames =
    unUnqualComponentName . fromJust . libName <$> subLibraries packageDesc

  packageName :: String
  packageName = unPackageName . pkgName $ package packageDesc

fromLibrary :: String -> Library -> NamedPackageSpec
fromLibrary name lib = NamedPackageSpec
  { npsName    = name
  , npsPkgSpec = fromBuildInfo Nothing mempty $ libBuildInfo lib
  }

fromExecutable :: Set String -> Executable -> NamedPackageSpec
fromExecutable localDeps exe = NamedPackageSpec
  { npsName    = ("exe-" <>) . unUnqualComponentName $ exeName exe
  , npsPkgSpec = fromBuildInfo
    (Just . T.pack . takeBaseName $ modulePath exe)
    localDeps
    (buildInfo exe)
  }

fromBuildInfo :: Maybe Text -> Set String -> BuildInfo -> PackageSpec
fromBuildInfo main localDeps bi = PackageSpec
  { psSource           = sourceDir
  , psMain             = main
  , psGhcOptions       = maybe mempty Set.fromList $ lookup GHC $ options bi
  , psDependencies     = externalDeps
  , psExtensions       = Set.fromList $ defaultExtensions bi
  , psExtraFiles       = mempty
  , psExtraDirectories = mempty
  , psPackages         = packages
  }
 where
  sourceDir :: String
  sourceDir = prefixPath $ head $ hsSourceDirs bi

  prefixPath :: String -> String
  prefixPath path = if isAlpha (head path) then "./" <> path else path

  allDeps :: Set String
  allDeps =
    Set.fromList . fmap (unPackageName . depPkgName) $ targetBuildDepends bi

  packages :: Set String
  externalDeps :: Set String
  (packages, externalDeps) = Set.partition (`elem` localDeps) allDeps
