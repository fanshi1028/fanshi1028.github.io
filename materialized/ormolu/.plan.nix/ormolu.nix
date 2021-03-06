{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { dev = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "ormolu"; version = "0.4.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Mark Karpov <mark.karpov@tweag.io>";
      author = "";
      homepage = "https://github.com/tweag/ormolu";
      url = "";
      synopsis = "A formatter for Haskell source code";
      description = "A formatter for Haskell source code.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE.md" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "data/**/*.hs" "data/**/*.txt" ];
      extraTmpFiles = [];
      extraDocFiles = [
        "CONTRIBUTING.md"
        "CHANGELOG.md"
        "DESIGN.md"
        "README.md"
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Diff" or (errorHandler.buildDepError "Diff"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          ];
        buildable = true;
        modules = [
          "GHC/DynFlags"
          "Ormolu"
          "Ormolu/Config"
          "Ormolu/Diff/ParseResult"
          "Ormolu/Diff/Text"
          "Ormolu/Exception"
          "Ormolu/Imports"
          "Ormolu/Parser"
          "Ormolu/Parser/CommentStream"
          "Ormolu/Parser/Pragma"
          "Ormolu/Parser/Result"
          "Ormolu/Printer"
          "Ormolu/Printer/Combinators"
          "Ormolu/Printer/Comments"
          "Ormolu/Printer/Internal"
          "Ormolu/Printer/Meat/Common"
          "Ormolu/Printer/Meat/Declaration"
          "Ormolu/Printer/Meat/Declaration/Annotation"
          "Ormolu/Printer/Meat/Declaration/Class"
          "Ormolu/Printer/Meat/Declaration/Data"
          "Ormolu/Printer/Meat/Declaration/Default"
          "Ormolu/Printer/Meat/Declaration/Foreign"
          "Ormolu/Printer/Meat/Declaration/Instance"
          "Ormolu/Printer/Meat/Declaration/RoleAnnotation"
          "Ormolu/Printer/Meat/Declaration/Rule"
          "Ormolu/Printer/Meat/Declaration/Signature"
          "Ormolu/Printer/Meat/Declaration/Splice"
          "Ormolu/Printer/Meat/Declaration/Type"
          "Ormolu/Printer/Meat/Declaration/TypeFamily"
          "Ormolu/Printer/Meat/Declaration/Value"
          "Ormolu/Printer/Meat/Declaration/Warning"
          "Ormolu/Printer/Meat/ImportExport"
          "Ormolu/Printer/Meat/Module"
          "Ormolu/Printer/Meat/Pragma"
          "Ormolu/Printer/Meat/Type"
          "Ormolu/Printer/Operators"
          "Ormolu/Printer/SpanStream"
          "Ormolu/Processing/Common"
          "Ormolu/Processing/Cpp"
          "Ormolu/Processing/Preprocess"
          "Ormolu/Terminal"
          "Ormolu/Utils"
          "Ormolu/Utils/Extensions"
          "Ormolu/Utils/IO"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "ormolu" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
            (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ormolu" or (errorHandler.buildDepError "ormolu"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "Paths_ormolu" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."ormolu" or (errorHandler.buildDepError "ormolu"))
            (hsPkgs."path" or (errorHandler.buildDepError "path"))
            (hsPkgs."path-io" or (errorHandler.buildDepError "path-io"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Ormolu/CabalExtensionsSpec"
            "Ormolu/Diff/TextSpec"
            "Ormolu/Parser/OptionsSpec"
            "Ormolu/Parser/ParseFailureSpec"
            "Ormolu/Parser/PragmaSpec"
            "Ormolu/PrinterSpec"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }