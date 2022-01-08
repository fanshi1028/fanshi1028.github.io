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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-fmt"; version = "0.1.5.1"; };
      license = "GPL-3.0-or-later AND BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "";
      url = "";
      synopsis = "Format .cabal files";
      description = "Format @.cabal@ files preserving the original field ordering, and comments.\n\nTuned for Oleg's preference, but has some knobs still.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "Changelog.md" "fixtures/*.cabal" "fixtures/*.format" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      sublibs = {
        "cabal-fmt-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            ];
          buildable = true;
          modules = [
            "CabalFmt"
            "CabalFmt/Comments"
            "CabalFmt/Error"
            "CabalFmt/Fields"
            "CabalFmt/Fields/BuildDepends"
            "CabalFmt/Fields/Extensions"
            "CabalFmt/Fields/Modules"
            "CabalFmt/Fields/SourceFiles"
            "CabalFmt/Fields/TestedWith"
            "CabalFmt/Glob"
            "CabalFmt/Monad"
            "CabalFmt/Options"
            "CabalFmt/Parser"
            "CabalFmt/Pragma"
            "CabalFmt/Prelude"
            "CabalFmt/Refactoring"
            "CabalFmt/Refactoring/ExpandExposedModules"
            "CabalFmt/Refactoring/Fragments"
            "CabalFmt/Refactoring/GlobFiles"
            "CabalFmt/Refactoring/Type"
            ];
          hsSourceDirs = [ "src" ];
          };
        };
      exes = {
        "cabal-fmt" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cabal-fmt".components.sublibs.cabal-fmt-internal or (errorHandler.buildDepError "cabal-fmt:cabal-fmt-internal"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          modules = [ "Paths_cabal_fmt" ];
          hsSourceDirs = [ "cli" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "golden" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."cabal-fmt".components.sublibs.cabal-fmt-internal or (errorHandler.buildDepError "cabal-fmt:cabal-fmt-internal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests" ];
          mainPath = [ "Golden.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }