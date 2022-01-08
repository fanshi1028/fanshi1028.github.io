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
      specVersion = "2.4";
      identifier = { name = "stan"; version = "0.0.1.0"; };
      license = "MPL-2.0";
      copyright = "2020 Kowainik";
      maintainer = "Kowainik <xrom.xkov@gmail.com>";
      author = "Veronika Romashkina, Dmitrii Kovanikov";
      homepage = "https://github.com/kowainik/stan";
      url = "";
      synopsis = "Haskell STatic ANalyser";
      description = "Stan is a Haskell __ST__atic __AN__alysis CLI tool.\nSee [README.md](https://github.com/kowainik/stan#stan) for more details.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "test/.stan-example.toml" ];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGELOG.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
          (hsPkgs."colourista" or (errorHandler.buildDepError "colourista"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
          (hsPkgs."dir-traverse" or (errorHandler.buildDepError "dir-traverse"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extensions" or (errorHandler.buildDepError "extensions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
          (hsPkgs."microaeson" or (errorHandler.buildDepError "microaeson"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."slist" or (errorHandler.buildDepError "slist"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
          (hsPkgs."trial" or (errorHandler.buildDepError "trial"))
          (hsPkgs."trial-optparse-applicative" or (errorHandler.buildDepError "trial-optparse-applicative"))
          (hsPkgs."trial-tomland" or (errorHandler.buildDepError "trial-tomland"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "Paths_stan"
          "Stan"
          "Stan/Analysis"
          "Stan/Analysis/Analyser"
          "Stan/Analysis/Pretty"
          "Stan/Analysis/Summary"
          "Stan/Browse"
          "Stan/Cabal"
          "Stan/Category"
          "Stan/Cli"
          "Stan/Config"
          "Stan/Config/Pretty"
          "Stan/Core/Id"
          "Stan/Core/List"
          "Stan/Core/ModuleName"
          "Stan/Example"
          "Stan/Ghc/Compat"
          "Stan/EnvVars"
          "Stan/FileInfo"
          "Stan/Hie"
          "Stan/Hie/Compat"
          "Stan/Hie/Debug"
          "Stan/Hie/MatchAst"
          "Stan/Hie/MatchType"
          "Stan/Info"
          "Stan/Inspection"
          "Stan/Inspection/All"
          "Stan/Inspection/AntiPattern"
          "Stan/Inspection/Infinite"
          "Stan/Inspection/Partial"
          "Stan/Inspection/Style"
          "Stan/NameMeta"
          "Stan/Observation"
          "Stan/Pattern/Ast"
          "Stan/Pattern/Edsl"
          "Stan/Pattern/Type"
          "Stan/Report"
          "Stan/Report/Css"
          "Stan/Report/Html"
          "Stan/Report/Settings"
          "Stan/Severity"
          "Stan/Toml"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "target" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [
            "Target/AntiPattern"
            "Target/AntiPattern/Stan0206"
            "Target/AntiPattern/Stan0206Extensions"
            "Target/AntiPattern/Stan0212"
            "Target/AntiPattern/Stan0213"
            "Target/AntiPattern/Stan0214"
            "Target/Infinite"
            "Target/Partial"
            "Target/Style"
            ];
          hsSourceDirs = [ "target" ];
          };
        };
      exes = {
        "stan" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."stan" or (errorHandler.buildDepError "stan"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = (((([
            "Main.hs"
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.0") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.2") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8") "") ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.10") "";
          };
        };
      tests = {
        "stan-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."stan" or (errorHandler.buildDepError "stan"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tomland" or (errorHandler.buildDepError "tomland"))
            (hsPkgs."trial" or (errorHandler.buildDepError "trial"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          modules = [
            "Test/Stan/Analysis"
            "Test/Stan/Analysis/AntiPattern"
            "Test/Stan/Analysis/Common"
            "Test/Stan/Analysis/Infinite"
            "Test/Stan/Analysis/Partial"
            "Test/Stan/Analysis/Style"
            "Test/Stan/Cli"
            "Test/Stan/Config"
            "Test/Stan/Gen"
            "Test/Stan/Number"
            "Test/Stan/Observation"
            "Test/Stan/Toml"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }