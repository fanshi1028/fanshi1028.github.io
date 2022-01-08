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
    flags = {
      previewserver = true;
      watchserver = true;
      checkexternal = true;
      buildwebsite = false;
      usepandoc = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "hakyll"; version = "4.15.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jasper Van der Jeugt <m@jaspervdj.be>";
      author = "Jasper Van der Jeugt <m@jaspervdj.be>";
      homepage = "http://jaspervdj.be/hakyll";
      url = "";
      synopsis = "A static website compiler library";
      description = "Hakyll is a static website compiler library. It provides you with the tools to\ncreate a simple or advanced static website using a Haskell DSL and formats\nsuch as markdown or RST. You can find more information, including a tutorial,\non the website:\n\n* <http://jaspervdj.be/hakyll>\n\nIf you seek assistance, there's:\n\n* A google group: <http://groups.google.com/group/hakyll>\n\n* An IRC channel, @#hakyll@ on irc.libera.chat (we *do not* have a channel on Freenode anymore)\n\nAdditionally, there's the Haddock documentation in the different modules,\nmeant as a reference.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "data";
      dataFiles = [
        "example/posts/2015-11-28-carpe-diem.markdown"
        "example/posts/2015-10-07-rosa-rosa-rosam.markdown"
        "example/posts/2015-12-07-tu-quoque.markdown"
        "example/posts/2015-08-12-spqr.markdown"
        "example/site.hs"
        "example/images/haskell-logo.png"
        "example/templates/post-list.html"
        "example/templates/default.html"
        "example/templates/archive.html"
        "example/templates/post.html"
        "example/css/default.css"
        "example/index.html"
        "example/about.rst"
        "example/contact.markdown"
        ];
      extraSrcFiles = [
        "CHANGELOG.md"
        "tests/data/biblio/chicago.csl"
        "tests/data/biblio/cites-meijer.golden"
        "tests/data/biblio/cites-multiple.golden"
        "tests/data/biblio/cites-multiple.markdown"
        "tests/data/biblio/default.html"
        "tests/data/biblio/page.markdown"
        "tests/data/biblio/refs.bib"
        "tests/data/biblio/refs.yaml"
        "tests/data/biblio/refs2.yaml"
        "tests/data/embed.html"
        "tests/data/example.md"
        "tests/data/example.md.metadata"
        "tests/data/images/favicon.ico"
        "tests/data/just-meta.html"
        "tests/data/just-meta.html.out"
        "tests/data/partial-helper.html"
        "tests/data/partial.html"
        "tests/data/partial.html.out"
        "tests/data/posts/2010-08-26-birthday.md"
        "tests/data/posts/2018-09-26.md"
        "tests/data/posts/2019/05/10/tomorrow.md"
        "tests/data/russian.md"
        "tests/data/strip.html"
        "tests/data/strip.html.out"
        "tests/data/template-empty.html"
        "tests/data/template.html"
        "tests/data/template.html.out"
        "data/templates/atom-item.xml"
        "data/templates/atom.xml"
        "data/templates/rss-item.xml"
        "data/templates/rss.xml"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."blaze-html" or (errorHandler.buildDepError "blaze-html"))
          (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."lrucache" or (errorHandler.buildDepError "lrucache"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-locale-compat" or (errorHandler.buildDepError "time-locale-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ] ++ (pkgs.lib).optionals (flags.previewserver) [
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
          ]) ++ (pkgs.lib).optional (flags.watchserver) (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))) ++ (pkgs.lib).optionals (flags.checkexternal) [
          (hsPkgs."http-conduit" or (errorHandler.buildDepError "http-conduit"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          ]) ++ (pkgs.lib).optional (flags.usepandoc) (hsPkgs."pandoc" or (errorHandler.buildDepError "pandoc"));
        buildable = true;
        modules = (([
          "Data/List/Extended"
          "Data/Yaml/Extended"
          "Hakyll/Check"
          "Hakyll/Core/Compiler/Require"
          "Hakyll/Core/Identifier/Pattern/Internal"
          "Hakyll/Core/Item/SomeItem"
          "Hakyll/Core/Provider/Internal"
          "Hakyll/Core/Provider/MetadataCache"
          "Hakyll/Core/Util/Parser"
          "Paths_hakyll"
          "Hakyll"
          "Hakyll/Commands"
          "Hakyll/Core/Compiler"
          "Hakyll/Core/Compiler/Internal"
          "Hakyll/Core/Configuration"
          "Hakyll/Core/Dependencies"
          "Hakyll/Core/File"
          "Hakyll/Core/Identifier"
          "Hakyll/Core/Identifier/Pattern"
          "Hakyll/Core/Item"
          "Hakyll/Core/Logger"
          "Hakyll/Core/Metadata"
          "Hakyll/Core/Provider"
          "Hakyll/Core/Provider/Metadata"
          "Hakyll/Core/Routes"
          "Hakyll/Core/Rules"
          "Hakyll/Core/Rules/Internal"
          "Hakyll/Core/Runtime"
          "Hakyll/Core/Store"
          "Hakyll/Core/UnixFilter"
          "Hakyll/Core/Util/File"
          "Hakyll/Core/Util/String"
          "Hakyll/Core/Writable"
          "Hakyll/Main"
          "Hakyll/Web/CompressCss"
          "Hakyll/Web/Feed"
          "Hakyll/Web/Html"
          "Hakyll/Web/Html/RelativizeUrls"
          "Hakyll/Web/Meta/JSONLD"
          "Hakyll/Web/Meta/OpenGraph"
          "Hakyll/Web/Meta/TwitterCard"
          "Hakyll/Web/Paginate"
          "Hakyll/Web/Redirect"
          "Hakyll/Web/Tags"
          "Hakyll/Web/Template"
          "Hakyll/Web/Template/Context"
          "Hakyll/Web/Template/Internal"
          "Hakyll/Web/Template/Internal/Element"
          "Hakyll/Web/Template/Internal/Trim"
          "Hakyll/Web/Template/List"
          ] ++ (pkgs.lib).optionals (flags.previewserver) [
          "Hakyll/Preview/Poll"
          "Hakyll/Preview/Server"
          ]) ++ (pkgs.lib).optional (flags.watchserver) "Hakyll/Preview/Poll") ++ (pkgs.lib).optionals (flags.usepandoc) [
          "Hakyll/Web/Pandoc/Binary"
          "Hakyll/Web/Pandoc"
          "Hakyll/Web/Pandoc/Biblio"
          "Hakyll/Web/Pandoc/FileType"
          ];
        hsSourceDirs = [ "lib" ];
        };
      exes = {
        "hakyll-init" = {
          depends = [
            (hsPkgs."hakyll" or (errorHandler.buildDepError "hakyll"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          modules = [ "Paths_hakyll" ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Init.hs" ];
          };
        "hakyll-website" = {
          depends = [
            (hsPkgs."hakyll" or (errorHandler.buildDepError "hakyll"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."pandoc" or (errorHandler.buildDepError "pandoc"))
            ];
          buildable = if flags.buildwebsite then true else false;
          hsSourceDirs = [ "web" ];
          mainPath = [ "site.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "hakyll-tests" = {
          depends = [
            (hsPkgs."hakyll" or (errorHandler.buildDepError "hakyll"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."tagsoup" or (errorHandler.buildDepError "tagsoup"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          modules = [
            "Hakyll/Core/Dependencies/Tests"
            "Hakyll/Core/Identifier/Tests"
            "Hakyll/Core/Provider/Metadata/Tests"
            "Hakyll/Core/Provider/Tests"
            "Hakyll/Core/Routes/Tests"
            "Hakyll/Core/Rules/Tests"
            "Hakyll/Core/Runtime/Tests"
            "Hakyll/Core/Store/Tests"
            "Hakyll/Core/UnixFilter/Tests"
            "Hakyll/Core/Util/String/Tests"
            "Hakyll/Web/CompressCss/Tests"
            "Hakyll/Web/Html/RelativizeUrls/Tests"
            "Hakyll/Web/Html/Tests"
            "Hakyll/Web/Tags/Tests"
            "Hakyll/Web/Template/Context/Tests"
            "Hakyll/Web/Template/Tests"
            "TestSuite/Util"
            ] ++ (pkgs.lib).optionals (flags.usepandoc) [
            "Hakyll/Web/Pandoc/Biblio/Tests"
            "Hakyll/Web/Pandoc/FileType/Tests"
            ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "TestSuite.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }