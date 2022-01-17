import Hakyll
  ( Command (Rebuild),
    Configuration (destinationDirectory),
    Context,
    Options (Options),
    applyAsTemplate,
    compile,
    complement,
    compressCss,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultConfiguration,
    defaultContext,
    fromList,
    getResourceBody,
    hakyllWithArgs,
    idRoute,
    listField,
    loadAll,
    loadAndApplyTemplate,
    makeItem,
    match,
    pandocCompiler,
    recentFirst,
    relativizeUrls,
    route,
    setExtension,
    templateBodyCompiler,
    (.&&.),
  )
import Text.Pandoc.Highlighting (pygments, styleToCss)

main :: IO ()
main = hakyllWithArgs
  defaultConfiguration {destinationDirectory = "docs"}
  (Options True Rebuild)
  $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ makeItem $ compressCss $ styleToCss pygments
    -- compile $ makeItem $ compressCss $ styleToCss espresso
    -- compile $ makeItem $ compressCss $ styleToCss zenburn

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match ("posts/*" .&&. complement "posts/shell.nix") $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    let postsListCtx = listField "posts" postCtx $ recentFirst =<< loadAll "posts/*"

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        let archiveCtx = postsListCtx <> constField "title" "Archives" <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = postsListCtx <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext
