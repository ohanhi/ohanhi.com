--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           System.FilePath (splitExtension)
import           Data.Monoid (mappend)
import           Control.Applicative (Alternative (..))
import qualified Control.Monad as Monad
import           Data.Maybe (fromMaybe)
import           Network.HTTP.Base (urlEncode)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route   $ customRoute $ pathToPostName . toFilePath
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*" >>= recentFirst
            let indexCtx =
                  listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler


(|>) = flip ($)


{- This function keeps the same filename structure I've had with Jekyll:
/posts/YYYY-MM-DD-post-name.md
-> /post-name.html
-}
pathToPostName :: String -> String
pathToPostName path =
    path
        |> splitAll "/"
        |> (\(p : name : _) -> splitExtension name )
        |> (\(n, e) -> drop 11 n ++ ".html")

--------------------------------------------------------------------------------


postCtx :: Context String
postCtx =
    dateField "date" "%B %-d, %Y"
      `mappend` constField "base_url" "https://ohanhi.com"
      `mappend` twitterFields "https://ohanhi.com"
      `mappend` listContextWith "i_expect_you_to_know"
      `mappend` listContextWith "read_this_to"
      `mappend` defaultContext

twitterFields :: String -> Context a
twitterFields baseUrl =
    twitterUrlField baseUrl
      `mappend` twitterTextField

twitterUrlField :: String -> Context a
twitterUrlField baseUrl = field "twitter_url" $
      fmap (maybe empty (\a -> urlEncode $ baseUrl ++ toUrl a)) . getRoute . itemIdentifier

twitterTextField :: Context a
twitterTextField = field "twitter_text" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    let find s = lookupString s metadata
    (find "series", find "title", find "subtitle")
        |> (\(series, title, subtitle) ->
            fromMaybe "" (fmap (\s -> s ++ ": ") series)
              ++ fromMaybe "No title" title
              ++ fromMaybe "" (fmap ((++) " - ") subtitle)
           )
        |> urlEncode
        |> return

listContextWith :: String -> Context a
listContextWith s =
    getList s
        |> listField s defaultContext


getList :: String -> Compiler [Item String]
getList s = do
    identifier <- getUnderlying
    metadata <- getMetadata identifier
    metadata
        |> lookupStringList s
        |> fromMaybe []
        |> map toItem
        |> return


toItem x =
    Item (fromFilePath x) x
