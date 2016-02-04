{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty                           as S

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                            (sortBy)
import           Data.List.Split                      (chunksOf)
import           Safe                                 (readDef)
import           System.Directory
import           System.FilePath

import           Text.Blaze.Html.Renderer.Text        (renderHtml)
import           Text.Blaze.Html5                     hiding (main, map)
import qualified Text.Blaze.Html5                     as H
import           Text.Blaze.Html5.Attributes          hiding (param)
import qualified Text.Blaze.Html5.Attributes          as A

blaze = S.html . renderHtml

main :: IO ()
main =
    scotty 4646 $
    do middleware logStdoutDev
       middleware $ staticPolicy $ hasPrefix "maps/" <|> hasPrefix "static/"
       get "/map/:map" $
           do mapSelection <- S.param "map"
              when ('/' `elem` mapSelection) next
              files <- liftIO $ getDirectoryContents $ "maps/" </> mapSelection
              let files' =
                      filter
                          (\f ->
                                f /= "." && f /= "..")
                          files
                  files'' =
                      sortBy
                          (\a b ->
                                let aName = takeBaseName a
                                    bName = takeBaseName b
                                    na = readDef 0 $ init aName :: Int
                                    nb = readDef 0 $ init bName :: Int
                                in compare na nb)
                          files'
              maps <- liftIO $ getDirectoryContents "maps/"
              let maps' =
                      filter
                          (\m ->
                                m /= "." && m /= "..")
                          maps
              blaze $
                  template maps' ("map viewer: " ++ mapSelection) $
                  do H.div ! class_ "container-fluid" $
                         do let chunkedFiles = chunksOf 2 files''
                                toCol :: FilePath -> Html
                                toCol f =
                                    H.div ! class_ "col-md-6" $
                                    do H.img !
                                           A.src
                                               (toValue $
                                                "/maps" </> mapSelection </> f)
                            forM_ chunkedFiles $
                                (H.div ! class_ "row") . mapM_ toCol


template :: [String] -> String -> Html -> Html
template ms title body = do
    docType
    H.html ! lang "en" $
        do H.head $
               do meta ! charset "utf-8"
                  meta ! name "viewport" !
                      content
                          "width=device-width, initial-scale=1"
                  H.title $ toHtml title
                  includes
           H.body $
               do body
                  theFooter ms

theFooter :: [String] -> Html
theFooter ms =
    nav ! class_ "navbar navbar-default navbar-fixed-bottom" $
    do H.div ! class_ "container" $
           do H.div ! class_ "footer" $
                  do do forM_ ms $
                            \m ->
                                 a ! class_ "navbar-link" !
                                 href (toValue $ "/map/" ++ m) $
                                 toHtml m

includes :: Html
includes = do
    link ! rel "stylesheet" ! -- bootstrap
        href
            "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    script mempty !
        src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
    script mempty !
        src
            "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
    H.link ! A.href "/static/css/default.css" ! A.title "compact" !
        A.rel "stylesheet" !
        A.type_ "text/css"
