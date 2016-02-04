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

import qualified Data.Text                            as T
import           Lucid
import           Lucid.Base

lucid = S.raw . renderBS

main :: IO ()
main =
    scotty 4646 $
    do middleware logStdoutDev
       middleware $ staticPolicy $ hasPrefix "maps/" <|> hasPrefix "static/"
       get "/map/:map" $
           do setHeader "Content-Type" "text/html"
              mapSelection <- S.param "map"
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
                  filesWithFloor =
                      map
                          (\f ->
                                (takeBaseName f, f))
                          files''
              maps <- liftIO $ getDirectoryContents "maps/"
              let maps' =
                      filter
                          (\m ->
                                m /= "." && m /= "..")
                          maps
              lucid $
                  template maps' ("map viewer: " ++ mapSelection) $
                  do div_ [class_ "container-fluid"] $
                         do let chunkedFiles = chunksOf 2 filesWithFloor
                                toCol :: (String, FilePath) -> Html ()
                                toCol (name,f) =
                                    div_ [class_ "col-md-6"] $
                                    do div_ [class_ "col-md-12"] $
                                           h3_ $ toHtml name
                                       img_
                                           [ src_ $
                                             T.pack $
                                             "/maps" </> mapSelection </> f
                                           , class_ "img-responsive"]
                            forM_ chunkedFiles $
                                (div_ [class_ "row"]) . mapM toCol


template :: [String] -> String -> Html () -> Html ()
template ms title body = do
    doctype_
    html_ [lang_ "en"] $
        do head_ $
               do meta_ [charset_ "utf-8"]
                  meta_
                      [ name_ "viewport"
                      , content_ "width=device-width, initial-scale=1"]
                  title_ $ toHtml title
                  includes
           body_ $
               do body
                  theFooter ms

theFooter :: [String] -> Html ()
theFooter ms =
    nav_ [class_ "navbar navbar-default navbar-fixed-bottom"] $
    do div_ [class_ "container"] $
           do div_ [class_ "footer"] $
                  do forM_ ms $
                         \m ->
                              a_
                                  [ class_ "navbar-link"
                                  , href_ (T.pack $ "/map/" ++ m)] $
                              toHtml m

includes :: Html ()
includes = do
    link_
        [ rel_ "stylesheet"
        , href_
              "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"]
    scriptIncl_
        [ src_
              "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"]

    scriptIncl_
        [ src_
              "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"]
    link_
        [ href_ "/static/css/default.css"
        , title_ "compact"
        , rel_ "stylesheet"
        , type_ "text/css"]

scriptIncl_ :: [Attribute] -> Html ()
scriptIncl_ as = script_ as $ T.pack ""
