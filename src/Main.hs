{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty                           as S

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy             as ST
import           Data.List                            (sortBy)
import           Data.List.Split                      (chunksOf)
import           Safe                                 (readDef)
import           System.Directory
import           System.FilePath

import qualified Data.Text                            as T
import           Lucid


type MapHtml = HtmlT (StateT MapState IO)

type MapState = [String] -- list of map names

lucid :: MapHtml () -> ActionM ()
lucid h = do
  maps <- liftIO $ getDirectoryContents "maps/"
  let maps' = filter (not . (`elem` [".", ".."])) maps
      bst = renderBST h
  bst' <- liftIO $ evalStateT bst maps'
  S.raw bst'

main :: IO ()
main =
    scotty 4646 $
    do middleware logStdoutDev
       middleware $ staticPolicy $ hasPrefix "maps/" <|> hasPrefix "static/"
       S.get "/" $ redirect "/map/yacht"
       S.get "/map/:map" $
           do setHeader "Content-Type" "text/html"
              mapSelection <- S.param "map"
              when ('/' `elem` mapSelection) $ redirect "/"
              fs <- liftIO $ getDirectoryContents $ "maps/" </> mapSelection
              let filesWithFloor =
                      map
                          (\f ->
                                (takeBaseName f, f)) $
                      sortBy floorCmp $ filter (not . (`elem` [".", ".."])) fs
                  chunkedFiles = chunksOf 2 filesWithFloor
              lucid $
                  do template ("map viewer: " ++ mapSelection) $
                         do div_ [class_ "container-fluid"] $
                                forM_ chunkedFiles $
                                (div_ [class_ "row"]) .
                                mapM (toCol mapSelection)
  where
    floorCmp a b =
        let aName = takeBaseName a
            bName = takeBaseName b
            na = readDef 0 $ init aName :: Int
            nb = readDef 0 $ init bName :: Int
        in compare na nb
    toCol :: String -> (String, FilePath) -> MapHtml ()
    toCol mapSel (name,f) =
        div_ [class_ "col-md-6"] $
        do div_ [class_ "col-md-12"] $ h3_ $ toHtml name
           img_
               [ src_ $ T.pack $ "/maps" </> mapSel </> f
               , class_ "img-responsive"]


template :: String -> MapHtml () -> MapHtml ()
template title bod = do
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
               do bod
                  theFooter

theFooter :: MapHtml ()
theFooter =
    nav_ [class_ "navbar navbar-default navbar-fixed-bottom"] $
    do div_ [class_ "container"] $
           do div_ [class_ "footer"] $
                  do ms <- lift ST.get
                     forM_ ms $
                         \m ->
                              a_
                                  [ class_ "navbar-link"
                                  , href_ (T.pack $ "/map/" ++ m)] $
                              toHtml m

includes :: MapHtml ()
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

scriptIncl_ :: [Attribute] -> MapHtml ()
scriptIncl_ as = script_ as $ T.pack ""
