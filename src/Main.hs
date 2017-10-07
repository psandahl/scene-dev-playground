module Main where

import           Scene
import           Text.Printf (printf)

main :: IO ()
main = do
    res <- viewScenes defaultConfiguration onInit onEvent onExit
    print res

onInit :: Viewer -> IO ()
onInit _viewer = putStrLn "onInit"

onEvent :: Viewer -> Event -> () -> IO ()

onEvent viewer CloseRequest _ = do
    putStrLn "onEvent: Close now!"
    close viewer

onEvent _viewer (Frame duration viewport) _ =
    printf "onEvent: Frame duration=%f, viewport=%s\n" duration (show viewport)

onExit :: Viewer -> () -> IO ()
onExit _viewer _ = putStrLn "onExit"
