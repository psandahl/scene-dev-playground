module Main where

import           Scene

main :: IO ()
main = do
    res <- viewScenes defaultConfiguration onInit onEvent onExit
    print res

onInit :: Viewer -> IO ()
onInit _viewer = putStrLn "onInit"

onEvent :: Viewer -> Event -> () -> IO ()

onEvent viewer CloseRequest _ = do
    putStrLn "Close now!"
    close viewer

--onEvent _viewer event _ =
--    putStrLn $ "onEvent: " ++ show event

onExit :: Viewer -> () -> IO ()
onExit _viewer _ = putStrLn "onExit"
