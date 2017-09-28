module Main where

import           Graphics.Scene

main :: IO ()
main =
    either putStrLn runViewer =<<
        createViewer "Dev Playground" defaultConfiguration [ ClearColor 1 0 0 0 ]

runViewer :: Viewer -> IO ()
runViewer = waitOnViewerClose
