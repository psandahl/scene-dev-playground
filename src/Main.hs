module Main where

import           Graphics.Scene

main :: IO ()
main = do
    eViewver <- createViewer "Dev Playground" defaultConfiguration [ ClearColor 1 0 0 0 ]
    case eViewver of
        Right viewer -> waitOnViewerClose viewer
        Left err     -> putStrLn err
