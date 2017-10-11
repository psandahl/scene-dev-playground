{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Scene
--import           Text.Printf (printf)

main :: IO ()
main = do
    let config = defaultConfiguration { globalActions =
                                            [ClearColor 1 0 0 1]
                                      , initialScene =
                                          Scene { actions =
                                                    [ Clear [ColorBufferBit]
                                                    , Enable DepthTest
                                                    ]
                                                }
                                      }
    res <- viewScenes config onInit onEvent onExit
    print res

onInit :: Viewer -> IO ()
onInit viewer = do
    putStrLn "onInit - enter"
    result <- programFromFiles viewer
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniformNames = ["col"]
            }
    print result
    putStrLn "onInit - done"

onEvent :: Viewer -> Event -> () -> IO ()

onEvent viewer CloseRequest _ = do
    putStrLn "onEvent: Close now!"
    close viewer

onEvent _viewer _ _ = return ()
    -- printf "onEvent: Frame duration=%f, viewport=%s\n" duration (show viewport)

onExit :: Viewer -> () -> IO ()
onExit _viewer _ = putStrLn "onExit"
