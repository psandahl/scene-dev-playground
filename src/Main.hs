module Main where

--import           Control.Concurrent.Async
import           Graphics.Scene

main :: IO ()
main =
    either putStrLn runViewer =<<
        createViewer "Dev Playground" defaultConfiguration [ ClearColor 1 0 0 0 ]

runViewer :: Viewer -> IO ()
runViewer viewer = do
    res <- loadShaderProgram viewer $
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniforms = []
            }
    print res
    waitOnViewerClose viewer
