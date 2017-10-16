{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Vector.Storable             (Vector, fromList)
import           Graphics.GL                      (GLfloat, GLuint)
import           Linear                           (V3 (..))
import           Scene
import           Scene.GL.Attribute.VertexWithPos (VertexWithPos (..))
--import           Text.Printf (printf)

main :: IO ()
main = do
    let config = defaultConfiguration
            { globalSettings =
                [ClearColor 1 0 0 1]
            , initialScene =
                Scene
                    { sceneSettings =
                        [ Clear [ColorBufferBit]
                        ]
                    , sceneEntities = []
                    }
            }
    res <- viewScenes config onInit onEvent onExit
    print res

onInit :: Viewer -> IO ()
onInit viewer = do
    putStrLn "onInit - enter"
    progResult <- programFromFiles viewer
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniformNames = ["col"]
            }

    case progResult of
        Right program -> do
            meshResult <- meshFromRequest viewer
                MeshRequest
                    { vertices = triangleVertices
                    , indices = triangleIndices
                    , primitive = Triangles
                    }
            case meshResult of
                Right mesh -> do
                    let entity = Entity { entitySettings = []
                                        , entityProgram = program
                                        , entityMesh = mesh
                                        , entityUniforms = [ UniformValue "col" triangleColor ]
                                        }
                    setCurrentScene viewer
                        Scene { sceneSettings = [Clear [ColorBufferBit]]
                              , sceneEntities = [entity]
                              }
                Left err -> putStrLn err
        Left err      -> putStrLn err

    putStrLn "onInit - done"

onEvent :: Viewer -> Event -> () -> IO ()

onEvent viewer CloseRequest _ = do
    putStrLn "onEvent: Close now!"
    close viewer

onEvent _viewer _ _ = return ()
    -- printf "onEvent: Frame duration=%f, viewport=%s\n" duration (show viewport)

onExit :: Viewer -> () -> IO ()
onExit _viewer _ = putStrLn "onExit"

triangleVertices :: Vector VertexWithPos
triangleVertices = fromList
    [ VertexWithPos { position = V3 0 1 0 }
    , VertexWithPos { position = V3 (-1) (-1) 0 }
    , VertexWithPos { position = V3 1 (-1) 0 }
    ]

triangleIndices :: Vector GLuint
triangleIndices = fromList [0, 1, 2]

triangleColor :: V3 GLfloat
triangleColor = V3 0 0 1
