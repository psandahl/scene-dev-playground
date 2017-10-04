module Main where

--import           Control.Concurrent.Async
import           Data.Vector.Storable (Vector, fromList)
import           Graphics.GL          (GLuint)
import           Graphics.Scene
import           Linear               (V3 (..))

main :: IO ()
main =
    either putStrLn runViewer =<<
        createViewer "Dev Playground"
                     defaultConfiguration
                     [ ClearColor 0 0 1 0 ]
                     (Scene [Clear [ColorBufferBit]] [])

runViewer :: Viewer -> IO ()
runViewer viewer = do
    shaderRes <- loadShaderProgram viewer $
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniforms = []
            }

    meshRes <- loadMesh viewer $
        MeshRequest
            { vertices = triangleVertices
            , primitive = Triangles
            , indices = triangleIndices
            }

    case shaderRes of
        Right shader ->

            case meshRes of
                Right mesh' -> do
                    let entity = Entity [] shader mesh'
                    setScene viewer $ Scene [Clear [ColorBufferBit]] [entity]

                Left err -> logStrLn viewer err

        Left err -> logStrLn viewer err

    waitOnViewerClose viewer

triangleVertices :: Vector VertexWithPos
triangleVertices =
    fromList
        [ VertexWithPos { position = V3 0 0.5 0}
        , VertexWithPos { position = V3 (-0.5) (-0.5) 0}
        , VertexWithPos { position = V3 0.5 (-0.5) 0}
        ]

triangleIndices :: Vector GLuint
triangleIndices = fromList [0, 1, 2]
