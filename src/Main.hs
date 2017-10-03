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
                     [ ClearColor 1 0 0 0 ]
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
    print shaderRes

    meshRes <- loadMesh viewer $
        MeshRequest
            { vertices = triangleVertices
            , primitive = Triangles
            , indices = triangleIndices
            }
    print meshRes

    waitOnViewerClose viewer

triangleVertices :: Vector VertexWithPos
triangleVertices =
    fromList
        [ VertexWithPos { position = V3 0 1 0}
        , VertexWithPos { position = V3 (-1) (-1) 0}
        , VertexWithPos { position = V3 1 (-1) 0}
        ]

triangleIndices :: Vector GLuint
triangleIndices = fromList [0, 1, 2]
