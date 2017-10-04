{-# LANGUAGE OverloadedStrings #-}
module Main where

--import           Control.Concurrent.Async
import           Control.Monad        (void)
import           Data.Vector.Storable (Vector, fromList)
import           Graphics.GL          (GLfloat, GLuint)
import           Graphics.Scene
import           Linear               (V3 (..))

main :: IO ()
main = void $ viewScenes defaultConfiguration viewApp

viewApp :: Viewer -> IO ()
viewApp viewer = do
    shaderRes <- loadShaderProgram viewer $
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniformNames = ["col"]
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
                    let entity = Entity [] shader [UniformValue "col" triangleColor] mesh'
                    setScene viewer $ Scene [Clear [ColorBufferBit, DepthBufferBit]] [entity]

                Left err -> logStrLn viewer err

        Left err -> logStrLn viewer err

triangleVertices :: Vector VertexWithPos
triangleVertices =
    fromList
        [ VertexWithPos { position = V3 0 1 0}
        , VertexWithPos { position = V3 (-1) (-1) 0}
        , VertexWithPos { position = V3 1 (-1) 0}
        ]

triangleIndices :: Vector GLuint
triangleIndices = fromList [0, 1, 2]

triangleColor :: V3 GLfloat
triangleColor = V3 1 1 0
