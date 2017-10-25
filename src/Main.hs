{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                    (void)
import           Data.Vector.Storable             (Vector, fromList)
import           Flow                             ((<|))
import           Graphics.GL                      (GLfloat, GLuint)
import           Linear                           (M44, V3 (..), V4 (..), (!*!))
import           Scene
import           Scene.GL.Attribute.VertexWithPos (VertexWithPos (..))
import           Scene.Math
import qualified Scene.Math                       as Math
--import           Text.Printf (printf)

data App = App
    { program      :: !Program
    , triangleMesh :: !Mesh
    , squareMesh   :: !Mesh
    , trianglesAt  :: ![Angle GLfloat]
    }

main :: IO ()
main = do
    let globalSettings' = [ SetClearColor 0 0 0 0
                          , Enable DepthTest
                          , SetDepthFunc Less
                          ]
        initialScene' =
            Scene { sceneSettings = [ Clear [ColorBufferBit, DepthBufferBit] ]
                  , sceneEntities = []
                  }
        config = defaultConfiguration { globalSettings = globalSettings'
                                      , initialScene = initialScene'
                                      }
    void $ viewScenes config appInit appEvent appExit

appInit :: Viewer -> IO (Maybe App)
appInit viewer = do
    putStrLn "appInit"

    -- Load program
    progResult <- programFromFiles viewer
        ProgramRequest
            { shaders = [ (Vertex, "resources/vertex.glsl")
                        , (Fragment, "resources/fragment.glsl")
                        ]
            , uniformNames = ["col", "mvp"]
            }

    triMeshResult <- meshFromRequest viewer
        MeshRequest
            { vertices = triangleVertices
            , indices = triangleIndices
            , primitive = Triangles
            }

    squareMeshResult <- meshFromRequest viewer
        MeshRequest
            { vertices = squareVertices
            , indices = squareIndices
            , primitive = Triangles
            }

    case (progResult, triMeshResult, squareMeshResult) of
        (Right program', Right triangleMesh', Right squareMesh') ->
            return $
                Just App { program = program'
                         , triangleMesh = triangleMesh'
                         , squareMesh = squareMesh'
                         , trianglesAt = [ Degrees 0, Degrees 90, Degrees 180, Degrees 270 ]
                         }

        _ -> return Nothing

appEvent :: Viewer -> Event -> Maybe App -> IO (Maybe App)

-- | Catch the frame event and render stuff.
appEvent viewer (Frame duration viewport) (Just app) = do
    let perspectiveMatrix =
            mkPerspectiveMatrix (Degrees 45) (toAspectRatio viewport) 0.1 1000 :: M44 GLfloat

        viewMatrix = mkViewMatrix (V3 0 0 50) origo3d up3d

        trianglesAt' = map (rotateTriangle duration) <| trianglesAt app

        triangles = map (renderTriangle (program app) (triangleMesh app)
                         perspectiveMatrix viewMatrix) trianglesAt'

        square = renderSquare (program app) (squareMesh app)
                               perspectiveMatrix viewMatrix

    setScene viewer
        Scene
            { sceneSettings = [Clear [ColorBufferBit, DepthBufferBit]]
            , sceneEntities = triangles ++ [square]
            }

    return $ Just <| app { trianglesAt = trianglesAt' }

-- Catch the case where we have no app ...
appEvent viewer _ Nothing = do
    putStrLn "appInit reported Nothing. Bye!"
    close viewer
    return Nothing

-- Catch the case where close is requested from the window.
appEvent viewer CloseRequest app = do
    putStrLn "Close requested. Bye!"
    close viewer
    return app

appExit :: Viewer -> Maybe App -> IO ()
appExit _ _ = putStrLn "appExit"

renderSquare :: Program -> Mesh -> M44 GLfloat -> M44 GLfloat -> Entity
renderSquare sqProgram sqMesh perspective view =
    let modelMatrix = mkScalingMatrix (V3 8 8 8)
    in Entity
         { entitySettings = [ Enable Blend
                            , SetBlendEquationSeparate FuncAdd FuncAdd
                            , SetBlendFuncSeparate SrcAlpha OneMinusSrcAlpha One Zero
                            ]
         , entityProgram = sqProgram
         , entityMesh = sqMesh
         , entityUniforms =
             [ UniformValue "col" squareColor
             , UniformValue "mvp" <| mvpMatrix modelMatrix view perspective
             ]
         , entityTextures = []
         }

renderTriangle :: Program -> Mesh -> M44 GLfloat
               -> M44 GLfloat -> Angle GLfloat -> Entity
renderTriangle triProgram triMesh perspective view angle =
    -- Rotate around the y axis when transformed.
    let modelMatrix =
            mkRotationMatrix y3d angle !*!
                mkTranslationMatrix (V3 0 0 triangleRotationRadius)
    in Entity
        { entitySettings = []
        , entityProgram = triProgram
        , entityMesh = triMesh
        , entityUniforms =
            [ UniformValue "col" triangleColor
            , UniformValue "mvp" <| mvpMatrix modelMatrix view perspective
            ]
        , entityTextures = []
        }

rotateTriangle :: Double -> Angle GLfloat -> Angle GLfloat
rotateTriangle duration angle =
    maybe angle id <| angle `angleAdd` frameRotation duration

frameRotation :: Double -> Angle GLfloat
frameRotation duration =
    Degrees <| realToFrac duration * 45

triangleRotationRadius :: GLfloat
triangleRotationRadius = 10

triangleVertices :: Vector VertexWithPos
triangleVertices = fromList
    [ VertexWithPos { position = V3 0 0.5 0 }
    , VertexWithPos { position = V3 (-0.5) (-0.5) 0 }
    , VertexWithPos { position = V3 0.5 (-0.5) 0 }
    ]

triangleIndices :: Vector GLuint
triangleIndices = fromList [0, 1, 2]

triangleColor :: V4 GLfloat
triangleColor = V4 1 (69 / 255) 0 1

squareVertices :: Vector VertexWithPos
squareVertices = fromList
    [ VertexWithPos { position = V3 (-0.5) 0.5 0 }
    , VertexWithPos { position = V3 0.5 0.5 0 }
    , VertexWithPos { position = V3 (-0.5) (-0.5) 0 }
    , VertexWithPos { position = V3 0.5 (-0.5) 0 }
    ]

squareIndices :: Vector GLuint
squareIndices = fromList [1, 0, 2, 1, 2, 3]

squareColor :: V4 GLfloat
squareColor = V4 0 0 (139 / 255) 0.5

toAspectRatio :: Viewport -> AspectRatio
toAspectRatio viewport =
    AspectRatio { Math.width = Scene.width viewport
                , Math.height = Scene.height viewport
                }
