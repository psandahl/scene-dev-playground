{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                    (void)
import           Data.Vector.Storable             (Vector, fromList)
import           Flow                             ((<|))
import           Graphics.GL                      (GLfloat, GLuint)
import           Linear                           (M44, V3 (..), (!*!))
import           Scene
import           Scene.GL.Attribute.VertexWithPos (VertexWithPos (..))
import           Scene.Math
import qualified Scene.Math                       as Math
--import           Text.Printf (printf)

data App = App
    { triangleProgram :: !Program
    , triangleMesh    :: !Mesh
    , trianglesAt     :: ![Angle GLfloat]
    }

main :: IO ()
main = do
    let globalSettings' = [ SetClearColor 1 0 0 1 ]
        initialScene' =
            Scene { sceneSettings = [ Clear [ColorBufferBit]]
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

    meshResult <- meshFromRequest viewer
        MeshRequest
            { vertices = triangleVertices
            , indices = triangleIndices
            , primitive = Triangles
            }

    case (progResult, meshResult) of
        (Right program, Right mesh) ->
            return $
                Just App { triangleProgram = program
                         , triangleMesh = mesh
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

        entities = map (renderTriangle (triangleProgram app) (triangleMesh app)
                        perspectiveMatrix viewMatrix) trianglesAt'

    setScene viewer
        Scene
            { sceneSettings = [Clear [ColorBufferBit]]
            , sceneEntities = entities
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

renderTriangle :: Program -> Mesh -> M44 GLfloat
               -> M44 GLfloat -> Angle GLfloat -> Entity
renderTriangle program mesh perspective view angle =
    -- Rotate around the y axis when transformed.
    let modelMatrix =
            mkRotationMatrix y3d angle !*!
                mkTranslationMatrix (V3 0 0 triangleRotationRadius)
    in Entity
        { entitySettings = []
        , entityProgram = program
        , entityMesh = mesh
        , entityUniforms =
            [ UniformValue "col" triangleColor
            , UniformValue "mvp" <| mvpMatrix modelMatrix view perspective
            ]
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

triangleColor :: V3 GLfloat
triangleColor = V3 0 0 1

toAspectRatio :: Viewport -> AspectRatio
toAspectRatio viewport =
    AspectRatio { Math.width = Scene.width viewport
                , Math.height = Scene.height viewport
                }
