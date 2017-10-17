{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                    (void)
import           Data.Vector.Storable             (Vector, fromList)
import           Graphics.GL                      (GLfloat, GLuint)
import           Linear                           (M44, V3 (..))
import           Scene
import qualified Scene
import           Scene.GL.Attribute.VertexWithPos (VertexWithPos (..))
import           Scene.Math
import qualified Scene.Math                       as Math
--import           Text.Printf (printf)

data App = App
    { triangle :: !Entity
    }

main :: IO ()
main = do
    let globalSettings' = [ ClearColor 1 0 0 1 ]
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
        (Right program, Right mesh) -> do
            let triangle' = Entity { entitySettings = []
                                   , entityProgram = program
                                   , entityMesh = mesh
                                   , entityUniforms = []
                                   }
            return $ Just App { triangle = triangle' }

        _ -> return Nothing

appEvent :: Viewer -> Event -> Maybe App -> IO (Maybe App)

-- | Catch the frame event and render stuff.
appEvent viewer (Frame _ viewport) (Just app) = do
    let perspectiveMatrix =
            mkPerspectiveMatrix (Degrees 45) (toAspectRatio viewport) 0.1 1000 :: M44 GLfloat
        viewMatrix = mkViewMatrix (V3 0 0 10) origo3d up3d
        modelMatrix = mkIdentityMatrix
        mvp = mvpMatrix modelMatrix viewMatrix perspectiveMatrix

    setCurrentScene viewer
        Scene { sceneSettings = [ Clear [ColorBufferBit] ]
              , sceneEntities =
                  [(triangle app)
                    { entityUniforms = [ UniformValue "col" triangleColor
                                       , UniformValue "mvp" mvp
                                       ]}]
              }
    return (Just app)

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

toAspectRatio :: Viewport -> AspectRatio
toAspectRatio viewport =
    AspectRatio { Math.width = Scene.width viewport
                , Math.height = Scene.height viewport
                }
