module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL

defaultWidth = 480
defaultHeight = 480

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    sfield <- statusField [ text := "ready"]
    f <- frame [ text := "GodLab",
                 statusBar := [ sfield ] ]

    createMenu f

    glw <- window f [area := WX.Rect 0 0 defaultWidth defaultHeight]
    WX.set f [visible := True]

    glCanvas <- glCanvasCreateEx glw 0 (Rect 0 0 defaultWidth defaultHeight) 0 "GLCanvas" [GL_RGBA] nullPalette
    glContext <- glContextCreateFromNull glCanvas
    multisample $= Enabled
    WX.set f [ layout := widget glw,
        on paintRaw := paintGL glContext glCanvas ]

    -- frameShowFullScreen f True wxFULLSCREEN_ALL
    timer f [interval := 20, on command := paintIt glContext glCanvas (WX.Size defaultWidth defaultHeight)]
    return ()

createMenu frame = do
    file <- menuPane [ text := "&File" ]
    mclose <- menuItem file [text := "&Close\tCtrl+Q", help := "Close the document"]

    WX.set frame [ menuBar := [file],
                   on (menu mclose) := close frame]

paintGL glContext glWindow dc myrect _ = paintIt glContext glWindow $ rectSize myrect
paintIt glContext glWindow size = do
    glCanvasSetCurrent glWindow glContext
    GL.viewport $= (Position 0 0, convWG size)
    clearColor $= Color4 0 0 0 0
    clear [ ColorBuffer ]
    GL.matrixMode GL.$= GL.Modelview 0
    let renderV ((x, y, z), (r, g, b)) = do
        GL.color $ Color3 r g b
        vertex $ Vertex3 x y z
    renderPrimitive Triangles $ mapM_ renderV $ zip myPoints myColors
    rotate (1::GLfloat) $ Vector3 0 0 1
    flush
    glCanvasSwapBuffers glWindow
    return ()

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\i -> let r = i * 2 * pi / 3 in (sin r, cos r, 0.0)) [0..2]

myColors :: [(GLfloat, GLfloat, GLfloat)]
myColors = [(255, 0, 0), (0, 255, 0), (0, 0, 255)]

convWG (WX.Size w h) = GL.Size (convInt32 w) (convInt32  h)
convInt32 = fromInteger . toInteger
