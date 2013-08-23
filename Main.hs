module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL

import Render
import qualified TriangleSample as R

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

    dat <- R.initialize
 
    WX.set f [ layout := widget glw,
        on paintRaw := paintGL dat glContext glCanvas ]

    -- frameShowFullScreen f True wxFULLSCREEN_ALL
    timer f [interval := 20, on command := paintIt dat glContext glCanvas (WX.Size defaultWidth defaultHeight)]
    return ()

createMenu frame = do
    file <- menuPane [ text := "&File" ]
    mclose <- menuItem file [text := "&Close\tCtrl+Q", help := "Close the document"]

    WX.set frame [ menuBar := [file],
                   on (menu mclose) := close frame]

paintGL dat glContext glWindow _ myrect _ = paintIt dat glContext glWindow $ rectSize myrect
paintIt dat glContext glWindow size = do
    glCanvasSetCurrent glWindow glContext
    GL.viewport $= (Position 0 0, convWG size)
    render dat
    flush
    glCanvasSwapBuffers glWindow
    return ()

convWG (WX.Size w h) = GL.Size (convInt32 w) (convInt32  h)
convInt32 = fromInteger . toInteger
