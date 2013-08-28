module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL

import App
import Render
import Video as R

import qualified Script

defaultWidth = 480
defaultHeight = 480

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    infoField <- statusField [ text := "ready"]
    f <- frame [ text := "God's Lab",
                 statusBar := [ infoField ] ]

    createMenu f

    glw <- window f [area := WX.Rect 0 0 defaultWidth defaultHeight]
    WX.set f [visible := True]

    glCanvas <- glCanvasCreateEx glw 0 (Rect 0 0 defaultWidth defaultHeight) 0 "GLCanvas" [GL_RGBA] nullPalette
    glContext <- glContextCreateFromNull glCanvas

    glCanvasSetCurrent glCanvas glContext

    dat <- R.initialize defaultWidth defaultHeight

    script <- Script.initialize

    glwSize <- WX.get glw clientSize
    GL.viewport $= (Position 0 0, convWG glwSize)
 
    let app = App dat script AppControl
    WX.set f [ layout := widget glw, on paintRaw := paintGL app glContext glCanvas]

    -- frameShowFullScreen f True wxFULLSCREEN_ALL
    timer f [interval := 20, on command := paintIt app glContext glCanvas (WX.Size defaultWidth defaultHeight)]
    return ()

createMenu frame = do
    file <- menuPane [ text := "&File" ]
    mclose <- menuItem file [text := "&Close\tCtrl+Q", help := "Close the document"]

    WX.set frame [ menuBar := [file],
                   on (menu mclose) := close frame]

paintGL app glContext glWindow _ myrect _ = paintIt app glContext glWindow $ rectSize myrect
paintIt app glContext glWindow _ = do
    glCanvasSetCurrent glWindow glContext
    multisample $= Enabled

    render app
    flush
    glCanvasSwapBuffers glWindow
    return ()

convWG (WX.Size w h) = GL.Size (convInt32 w) (convInt32  h)
convInt32 = fromInteger . toInteger
