module Video (
    initialize, Video,
    Image(Image), loadImage, Video.imageWidth, Video.imageHeight,
    drawImage, drawImageOrig, drawImageRgn, drawImageEx,
    Video.Font(Font), loadFont, drawText,
    _drawText) where

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT

import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video (freeSurface)
import Graphics.Rendering.FTGL as FTGL

import App
import Render

data Video = Video

instance RenderProcedure Video where
    render app = do
        clear [ ColorBuffer ]
        invokeScript app "user.draw"


initialize :: Int -> Int -> IO Video
initialize w h = do
    matrixMode $= Modelview 0
    ortho 0 (fromIntegral w) (fromIntegral h) 0 (-10000) 10000
    return Video

data Image = Image { imageTexture :: TextureObject, imageWidth :: Int, imageHeight :: Int }

loadImage :: String -> IO Image
loadImage path = do
    surface <- load path
    let width = fromIntegral $ SDL.surfaceGetWidth surface
        height = fromIntegral $ SDL.surfaceGetHeight surface

    [name] <- genObjectNames 1 :: IO [TextureObject]
    textureBinding Texture2D $= Just name
    rowAlignment Unpack $= 1

    pixels <- SDL.surfaceGetPixels surface

    bpp <- SDL.pixelFormatGetBytesPerPixel (SDL.surfaceGetPixelFormat surface)
    let pixelFormat = case bpp of
                          1 -> RGB
                          3 -> RGB
                          4 -> BGRA
    let dataType = case bpp of
                        3 -> UnsignedByte
                        4 -> UnsignedByte
                        1 -> UnsignedByte332

    texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D width height) 0 (PixelData pixelFormat dataType pixels)

    freeSurface surface

    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    return $ Image name (fromIntegral width) (fromIntegral height)

drawImageOrig :: Image -> Float -> Float -> IO ()
drawImageOrig img l t = drawImage img l t (fromIntegral $ Video.imageWidth img) (fromIntegral $ Video.imageHeight img)

drawImage :: Image -> Float -> Float -> Float -> Float -> IO ()
drawImage img {-l t w h-} = drawImageEx img 0 0 0

drawImageEx img ox oy r = drawImageRgn img ox oy r 0 0 (fromIntegral $ Video.imageWidth img) (fromIntegral $ Video.imageHeight img)

drawImageRgn img ox oy r srcx srcy srcw srch x y w h = do
    let iw = fromIntegral $ Video.imageWidth img
        ih = fromIntegral $ Video.imageHeight img
    textureBinding Texture2D $= Just (imageTexture img)
    texture Texture2D $= Enabled

    let srcl = srcx / iw
        srct = srcy / ih
        srcr = (srcx + srcw) / iw
        srcb = (srcy + srch) / ih
        ll = -ox
        lt = -oy
        lr = w - ox
        lb = h - oy
        calcX rx ry = (rx * cos r) - (ry * sin r) + x
        calcY rx ry = (rx * sin r) + (ry * cos r) + y
        drawV px py = vertex $ vertex3f (calcX px py) (calcY px py) 0

    renderPrimitive Quads $ do
        texCoord $ texCoord2f srcl srct
        drawV ll lt
        texCoord $ texCoord2f srcr srct
        drawV lr lt
        texCoord $ texCoord2f srcr srcb
        drawV lr lb
        texCoord $ texCoord2f srcl srcb
        drawV ll lb

_drawText :: Float -> Float -> String -> Bool -> IO ()
_drawText x y str mono = preservingMatrix $ do
    color $ Color3 255 255 (255::GLfloat)

    loadIdentity
    ortho (-1) 1 (-1) 1 (-10000) 10000
    scale 0.001 0.001 (0.001 ::GLfloat)
    rasterPos $ Vertex2 x y
    GLUT.renderString (if mono
        then GLUT.MonoRoman
        else GLUT.Roman) str

newtype Font = Font FTGL.Font
loadFont :: String -> Int -> IO Video.Font
loadFont path sz = do
    f <- createTextureFont path
    checkV 1 =<< setFontFaceSize f sz 0 -- costing calculation here, so put it in the load procedure
    -- FTGL.setCharMap f FTGL.EncodingUnicode -- Not necessary
    return $ Font f
drawText :: Video.Font -> String -> Float -> Float -> IO ()
drawText (Font f) str x y = preservingMatrix $ do
    translate $ Vector3 (x::Float) y 0
    scale 1 (-1) (1::GLfloat)
    renderFont f str All

vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

texCoord2f :: Float -> Float -> TexCoord2 GLfloat
texCoord2f u v = TexCoord2 (realToFrac u) (realToFrac v)