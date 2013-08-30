module Video where

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Video (freeSurface)

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


vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

texCoord2f :: Float -> Float -> TexCoord2 GLfloat
texCoord2f u v = TexCoord2 (realToFrac u) (realToFrac v)