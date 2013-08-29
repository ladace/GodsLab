module Video where

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL.Image
import Graphics.UI.SDL.Types
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

data Image = Image { imageTexture :: TextureObject, mImageWidth :: Int, mImageHeight :: Int }

drawImage :: Image -> Float -> Float -> Float -> Float -> IO ()
drawImage img l t w h = do
    textureBinding Texture2D $= Just (imageTexture img)
    texture Texture2D $= Enabled

    let r = l + w
        b = t + h
    renderPrimitive Quads $ do
        texCoord $ texCoord2f 0 0
        vertex $ vertex3f l t 0
        texCoord $ texCoord2f 1 0
        vertex $ vertex3f r t 0
        texCoord $ texCoord2f 1 1
        vertex $ vertex3f r b 0
        texCoord $ texCoord2f 0 1
        vertex $ vertex3f l b 0

loadImage :: String -> IO Image
loadImage path = do
    surface <- load path
    let width = fromIntegral $ surfaceGetWidth surface
        height = fromIntegral $ surfaceGetHeight surface

    [name] <- genObjectNames 1 :: IO [TextureObject]
    textureBinding Texture2D $= Just name
    rowAlignment Unpack $= 1

    pixels <- surfaceGetPixels surface

    bpp <- pixelFormatGetBytesPerPixel (surfaceGetPixelFormat surface)
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


vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

texCoord2f :: Float -> Float -> TexCoord2 GLfloat
texCoord2f u v = TexCoord2 (realToFrac u) (realToFrac v)