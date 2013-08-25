module Samples.Image where
    import Graphics.Rendering.OpenGL
    import Graphics.UI.SDL.Image
    import Graphics.UI.SDL.Types
    import Graphics.UI.SDL.Video (freeSurface)
    import Render

    data Data = Data TextureObject
    instance RenderProcedure Data where
        render (Data name) = do
            clearColor $= Color4 0 0 0 0
            clear [ ColorBuffer ]

            textureBinding Texture2D $= Just name
            texture Texture2D $= Enabled

            renderPrimitive Quads $ do

                texCoord $ texCoord2f 0 1
                vertex $ vertex3f (-1) (-1) 0
                texCoord $ texCoord2f 1 1
                vertex $ vertex3f 1 (-1) 0
                texCoord $ texCoord2f 1 0
                vertex $ vertex3f 1 1 0
                texCoord $ texCoord2f 0 0
                vertex $ vertex3f (-1) 1 0

    initialize :: IO Data
    initialize = do
        surface <- load "assets/samples/test.png"
        let width = fromIntegral $ surfaceGetWidth surface
            height = fromIntegral $ surfaceGetHeight surface


        bpp <- pixelFormatGetBytesPerPixel (surfaceGetPixelFormat surface)
        print bpp
        
        -- Due to Haskell-SDL's limits:
        --  Can't support palattes
        --  Only support specific formats

        let pixelFormat = case bpp of
                              1 -> RGB
                              3 -> RGB
                              4 -> BGRA
        let dataType = case bpp of
                            3 -> UnsignedByte
                            4 -> UnsignedByte
                            1 -> UnsignedByte332

        [name] <- genObjectNames 1 :: IO [TextureObject]
        textureBinding Texture2D $= Just name
        rowAlignment Unpack $= 1

        pixels <- surfaceGetPixels surface

        texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D width height) 0 (PixelData pixelFormat dataType pixels)

        freeSurface surface

        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)

        return $ Data name

    vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
    vertex3f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

    texCoord2f :: Float -> Float -> TexCoord2 GLfloat
    texCoord2f u v = TexCoord2 (realToFrac u) (realToFrac v)