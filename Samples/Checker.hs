module Samples.Checker(initialize) where
	import Graphics.Rendering.OpenGL
	import Foreign.Ptr
	import Foreign.Storable
	import Foreign.Marshal
	import Data.IORef

	import Render

	data Data = Data TextureObject

	instance RenderProcedure Data where
		render (Data name) = do
			clearColor $= Color4 0 0 0 0
			clear [ ColorBuffer ]

			textureBinding Texture2D $= Just name
			texture Texture2D $= Enabled

			renderPrimitive Quads $ do

		    	texCoord $ texCoord2f 0 0
		    	vertex $ vertex3f (-1) (-1) 0
		    	texCoord $ texCoord2f 5 0
		    	vertex $ vertex3f 1 (-1) 0
		    	texCoord $ texCoord2f 5 5
		    	vertex $ vertex3f 1 1 0
		    	texCoord $ texCoord2f 0 5
		    	vertex $ vertex3f (-1) 1 0

			return ()

	initialize :: IO Data
	initialize = do
		let bytes = 2 * 2 * 4
		allocaBytes bytes $ \tex -> do
			[name] <- genObjectNames 1 :: IO [TextureObject]
			setBufferFromData tex textureData
			textureBinding Texture2D $= Just name
			rowAlignment Unpack $= 2
			texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D 2 2) 0 (PixelData RGBA UnsignedByte tex)

			textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
			textureWrapMode Texture2D S $= (Repeated, Repeat)
			textureWrapMode Texture2D T $= (Repeated, Repeat)

			return $ Data name

	vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
	vertex3f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

	texCoord2f :: Float -> Float -> TexCoord2 GLfloat
	texCoord2f u v = TexCoord2 (realToFrac u) (realToFrac v)

	textureData :: [[GLubyte]]
	textureData = [[255, 0], [0, 255]]

	setBufferFromData :: Ptr GLubyte -> [[GLubyte]] -> IO ()
	setBufferFromData ptr dat = do
		idx <- newIORef 0
		mapM_ (\l -> mapM_ (setPixel idx) l) dat
		where setPixel idx v = do
			i <- readIORef idx
			pokeByteOff ptr i       v
			pokeByteOff ptr (i + 1) v
			pokeByteOff ptr (i + 2) v
			pokeByteOff ptr (i + 3) (255::GLubyte)
			writeIORef  idx (i + 4)
