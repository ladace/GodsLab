module Samples.Triangle(initialize) where
	import Graphics.Rendering.OpenGL
	import Render

	data Data = Data
	instance RenderProcedure Data where
		render _ = do
		    clearColor $= Color4 0 0 0 0
		    clear [ ColorBuffer ]
		    matrixMode $= Modelview 0
		    let renderV ((x, y, z), (r, g, b)) = do
		        color $ Color3 r g b
		        vertex $ Vertex3 x y z
		    renderPrimitive Triangles $ mapM_ renderV $ zip myPoints myColors
		    rotate (1::GLfloat) $ Vector3 0 0 1

	initialize :: IO Data
	initialize = return Data

	myPoints :: [(GLfloat, GLfloat, GLfloat)]
	myPoints = map (\i -> let r = i * 2 * pi / 3 in (sin r, cos r, 0.0)) [0..2]

	myColors :: [(GLfloat, GLfloat, GLfloat)]
	myColors = [(255, 0, 0), (0, 255, 0), (0, 0, 255)]