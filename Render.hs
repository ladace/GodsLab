module Render where
	class RenderProcedure a where
		render :: a -> IO ()