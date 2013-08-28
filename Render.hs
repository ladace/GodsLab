module Render where

import App

class RenderProcedure a where
	render :: App a -> IO ()