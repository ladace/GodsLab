module Render where
    import Scripting.Lua

    class RenderProcedure a where
		render :: a -> LuaState -> IO ()