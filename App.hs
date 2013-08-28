module App where

import qualified Scripting.Lua as Lua

data AppControl = AppControl  { printInfoStatus_ :: String -> IO () }
data App a = App { appData :: a, appScript :: Lua.LuaState, appControl :: AppControl }

printInfoStatus = printInfoStatus_ . appControl