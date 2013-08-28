module App where

import qualified Scripting.Lua as Lua

data AppControl = AppControl -- reserved
data App a = App { appData :: a, appScript :: Lua.LuaState, appControl :: AppControl }