module App where

import Data.IORef
import Control.Monad (when)

import qualified Scripting.Lua as Lua

data AppControl = AppControl  { printInfoStatus_ :: String -> IO () }
data App a = App { appData :: a, appScript :: Lua.LuaState, appScriptStatus :: ScriptStatus, appControl :: AppControl }

printInfoStatus = printInfoStatus_ . appControl

data ScriptStatus = ScriptStatus (IORef ScriptStatus_)
data ScriptStatus_ = ScriptReady | ScriptInvalid deriving Eq

setScriptStatus app = writeIORef ss
    where ScriptStatus ss = appScriptStatus app
getScriptStatus app = readIORef ss
    where ScriptStatus ss = appScriptStatus app
guardScript app proc = do
    b <- getScriptStatus app
    when (b == ScriptReady) proc

newScriptStatus = return . ScriptStatus =<< newIORef ScriptReady