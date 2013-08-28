module App where

import Data.IORef
import Control.Monad (when)
import Control.Exception as E

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

invokeScript app funcname = invokeScript' app False (appScript app) funcname (return ()) 0
invokeScriptMaybe app funcname = invokeScript' app True (appScript app) funcname (return ()) 0

class ScriptInvokeProc a where
    invokeScript' :: App b -> Bool -> Lua.LuaState -> String -> IO () -> Int -> a

instance ScriptInvokeProc (IO t) where
    invokeScript' app may l f a k = E.catch (guardScript app runProc >> return undefined) $ \e ->
        print (e :: IOException) >> return undefined
        where
            runProc = do
                Lua.getglobal2 l f
                if may then do
                    exist <- Lua.isfunction l (-1)
                    if not exist then
                        Lua.pop l 1
                    else runIt
                else runIt
            runIt = do
                a
                z <- Lua.pcall l k 0 0
                if z/=0
                    then do
                        Just msg <- Lua.peek l (-1)
                        Lua.pop l 1
                        Prelude.fail msg
                    else return undefined

instance (Lua.StackValue t, ScriptInvokeProc b) =>  ScriptInvokeProc (t -> b) where
    invokeScript' app may l f a k x = invokeScript' app may l f (a >> Lua.push l x) (k+1)