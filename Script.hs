module Script where

import Scripting.Lua
import qualified Video
import qualified Graphics.Rendering.OpenGL as GL

import Foreign.Storable as S
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad

data LuaError = LuaSuccess | LuaErrRun | LuaErrMem | LuaErrErr

toLuaError :: Int -> LuaError
toLuaError 0 = LuaSuccess
toLuaError 2 = LuaErrRun
toLuaError 4 = LuaErrMem
toLuaError 6 = LuaErrErr

onError :: Int -> (LuaError -> IO ()) -> IO ()
onError 0 _ = return ()
onError e proc = proc $ toLuaError e

initialize :: IO LuaState
initialize = do
    l <- newstate
    openlibs l
    doExport l

    pushcfunction l =<< wrap errorfunc

    err <- loadfile l "main.lua"

    when (err == 0) $
        pcall l 0 0 (-2) >>= flip onError handler
        
    return l

    where
        handler LuaErrRun = putStrLn "Script Error: Initial Run"
        handler LuaErrMem = putStrLn "Script Error: Memory Allocation"
        handler LuaErrErr = putStrLn "Script Error: Error Handler"

doExport :: LuaState -> IO ()
doExport l = do
    checkV 1 =<< newmetatable l "Image" -- returns 0
    pop l 1
    registerhsfunction l "loadImage" Video.loadImage
    registerhsfunction l "drawImage" Video.drawImage

foreign import ccall "wrapper"
    wrap :: LuaCFunction -> IO (FunPtr LuaCFunction) -- Never free them

errorfunc :: LuaCFunction
errorfunc l = do
    putStrLn =<< tostring l 1
    pushvalue l 1
    return 1

checkV :: Eq a => a -> a -> IO ()
checkV expected actual = when (expected /= actual) $ putStrLn "Assertion: Return value Not Matched!"

instance Storable Video.Image where
    peek p = do
        name <- S.peek $ castPtr p
        width <- S.peek $ plusPtr p $ sizeOf name
        height <- S.peek $ plusPtr p $ sizeOf name + sizeOf width
        return $ Video.Image (GL.TextureObject name) width height
    poke p img = do
        let Video.Image (GL.TextureObject texId) width height = img
        poke (castPtr p) texId
        poke (p `plusPtr` sizeOf texId) width
        poke (p `plusPtr` sizeOf texId `plusPtr` sizeOf width) height
        return ()
    sizeOf (Video.Image (GL.TextureObject texId) width height) = sizeOf texId + sizeOf width + sizeOf height
    alignment _ = 4

instance StackValue Float where
    push l a = pushnumber l (realToFrac a)
    peek l n = liftM (Just . realToFrac) $ tonumber l n
    valuetype _ = TNUMBER


instance StackValue Video.Image where
    push l a = do
        p <- newuserdata l $ sizeOf a
        poke (castPtr p) a
        setClass l "Image"

    peek = checkUData "Image"
    valuetype _ = TUSERDATA

checkUData :: (StackValue a, Storable a) => String -> LuaState -> Int -> IO (Maybe a)
checkUData tname l argn =
    getmetatable l argn >>= flip success (do
        getfield l registryindex tname
        eq <- equal l (-1) (-2)
        pop l 2

        success eq $ liftM Just $ touserdata l argn >>= S.peek)

setClass l className= do
    getfield l registryindex className
    setmetatable l (-2)

success :: MonadPlus m => Bool -> IO (m a) -> IO (m a)
success True proc = proc
success False _ = return mzero