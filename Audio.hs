module Audio where

import Data.IORef
import Foreign.ForeignPtr

import Graphics.UI.SDL.Mixer.Music as SDL
import Graphics.UI.SDL.Mixer as SDL

data Audio = Audio (IORef (Maybe SDL.Music))

initialize = do
    openAudio 44100 AudioS16Sys 2 1024
    return . Audio =<< newIORef Nothing

finalize (Audio ref) = do
    finalizeMusic =<< readIORef ref
    closeAudio
    where
        finalizeMusic (Just mus) = touchForeignPtr mus -- We must touch the musicStruct to ensure the music will not be garbage collected
        finalizeMusic Nothing = return ()

playMusic (Audio ref) path = do
    mus <- loadMUS path
    SDL.playMusic mus (-1)

    writeIORef ref (Just mus)
