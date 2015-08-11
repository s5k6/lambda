{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import System.Console.Haskeline

main :: IO ()
main = evalStateT (runInputT defaultSettings loop) 0

loop :: InputT (StateT Int IO) ()
loop = do x <- getInputLine "man, type> "
          case x of
            Nothing -> return ()
            Just t -> do v <- lift get
                         outputStrLn $ "Value was "++show v
                         lift $ put (v + read t)
                         loop
