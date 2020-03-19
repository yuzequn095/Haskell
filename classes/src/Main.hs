import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn welcome
  shell 0  [] -- call helper func

doStr :: Nano.Env -> String
doStr ((id, val):rest) = id ++ " " ++ doStr rest

---------------------------------------------------
shell :: Int -> Nano.Env -> IO()  
---------------------------------------------------
shell i env = do
	putStrFlush ( printf "\x3BB [%d] " i)
	str <- getLine
  	case strCmd str of 
		CQuit -> do 
			 doQuit
			 shell (i+1) env
		CEval str -> do 
			 doEval env str
			 shell (i+1) env
		CRun str -> do
			 doRun str
			 shell (i+1) env
		CLoad str -> do 
			 resEnv <- doLoad str    
			 -- strEnv <- doStr resEnv
			 -- output strEnv
			 output (resEnv)
			 shell (i+1) (resEnv++env) 
		       	 where
			 	--output :: String -> IO()
			 	output :: Nano.Env -> IO()
			 	--output str = do putStrLn ("definition: " ++ str)
			 	output e = do
					putStrLn("definitions: " ++ init( printL e))
				where
					printL :: Nano.Env -> String
					printL [] = []
					printL ((id, val):res) = id ++ " " printL res 
			 --shell (i+1) (env++resEnv)
		CUnknown -> do 
			 doUnknown
			 shell (i+1) env
--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

