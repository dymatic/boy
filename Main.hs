import System.IO
import LibHaskell.LibLists
import ProjectSpecific
import System.Process
import System.Exit
import System.Environment
import System.Directory
main = do
	args <- getArgs
	if ((length args) == 1) then do
							inUsrOne <- doesFileExist (at args 0)
							boyFile <- openFile (if inUsrOne then (at args 0) else ("/usr/share/boy/boy1/" ++ (at args 0))) ReadMode
							boyText <- hGetContents boyFile
							(mapM putStrLn (lines boyText))
							exitSuccess
					 else
							return ()
	let (a:b:_) = take 2 args
	inUsr <- doesFileExist b
	realOptionBoyFile  <- openFile (if inUsr then b else ("/usr/share/boy/boy1/" ++ b)) ReadMode
	realOptionBoyText <- hGetContents realOptionBoyFile
	let response = case a of
					"titles" -> (specTitles (lines realOptionBoyText))
					"descriptions" -> (compress (intersperse (allDescriptions (lines (realOptionBoyText))) ["\n"]))
					otherwise -> [""]
	let d = if (null $ grab response) then
							(getInfo (lines realOptionBoyText) a)
						      else
							 response
	mapM putStrLn d
					 
