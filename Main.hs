import System.IO
import Archimedes.Sequence.Manipulate
import Archimedes.Sequence.Clarify
import Archimedes.Sequence.Functional

import ProjectSpecific

import System.Process
import System.Exit
import System.Environment
import System.Directory

compress :: [[a]] -> [a]
compress [] = []
compress (x:xs) = x ++ compress xs

main = do
	args <- getArgs
	if ((length args) == 1) then do
							inUsrOne <- doesFileExist (args !! 0)
							boyFile <- openFile (if inUsrOne then (args !! 0) else ("/usr/share/boy/boy1/" ++ (args !! 0))) ReadMode
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
	let d = if (null $ head response) then
							(getInfo (lines realOptionBoyText) a)
						      else
							 response
	mapM putStrLn d
					 
