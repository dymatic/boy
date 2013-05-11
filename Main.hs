import System.IO

import LibHaskell.LibLists
import ProjectSpecific
import System.Process
import System.Exit
import System.Environment

main = do
	(b:a:_) <- getArgs
	boyFile <- openFile a ReadMode
	boyText <- hGetContents boyFile

	let rspnc = case b of
						"titles" -> (specTitles (lines boyText))
						"descriptions" -> (compress (intersperse (allDescriptions (lines boyText)) ["\n"]))
						otherwise -> [""]

	let d = (if (null $ grab rspnc) then
								(getInfo (lines boyText) b)
								else rspnc)
	mapM putStrLn d
