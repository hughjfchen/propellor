module Property where

import System.Directory
import Control.Applicative
import Control.Monad
import System.Console.ANSI
import System.Exit
import System.IO

import Utility.Monad
import Utility.Exception
import Utility.SafeCommand
import Utility.Tmp
import Utility.Env

-- Ensures that the system has some property. 
-- Actions must be idempotent; will be run repeatedly.
data Property
	= FileProperty Desc FilePath ([Line] -> [Line])
	| CmdProperty Desc String [CommandParam] [(String, String)]
	| IOProperty Desc (IO Result)

data Result = NoChange | MadeChange | FailedChange
	deriving (Show, Eq)

type Line = String
type Desc = String

combineResult :: Result -> Result -> Result
combineResult FailedChange _ = FailedChange
combineResult _ FailedChange = FailedChange
combineResult MadeChange _ = MadeChange
combineResult _ MadeChange = MadeChange
combineResult NoChange NoChange = NoChange

propertyDesc :: Property -> Desc
propertyDesc (FileProperty d _ _) = d
propertyDesc (CmdProperty d _ _ _) = d
propertyDesc (IOProperty d _) = d

combineProperties :: Desc -> [Property] -> Property
combineProperties desc ps = IOProperty desc $ go ps NoChange
  where
  	go [] rs = return rs
	go (l:ls) rs = do
		r <- ensureProperty l
		case r of
			FailedChange -> return FailedChange
			_ -> go ls (combineResult r rs)

ensureProperty :: Property -> IO Result
ensureProperty = catchDefaultIO FailedChange . ensureProperty'

ensureProperty' :: Property -> IO Result
ensureProperty' (FileProperty _ f a) = go =<< doesFileExist f
  where
	go True = do
		ls <- lines <$> readFile f
		let ls' = a ls
		if ls' == ls
			then noChange
			else makeChange $ viaTmp writeFile f (unlines ls')
	go False = makeChange $ writeFile f (unlines $ a [])
ensureProperty' (CmdProperty _ cmd params env) = do
	env' <- addEntries env <$> getEnvironment
	ifM (boolSystemEnv cmd params (Just env'))
		( return MadeChange
		, return FailedChange
		)
ensureProperty' (IOProperty _ a) = a

ensureProperties :: [Property] -> IO ()
ensureProperties ps = do
	r <- ensure ps NoChange
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
		putStr $ propertyDesc l ++ "... "
		hFlush stdout
		r <- ensureProperty l
		clearFromCursorToLineBeginning
		setCursorColumn 0
		putStr $ propertyDesc l ++ "... "
		case r of
			FailedChange -> do
				setSGR [SetColor Foreground Vivid Red]
				putStrLn "failed"
			NoChange -> do
				setSGR [SetColor Foreground Dull Green]
				putStrLn "unchanged"
			MadeChange -> do
				setSGR [SetColor Foreground Vivid Green]
				putStrLn "done"
		setSGR []
		ensure ls (combineResult r rs)

makeChange :: IO () -> IO Result
makeChange a = a >> return MadeChange

noChange :: IO Result
noChange = return NoChange

cmdProperty :: String -> [CommandParam] -> Property
cmdProperty cmd params = cmdProperty' cmd params []

cmdProperty' :: String -> [CommandParam] -> [(String, String)] -> Property
cmdProperty' cmd params env = CmdProperty desc cmd params env
  where
  	desc = unwords $ cmd : map showp params
	showp (Params s) = s
	showp (Param s) = s
	showp (File s) = s

{- Replaces all the content of a file. -}
fileHasContent :: FilePath -> [Line] -> Property
fileHasContent f newcontent = FileProperty ("replace " ++ f)
	f (\_oldcontent -> newcontent)

{- Ensures that a line is present in a file, adding it to the end if not. -}
lineInFile :: FilePath -> Line -> Property
lineInFile f l = FileProperty (f ++ " contains:" ++ l) f go
  where
	go ls
		| l `elem` ls = ls
		| otherwise = ls++[l]

{- Ensures that a line is not present in a file.
 - Note that the file is ensured to exist, so if it doesn't, an empty
 - file will be written. -}
lineNotInFile :: FilePath -> Line -> Property
lineNotInFile f l = FileProperty (f ++ " remove: " ++ l) f (filter (/= l))

{- Makes a perhaps non-idempotent Property be idempotent by using a flag
 - file to indicate whether it has run before.
 - Use with caution. -}
flagFile :: Property -> FilePath -> Property
flagFile property flagfile = IOProperty (propertyDesc property) $
	go =<< doesFileExist flagfile
  where
	go True = return NoChange
	go False = do
		r <- ensureProperty property
		when (r == MadeChange) $
			writeFile flagfile ""
		return r

{- Whenever a change has to be made for a Property, causes a hook
 - Property to also be run, but not otherwise. -}
onChange :: Property -> Property -> Property
property `onChange` hook = IOProperty (propertyDesc property) $ do
	r <- ensureProperty property
	case r of
		MadeChange -> do
			r' <- ensureProperty hook
			return $ combineResult r r'
		_ -> return r

requires :: Property -> Property -> Property
x `requires` y = combineProperties (propertyDesc x) [y, x]

{- Makes a Property only be performed when a test succeeds. -}
check :: IO Bool -> Property -> Property
check c property = IOProperty (propertyDesc property) $ ifM c
	( ensureProperty property
	, return NoChange
	)
