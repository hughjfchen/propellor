module Property where

import System.Directory
import Control.Monad
import System.Console.ANSI
import System.Exit
import System.IO

import Types
import Utility.Monad
import Utility.Exception

makeChange :: IO () -> IO Result
makeChange a = a >> return MadeChange

noChange :: IO Result
noChange = return NoChange

{- Combines a list of properties, resulting in a single property
 - that when run will run each property in the list in turn,
 - and print out the description of each as it's run. Does not stop
 - on failure; does propigate overall success/failure.
 -}
propertyList :: Desc -> [Property] -> Property
propertyList desc ps = Property desc $ ensureProperties' ps

{- Combines a list of properties, resulting in one property that
 - ensures each in turn, stopping on failure. -}
combineProperties :: [Property] -> Property
combineProperties ps = Property desc $ go ps NoChange
  where
  	go [] rs = return rs
	go (l:ls) rs = do
		r <- ensureProperty l
		case r of
			FailedChange -> return FailedChange
			_ -> go ls (combineResult r rs)
	desc = case ps of
		(p:_) -> propertyDesc p
		_ -> "(empty)"

{- Makes a perhaps non-idempotent Property be idempotent by using a flag
 - file to indicate whether it has run before.
 - Use with caution. -}
flagFile :: Property -> FilePath -> Property
flagFile property flagfile = Property (propertyDesc property) $
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
property `onChange` hook = Property (propertyDesc property) $ do
	r <- ensureProperty property
	case r of
		MadeChange -> do
			r' <- ensureProperty hook
			return $ combineResult r r'
		_ -> return r

{- Indicates that the first property can only be satisfied once
 - the second is. -} 
requires :: Property -> Property -> Property
x `requires` y = combineProperties [y, x] `describe` propertyDesc x

describe :: Property -> Desc -> Property
describe p d = p { propertyDesc = d }

(==>) :: Desc -> Property -> Property
(==>) = flip describe
infixl 1 ==>

{- Makes a Property only be performed when a test succeeds. -}
check :: IO Bool -> Property -> Property
check c property = Property (propertyDesc property) $ ifM c
	( ensureProperty property
	, return NoChange
	)

ensureProperty :: Property -> IO Result
ensureProperty = catchDefaultIO FailedChange . propertySatisfy

ensureProperties :: [Property] -> IO ()
ensureProperties ps = do
	r <- ensureProperties' [propertyList "overall" ps]
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess

ensureProperties' :: [Property] -> IO Result
ensureProperties' ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
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

warningMessage :: String -> IO ()
warningMessage s = do
	setSGR [SetColor Foreground Vivid Red]
	putStrLn $ "** warning: " ++ s
	setSGR []
	hFlush stdout
