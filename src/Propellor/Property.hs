{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property (
	-- * Property combinators
	  requires
	, before
	, onChange
	, onChangeFlagOnFail
	, flagFile
	, flagFile'
	, check
	, fallback
	, revert
	-- * Property descriptions
	, describe
	, (==>)
	-- * Constructing properties
	, Propellor
	, property
	, ensureProperty
	, withOS
	, makeChange
	, noChange
	, doNothing
	, endAction
	-- * Property result checking
	, UncheckedProperty
	, unchecked
	, changesFile
	, changesFileContent
	, isNewerThan
	, checkResult
	, Checkable
	, assume
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Monoid
import Control.Monad.IfElse
import "mtl" Control.Monad.RWS.Strict
import System.Posix.Files
import qualified Data.Hash.MD5 as MD5
import Control.Applicative
import Prelude

import Propellor.Types
import Propellor.Types.ResultCheck
import Propellor.Info
import Propellor.Exception
import Utility.Exception
import Utility.Monad
import Utility.Misc

-- | Constructs a Property, from a description and an action to run to
-- ensure the Property is met.
property :: Desc -> Propellor Result -> Property NoInfo
property d s = simpleProperty d s mempty

-- | Makes a perhaps non-idempotent Property be idempotent by using a flag
-- file to indicate whether it has run before.
-- Use with caution.
flagFile :: Property i -> FilePath -> Property i
flagFile p = flagFile' p . return

flagFile' :: Property i -> IO FilePath -> Property i
flagFile' p getflagfile = adjustPropertySatisfy p $ \satisfy -> do
	flagfile <- liftIO getflagfile
	go satisfy flagfile =<< liftIO (doesFileExist flagfile)
  where
	go _ _ True = return NoChange
	go satisfy flagfile False = do
		r <- satisfy
		when (r == MadeChange) $ liftIO $ 
			unlessM (doesFileExist flagfile) $ do
				createDirectoryIfMissing True (takeDirectory flagfile)
				writeFile flagfile ""
		return r

-- | Indicates that the first property depends on the second,
-- so before the first is ensured, the second must be ensured.
--
-- The combined property uses the description of the first property.
requires :: Combines x y => x -> y -> CombinedType x y
requires = combineWith
	-- Run action of y, then x
	(flip (<>))
	-- When reverting, run in reverse order.
	(<>)

-- | Combines together two properties, resulting in one property
-- that ensures the first, and if the first succeeds, ensures the second.
--
-- The combined property uses the description of the first property.
before :: Combines x y => x -> y -> CombinedType x y
before = combineWith
	-- Run action of x, then y
	(<>)
	-- When reverting, run in reverse order.
	(flip (<>))

-- | Whenever a change has to be made for a Property, causes a hook
-- Property to also be run, but not otherwise.
onChange
	:: (Combines x y)
	=> x
        -> y
        -> CombinedType x y
onChange = combineWith combiner revertcombiner
  where
	combiner p hook = do
		r <- p
		case r of
			MadeChange -> do
				r' <- hook
				return $ r <> r'
			_ -> return r
	revertcombiner = (<>)

-- | Same as `onChange` except that if property y fails, a flag file
-- is generated. On next run, if the flag file is present, property y
-- is executed even if property x doesn't change.
--
-- With `onChange`, if y fails, the property x `onChange` y returns
-- `FailedChange`. But if this property is applied again, it returns
-- `NoChange`. This behavior can cause trouble...
onChangeFlagOnFail
	:: (Combines x y)
	=> FilePath
        -> x
        -> y
        -> CombinedType x y
onChangeFlagOnFail flagfile = combineWith combiner revertcombiner
  where
	combiner s1 s2 = do
		r1 <- s1
		case r1 of
			MadeChange -> flagFailed s2
			_ -> ifM (liftIO $ doesFileExist flagfile)
				(flagFailed s2
				, return r1
				)
	revertcombiner = (<>)
	flagFailed s = do
		r <- s
		liftIO $ case r of
			FailedChange -> createFlagFile
			_ -> removeFlagFile
		return r
	createFlagFile = unlessM (doesFileExist flagfile) $ do
		createDirectoryIfMissing True (takeDirectory flagfile)
		writeFile flagfile ""
	removeFlagFile = whenM (doesFileExist flagfile) $ removeFile flagfile

-- | Changes the description of a property.
describe :: IsProp p => p -> Desc -> p
describe = setDesc

-- | Alias for @flip describe@
(==>) :: IsProp (Property i) => Desc -> Property i -> Property i
(==>) = flip describe
infixl 1 ==>

-- | For when code running in the Propellor monad needs to ensure a
-- Property.
--
-- This can only be used on a Property that has NoInfo.
ensureProperty :: Property NoInfo -> Propellor Result
ensureProperty = catchPropellor . propertySatisfy

-- | Tries the first property, but if it fails to work, instead uses
-- the second.
fallback :: (Combines p1 p2) => p1 -> p2 -> CombinedType p1 p2
fallback = combineWith combiner revertcombiner
  where
	combiner a1 a2 = do
		r <- a1
		if r == FailedChange
			then a2
			else return r
	revertcombiner = (<>)

-- | Indicates that a Property may change a particular file. When the file
-- is modified in any way (including changing its permissions or mtime),
-- the property will return MadeChange instead of NoChange.
changesFile :: Checkable p i => p i -> FilePath -> Property i
changesFile p f = checkResult getstat comparestat p
  where
	getstat = catchMaybeIO $ getSymbolicLinkStatus f
	comparestat oldstat = do
		newstat <- getstat
		return $ if samestat oldstat newstat then NoChange else MadeChange
	samestat Nothing Nothing = True
	samestat (Just a) (Just b) = and
		-- everything except for atime
		[ deviceID a == deviceID b
		, fileID a == fileID b
		, fileMode a == fileMode b
		, fileOwner a == fileOwner b
		, fileGroup a == fileGroup b
		, specialDeviceID a == specialDeviceID b
		, fileSize a == fileSize b
		, modificationTimeHiRes a == modificationTimeHiRes b
		, isBlockDevice a == isBlockDevice b
		, isCharacterDevice a == isCharacterDevice b
		, isNamedPipe a == isNamedPipe b
		, isRegularFile a == isRegularFile b
		, isDirectory a == isDirectory b
		, isSymbolicLink a == isSymbolicLink b
		, isSocket a == isSocket b
		]
	samestat _ _ = False

-- | Like `changesFile`, but compares the content of the file.
-- Changes to mtime etc that do not change file content are treated as
-- NoChange.
changesFileContent :: Checkable p i => p i -> FilePath -> Property i
changesFileContent p f = checkResult getmd5 comparemd5 p
  where
	getmd5 = catchMaybeIO $ MD5.md5 . MD5.Str <$> readFileStrictAnyEncoding f
	comparemd5 oldmd5 = do
		newmd5 <- getmd5
		return $ if oldmd5 == newmd5 then NoChange else MadeChange

-- | Determines if the first file is newer than the second file.
--
-- This can be used with `check` to only run a command when a file
-- has changed.
--
-- > check ("/etc/aliases" `isNewerThan` "/etc/aliases.db")
-- > 	(cmdProperty "newaliases" [] `assume` MadeChange) -- updates aliases.db
--
-- Or it can be used with `checkResult` to test if a command made a change.
--
-- > checkResult (return ())
-- > 	(\_ -> "/etc/aliases.db" `isNewerThan` "/etc/aliases")
-- > 	(cmdProperty "newaliases" [])
--
-- (If one of the files does not exist, the file that does exist is
-- considered to be the newer of the two.)
isNewerThan :: FilePath -> FilePath -> IO Bool
isNewerThan x y = do
	mx <- mtime x
	my <- mtime y
	return (mx > my)
  where
	mtime f = catchMaybeIO $ modificationTimeHiRes <$> getFileStatus f

-- | Makes a property that is satisfied differently depending on the host's
-- operating system. 
--
-- Note that the operating system may not be declared for all hosts.
--
-- > myproperty = withOS "foo installed" $ \o -> case o of
-- > 	(Just (System (Debian suite) arch)) -> ...
-- > 	(Just (System (Ubuntu release) arch)) -> ...
-- >	Nothing -> ...
withOS :: Desc -> (Maybe System -> Propellor Result) -> Property NoInfo
withOS desc a = property desc $ a =<< getOS

-- | Undoes the effect of a RevertableProperty.
revert :: RevertableProperty i -> RevertableProperty i
revert (RevertableProperty p1 p2) = RevertableProperty p2 p1

makeChange :: IO () -> Propellor Result
makeChange a = liftIO a >> return MadeChange

noChange :: Propellor Result
noChange = return NoChange

doNothing :: Property NoInfo
doNothing = property "noop property" noChange

-- | Registers an action that should be run at the very end, after
-- propellor has checks all the properties of a host.
endAction :: Desc -> (Result -> Propellor Result) -> Propellor ()
endAction desc a = tell [EndAction desc a]
