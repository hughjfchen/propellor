{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Monoid
import Control.Monad.IfElse
import "mtl" Control.Monad.RWS.Strict

import Propellor.Types
import Propellor.Info
import Utility.Monad

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

-- | Whenever a change has to be made for a Property, causes a hook
-- Property to also be run, but not otherwise.
onChange
	:: (Combines (Property x) (Property y))
	=> Property x
        -> Property y
        -> CombinedType (Property x) (Property y)
onChange = combineWith $ \p hook -> do
	r <- p
	case r of
		MadeChange -> do
			r' <- hook
			return $ r <> r'
		_ -> return r

-- | Same as `onChange` except that if property y fails, a flag file
-- is generated. On next run, if the flag file is present, property y
-- is executed even if property x doesn't change.
--
-- With `onChange`, if y fails, the property x `onChange` y returns
-- `FailedChange`. But if this property is applied again, it returns
-- `NoChange`. This behavior can cause trouble...
onChangeFlagOnFail
	:: (Combines (Property x) (Property y))
	=> FilePath
        -> Property x
        -> Property y
        -> CombinedType (Property x) (Property y)
onChangeFlagOnFail flagfile = combineWith go
  where
	go s1 s2 = do
		r1 <- s1
		case r1 of
			MadeChange -> flagFailed s2
			_ -> ifM (liftIO $ doesFileExist flagfile)
				(flagFailed s2
				, return r1
				)
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

-- | Alias for @flip describe@
(==>) :: IsProp (Property i) => Desc -> Property i -> Property i
(==>) = flip describe
infixl 1 ==>

-- | Makes a Property only need to do anything when a test succeeds.
check :: IO Bool -> Property i -> Property i
check c p = adjustPropertySatisfy p $ \satisfy -> ifM (liftIO c)
	( satisfy
	, return NoChange
	)

-- | Tries the first property, but if it fails to work, instead uses
-- the second.
fallback :: (Combines (Property p1) (Property p2)) => Property p1 -> Property p2 -> Property (CInfo p1 p2)
fallback = combineWith $ \a1 a2 -> do
	r <- a1
	if r == FailedChange
		then a2
		else return r

-- | Marks a Property as trivial. It can only return FailedChange or
-- NoChange. 
--
-- Useful when it's just as expensive to check if a change needs
-- to be made as it is to just idempotently assure the property is
-- satisfied. For example, chmodding a file.
trivial :: Property i -> Property i
trivial p = adjustPropertySatisfy p $ \satisfy -> do
	r <- satisfy
	if r == MadeChange
		then return NoChange
		else return r

-- | Makes a property that is satisfied differently depending on the host's
-- operating system. 
--
-- Note that the operating system may not be declared for some hosts.
withOS :: Desc -> (Maybe System -> Propellor Result) -> Property NoInfo
withOS desc a = property desc $ a =<< getOS

-- | Undoes the effect of a property.
revert :: RevertableProperty -> RevertableProperty
revert (RevertableProperty p1 p2) = RevertableProperty p2 p1

makeChange :: IO () -> Propellor Result
makeChange a = liftIO a >> return MadeChange

noChange :: Propellor Result
noChange = return NoChange

doNothing :: Property NoInfo
doNothing = property "noop property" noChange

-- | Registers an action that should be run at the very end,
endAction :: Desc -> (Result -> Propellor Result) -> Propellor ()
endAction desc a = tell [EndAction desc a]
