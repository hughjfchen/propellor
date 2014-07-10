{-# LANGUAGE PackageImports #-}

module Propellor.Property where

import System.Directory
import Control.Monad
import Data.Monoid
import Control.Monad.IfElse
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Info
import Propellor.Engine
import Utility.Monad
import System.FilePath

-- Constructs a Property.
property :: Desc -> Propellor Result -> Property
property d s = Property d s mempty

-- | Combines a list of properties, resulting in a single property
-- that when run will run each property in the list in turn,
-- and print out the description of each as it's run. Does not stop
-- on failure; does propigate overall success/failure.
propertyList :: Desc -> [Property] -> Property
propertyList desc ps = Property desc (ensureProperties ps) (combineInfos ps)

-- | Combines a list of properties, resulting in one property that
-- ensures each in turn. Does not stop on failure; does propigate
-- overall success/failure.
combineProperties :: Desc -> [Property] -> Property
combineProperties desc ps = Property desc (go ps NoChange) (combineInfos ps)
  where
  	go [] rs = return rs
	go (l:ls) rs = do
		r <- ensureProperty l
		case r of
			FailedChange -> return FailedChange
			_ -> go ls (r <> rs)

-- | Combines together two properties, resulting in one property
-- that ensures the first, and if the first succeeds, ensures the second.
-- The property uses the description of the first property.
before :: Property -> Property -> Property
p1 `before` p2 = p2 `requires` p1
	`describe` (propertyDesc p1)

-- | Makes a perhaps non-idempotent Property be idempotent by using a flag
-- file to indicate whether it has run before.
-- Use with caution.
flagFile :: Property -> FilePath -> Property
flagFile p = flagFile' p . return

flagFile' :: Property -> IO FilePath -> Property
flagFile' p getflagfile = adjustProperty p $ \satisfy -> do
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

--- | Whenever a change has to be made for a Property, causes a hook
-- Property to also be run, but not otherwise.
onChange :: Property -> Property -> Property
p `onChange` hook = Property (propertyDesc p) satisfy (combineInfo p hook)
  where
	satisfy = do
		r <- ensureProperty p
		case r of
			MadeChange -> do
				r' <- ensureProperty hook
				return $ r <> r'
			_ -> return r

(==>) :: Desc -> Property -> Property
(==>) = flip describe
infixl 1 ==>

-- | Makes a Property only need to do anything when a test succeeds.
check :: IO Bool -> Property -> Property
check c p = adjustProperty p $ \satisfy -> ifM (liftIO c)
	( satisfy
	, return NoChange
	)

-- | Marks a Property as trivial. It can only return FailedChange or
-- NoChange. 
--
-- Useful when it's just as expensive to check if a change needs
-- to be made as it is to just idempotently assure the property is
-- satisfied. For example, chmodding a file.
trivial :: Property -> Property
trivial p = adjustProperty p $ \satisfy -> do
	r <- satisfy
	if r == MadeChange
		then return NoChange
		else return r

doNothing :: Property
doNothing = property "noop property" noChange

-- | Makes a property that is satisfied differently depending on the host's
-- operating system. 
--
-- Note that the operating system may not be declared for some hosts.
withOS :: Desc -> (Maybe System -> Propellor Result) -> Property
withOS desc a = property desc $ a =<< getOS

boolProperty :: Desc -> IO Bool -> Property
boolProperty desc a = property desc $ ifM (liftIO a)
	( return MadeChange
	, return FailedChange
	)

-- | Undoes the effect of a property.
revert :: RevertableProperty -> RevertableProperty
revert (RevertableProperty p1 p2) = RevertableProperty p2 p1

-- | Starts accumulating the properties of a Host.
--
-- > host "example.com"
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
host :: HostName -> Host
host hn = Host hn [] mempty

-- | Adds a property to a Host
--
-- Can add Properties and RevertableProperties
(&) :: IsProp p => Host -> p -> Host
(Host hn ps as) & p = Host hn (ps ++ [toProp p]) (as <> getInfo p)

infixl 1 &

-- | Adds a property to the Host in reverted form.
(!) :: Host -> RevertableProperty -> Host
h ! p = h & revert p

infixl 1 !

-- Changes the action that is performed to satisfy a property. 
adjustProperty :: Property -> (Propellor Result -> Propellor Result) -> Property
adjustProperty p f = p { propertySatisfy = f (propertySatisfy p) }

-- Combines the Info of two properties.
combineInfo :: (IsProp p, IsProp q) => p -> q -> Info
combineInfo p q = getInfo p <> getInfo q

combineInfos :: IsProp p => [p] -> Info
combineInfos = mconcat . map getInfo

makeChange :: IO () -> Propellor Result
makeChange a = liftIO a >> return MadeChange

noChange :: Propellor Result
noChange = return NoChange
