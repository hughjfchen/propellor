{-# LANGUAGE PackageImports #-}

module Propellor.Property where

import System.Directory
import Control.Monad
import Data.Monoid
import Control.Monad.IfElse
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Types.Attr
import Propellor.Engine
import Utility.Monad

makeChange :: IO () -> Propellor Result
makeChange a = liftIO a >> return MadeChange

noChange :: Propellor Result
noChange = return NoChange

-- | Combines a list of properties, resulting in a single property
-- that when run will run each property in the list in turn,
-- and print out the description of each as it's run. Does not stop
-- on failure; does propigate overall success/failure.
propertyList :: Desc -> [Property] -> Property
propertyList desc ps = Property desc $ ensureProperties ps

-- | Combines a list of properties, resulting in one property that
-- ensures each in turn, stopping on failure.
combineProperties :: Desc -> [Property] -> Property
combineProperties desc ps = Property desc $ go ps NoChange
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
p1 `before` p2 = Property (propertyDesc p1) $ do
	r <- ensureProperty p1
	case r of
		FailedChange -> return FailedChange
		_ -> ensureProperty p2

-- | Makes a perhaps non-idempotent Property be idempotent by using a flag
-- file to indicate whether it has run before.
-- Use with caution.
flagFile :: Property -> FilePath -> Property
flagFile property flagfile = Property (propertyDesc property) $
	go =<< liftIO (doesFileExist flagfile)
  where
	go True = return NoChange
	go False = do
		r <- ensureProperty property
		when (r == MadeChange) $ liftIO $ 
			unlessM (doesFileExist flagfile) $
				writeFile flagfile ""
		return r

--- | Whenever a change has to be made for a Property, causes a hook
-- Property to also be run, but not otherwise.
onChange :: Property -> Property -> Property
property `onChange` hook = Property (propertyDesc property) $ do
	r <- ensureProperty property
	case r of
		MadeChange -> do
			r' <- ensureProperty hook
			return $ r <> r'
		_ -> return r

(==>) :: Desc -> Property -> Property
(==>) = flip describe
infixl 1 ==>

-- | Makes a Property only be performed when a test succeeds.
check :: IO Bool -> Property -> Property
check c property = Property (propertyDesc property) $ ifM (liftIO c)
	( ensureProperty property
	, return NoChange
	)

boolProperty :: Desc -> IO Bool -> Property
boolProperty desc a = Property desc $ ifM (liftIO a)
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
host hn = Host [] (\_ -> newAttr hn)

-- | Adds a property to a Host
-- Can add Properties, RevertableProperties, and AttrProperties
(&) :: IsProp p => Host -> p -> Host
(Host ps as) & p = Host (ps ++ [toProp p]) (getAttr p . as)

infixl 1 &

-- | Adds a property to the Host in reverted form.
(!) :: Host -> RevertableProperty -> Host
(Host ps as) ! p = Host (ps ++ [toProp q]) (getAttr q . as)
  where
	q = revert p

infixl 1 !
