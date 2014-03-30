module Property.Reboot where

import Property
import Utility.SafeCommand

{- Use eg, "+5" to reboot in 5 minutes. -}
scheduled :: String -> Property
scheduled rebootwhen = cmdProperty "shutdown" [ Param "-r", Param rebootwhen ]
