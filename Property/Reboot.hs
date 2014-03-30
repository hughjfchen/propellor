module Property.Reboot where

import Property
import Utility.SafeCommand

now -> Property
now = cmdProperty "reboot" []
