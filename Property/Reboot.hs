module Property.Reboot where

import Common

now :: Property
now = cmdProperty "reboot" []
