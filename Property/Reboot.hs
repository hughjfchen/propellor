module Property.Reboot where

import Property

now :: Property
now = cmdProperty "reboot" []
