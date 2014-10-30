-- |Properties for configuring firewall (iptables) rules
module Propellor.Property.Firewall where

import Data.Monoid
import Data.Char
import Data.List

import Propellor
import Utility.SafeCommand
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network

installed :: Property
installed = Apt.installed ["iptables"]

addRule :: Rule -> Property
addRule rule = property ("adding firewall rule: " <> show rule) addIpTable
  where
    addIpTable = liftIO $ do
      let r = toIpTable rule
      exist <- boolSystem "/sbin/iptables" (chk r)
      if exist then
        return NoChange
        else ifM (boolSystem "/sbin/iptables" (add r))
             ( return MadeChange , return FailedChange)
    add params = (Param "-A") : params
    chk params = (Param "-C") : params

toIpTable :: Rule -> [CommandParam]
toIpTable rule =  map Param ((show $ ruleChain rule) :
                             (toIpTableArg (ruleRules rule)) ++ [ "-j" , show $ ruleTarget rule ])

toIpTableArg :: Rules -> [String]
toIpTableArg NoRule = []
toIpTableArg (Proto proto)     = ["-p", map toLower $ show proto]
toIpTableArg (Port port)       = ["--dport", show port]
toIpTableArg (PortRange (f,t)) = ["--dport", show f ++ ":" ++ show t]
toIpTableArg (IFace iface)     = ["-i", show iface]
toIpTableArg (Ctstate states)  = ["-m", "conntrack","--ctstate", concat $ intersperse "," (map show states)]
toIpTableArg (r :- r')         = toIpTableArg r <> toIpTableArg r'

data Rule = Rule  {
  ruleChain :: Chain
  ,ruleTarget :: Target
  ,ruleRules :: Rules
  } deriving (Eq, Show, Read)
             
data Chain = INPUT | OUTPUT | FORWARD
           deriving (Eq,Show,Read)

data Target = ACCEPT | REJECT | DROP | LOG
            deriving (Eq,Show,Read)

data Proto = TCP | UDP | ICMP
           deriving (Eq,Show,Read)

type Port = Int

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
                     deriving (Eq,Show,Read)
                              
data Rules = NoRule
           | Proto Proto
             -- ^There is actually some order dependency between proto and port so this should be a specific
             -- data type with proto + ports
           | Port Port
           | PortRange (Port,Port)
           | IFace Network.Interface
           | Ctstate [ ConnectionState ]
           | Rules :- Rules   -- ^Combine two rules
          deriving (Eq,Show,Read)

infixl 0 :-

instance Monoid Rules where
  mempty = NoRule
  mappend = (:-)


