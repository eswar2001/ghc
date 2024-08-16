module GHC.Types.Name.Cache where

import GHC.Prelude (Maybe)
import GHC.Unit.Module (Module)
import GHC.Types.Name (Name, OccName)

isKnownOrigName_maybe :: Module -> OccName -> Maybe Name