-- | Metamodule exporting all of our EVE interface
module EVE
(module EVE.Monad
,module EVE.Query
) where

import EVE.Query
import EVE.Monad
