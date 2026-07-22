import           Copilot.Compile.C99
import           Copilot.Language              hiding (max, min, prop)
import           Copilot.Language.Prelude
import           Copilot.Library.LTL           (next)
import           Copilot.Library.MTL           hiding (alwaysBeen, since,
                                                trigger)
import qualified Copilot.Library.MTL           as MTL
import           Copilot.Library.PTLTL         (alwaysBeen, previous, since)
import qualified Copilot.Library.PTLTL         as PTLTL
import           Copilot.Library.StateMachines (stateMachine)
import           Language.Copilot              (reify)
import           Language.Copilot              hiding (max, min)
import           Prelude                       hiding (max, min, mod, not,
                                                until, (&&), (++), (/=), (<),
                                                (<=), (==), (>), (>=), (||))

externalState :: Stream Word8
externalState = extern "{{{state}}}" Nothing

input :: Stream Word8
input = extern "{{{input}}}" Nothing

{{{streamDefs}}}

-- | Complete specification. Calls C handler functions when properties are
-- violated.
spec :: Spec
spec = do
{{{triggers}}}

main :: IO ()
main = reify spec >>= compile "{{{specName}}}"
