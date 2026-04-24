import           Copilot.Compile.C99
import           Copilot.Language         hiding (max, min, prop)
import           Copilot.Language.Prelude
import           Copilot.Library.LTL      (next)
import           Copilot.Library.MTL      hiding (alwaysBeen, since, trigger)
import qualified Copilot.Library.MTL      as MTL
import           Copilot.Library.PTLTL    (alwaysBeen, previous, since)
import qualified Copilot.Library.PTLTL    as PTLTL
import           Language.Copilot         (reify)
import           Language.Copilot         hiding (max, min)
import           Prelude                  hiding (max, min, mod, not, until,
                                           (&&), (++), (/=), (<), (<=), (==),
                                           (>), (>=), (||))

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

-- Initial state, final state, no transition signal, transitions, bad state
type StateMachineGF = ( Word8, Word8, Stream Bool, [(Word8, Stream Bool, Word8)], Word8)

stateMachineGF :: StateMachineGF -> Stream Word8
stateMachineGF (initialState, finalState, noInputData, transitions, badState) = state
  where
    state         = ifThenElses transitions
    previousState = [initialState] ++ state

    ifThenElses :: [(Word8, Stream Bool, Word8)] -> Stream Word8
    ifThenElses [] =
      ifThenElse (previousState == constant finalState && noInputData)
        (constant finalState)
        (constant badState)

    ifThenElses ((s1,i,s2):ss) =
      ifThenElse (previousState == constant s1 && i) (constant s2) (ifThenElses ss)
