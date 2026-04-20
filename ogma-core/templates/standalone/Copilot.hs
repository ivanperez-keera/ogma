import           Copilot.Compile.C99
import           Copilot.Language          hiding (prop)
import           Copilot.Language.Prelude
import           Copilot.Library.LTL       (next)
import           Copilot.Library.MTL       hiding (since, alwaysBeen, trigger)
import           Copilot.Library.PTLTL     (since, previous, alwaysBeen)
import qualified Copilot.Library.PTLTL     as PTLTL
import qualified Copilot.Library.MTL       as MTL
import           Language.Copilot          (reify)
import           Prelude                   hiding ((&&), (||), (++), (<=), (>=), (<), (>), (==), (/=), not)

{{#copilot_extra_defs}}
{{{.}}}
{{/copilot_extra_defs}}
{{{externs}}}
{{{internals}}}
{{{reqs}}}

-- | Clock that increases in one-unit steps.
clock :: Stream Int64
clock = [0] ++ (clock + 1)

-- | First Time Point
ftp :: Stream Bool
ftp = [True] ++ false

pre :: Stream Bool -> Stream Bool
pre = ([False] ++)

tpre :: Stream Bool -> Stream Bool
tpre = ([True] ++)

notPreviousNot :: Stream Bool -> Stream Bool
notPreviousNot = not . PTLTL.previous . not

-- Initial state, final state, no transition signal, transitions, bad state
type StateMachine a = (a, a, Stream Bool, [(a, Stream Bool, a)], a)

stateMachine :: (Eq a, Typed a)
             => StateMachine a
             -> Stream a
stateMachine (initial, final, noInputData, transitions, bad) =
    state
  where
    state         = ifThenElses transitions
    previousState = [initial] ++ state

    -- ifThenElses :: [(a, Stream Bool, a)] -> Stream a
    ifThenElses [] =
      ifThenElse
        (previousState == constant final && noInputData)
        (constant final)
        (constant bad)

    ifThenElses ((s1, i, s2):ss) =
      ifThenElse
        (previousState == constant s1 && i)
        (constant s2)
        (ifThenElses ss)

-- | Complete specification. Calls C handler functions when properties are
-- violated.
spec :: Spec
spec = do
{{{triggers}}}

main :: IO ()
main = reify spec >>= compile "{{{specName}}}"
