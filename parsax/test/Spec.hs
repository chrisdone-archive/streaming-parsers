import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Parsax
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Empty stream"
    (it
       "Empty input"
       (shouldBe (runConduitPure (CL.sourceList [] .| objectSink (Pure ())))
                 ()))
