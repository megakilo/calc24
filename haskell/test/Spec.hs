import Calc24 (calc24)
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.Interpreter as Hint
import Test.Hspec

evalResult :: Maybe String -> IO (Maybe Double)
evalResult Nothing = return Nothing
evalResult (Just s) = do
  r <- Hint.runInterpreter $ do
    Hint.setImports ["Prelude"]
    Hint.interpret s (Hint.as :: Double)
  return $ case r of
    Left _ -> Nothing
    Right x -> Just x

main :: IO ()
main = hspec $ do
  describe "calc24" $ do
    it "has valid solution for [2, 3, 4]." $ do
      evalResult (calc24 [2, 3, 4]) `shouldReturn` Just 24
    it "has no solution for [1, 3, 4]." $ do
      evalResult (calc24 [1, 3, 4]) `shouldReturn` Nothing
    it "has valid solution for [1, 2, 3, 4]." $ do
      evalResult (calc24 [1, 2, 3, 4]) `shouldReturn` Just 24
    it "has valid solution for [3, 3, 7, 7] with floating point calculation." $ do
      evalResult (calc24 [3, 3, 7, 7]) `shouldReturn` Just 24
