
import Test.QuickCheck.Simple

int1 :: Int
int1 = 1

stringHello :: String
stringHello = "Hello"

prop_int1 :: Bool
prop_int1 = int1 == 1

prop_stringHelloBad :: Bool
prop_stringHelloBad = stringHello == "Hellox"

prop_intComBad :: Int -> Int -> Bool
prop_intComBad i j = i + j == j + i + 1

prop_intCom2Bad :: Int -> Int -> Bool
prop_intCom2Bad i j = i + j == j + i + 2

tests :: [Test]
tests =
  [ boolTest "int1"             prop_int1
  , boolTest' "stringHelloBad"  "Hello =/= Hellox" prop_stringHelloBad
  , qcTest   "intComBad"        prop_intComBad
  , qcTest   "intCom2Bad"       prop_intCom2Bad
  ]

main :: IO ()
main = defaultMain' True tests
