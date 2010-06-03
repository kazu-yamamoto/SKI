module Test where

import Command
import Test.Framework (defaultMain, testGroup, Test, TestName)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

define :: IO ()
define = do
    def "I=SKK"
    def "B=S(KS)K"
    def "C=S(BBS)(KK)"
    def "T=CI"
    def "W=ST"
    def "M=SII"
    def "L=CBM"
    def "Q=CB"
    def "O=SI"
    def "U=LO"

testSpec :: [(String, String, String)]
testSpec = [
    ("I", "Ix",   "x")
  , ("B", "Bxyz", "(x(yz))")
  , ("C", "Cxyz", "((xz)y)")
  , ("T", "Txy",  "(yx)")
  , ("W", "Wxy",  "((xy)y)")
  , ("M", "Mx",   "(xx)")
  , ("L", "Lxy",  "(x(yy))")
  , ("Q", "Qxyz", "(y(xz))")
  , ("O", "Oxy",  "(y(xy))")
  , ("U", "Uxy",  "(y((xx)y))")
  ]

makeTest :: (TestName, String, String) -> Test
makeTest (label, expr, ret) = testCase label $ do
    define
    x <- eval' expr
    x @?= ret

tests :: [Test]
tests = [ testGroup "Combinator definition" (map makeTest testSpec)]

main :: IO ()
main = defaultMain tests
