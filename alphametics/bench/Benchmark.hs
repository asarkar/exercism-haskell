import qualified Alphametics as A
import qualified Alphametics2 as A2
import Criterion.Main

{-
Execute with 'stack --resolver lts --work-dir . \
--stack-yaml alphametics/stack.yaml bench'
-}
main :: IO ()
main =
  defaultMain
    [ bgroup "Alphametics" $
        map
          (\(name, eqn) -> bench name $ whnf A.solve eqn)
          testData,
      bgroup "Alphametics2" $
        map
          (\(name, eqn) -> bench name $ whnf A2.solve eqn)
          testData
    ]

testData :: [(String, String)]
testData =
  [ ("puzzle with three letters", "I + BB == ILL"),
    ("solution must have unique value for each letter", "A == B"),
    ("leading zero solution is invalid", "ACA + DD == BD"),
    ("puzzle with two digits final carry", "A + A + A + A + A + A + A + A + A + A + A + B == BCC"),
    ("puzzle with four letters", "AS + A == MOM"),
    ("puzzle with six letters", "NO + NO + TOO == LATE"),
    ("puzzle with seven letters", "HE + SEES + THE == LIGHT"),
    ("puzzle with eight letters", "SEND + MORE == MONEY"),
    ("puzzle with ten letters", "AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE"),
    ("puzzle with ten letters and 199 addends", "THIS + A + FIRE + THEREFORE + FOR + ALL + HISTORIES + I + TELL + A + TALE + THAT + FALSIFIES + ITS + TITLE + TIS + A + LIE + THE + TALE + OF + THE + LAST + FIRE + HORSES + LATE + AFTER + THE + FIRST + FATHERS + FORESEE + THE + HORRORS + THE + LAST + FREE + TROLL + TERRIFIES + THE + HORSES + OF + FIRE + THE + TROLL + RESTS + AT + THE + HOLE + OF + LOSSES + IT + IS + THERE + THAT + SHE + STORES + ROLES + OF + LEATHERS + AFTER + SHE + SATISFIES + HER + HATE + OFF + THOSE + FEARS + A + TASTE + RISES + AS + SHE + HEARS + THE + LEAST + FAR + HORSE + THOSE + FAST + HORSES + THAT + FIRST + HEAR + THE + TROLL + FLEE + OFF + TO + THE + FOREST + THE + HORSES + THAT + ALERTS + RAISE + THE + STARES + OF + THE + OTHERS + AS + THE + TROLL + ASSAILS + AT + THE + TOTAL + SHIFT + HER + TEETH + TEAR + HOOF + OFF + TORSO + AS + THE + LAST + HORSE + FORFEITS + ITS + LIFE + THE + FIRST + FATHERS + HEAR + OF + THE + HORRORS + THEIR + FEARS + THAT + THE + FIRES + FOR + THEIR + FEASTS + ARREST + AS + THE + FIRST + FATHERS + RESETTLE + THE + LAST + OF + THE + FIRE + HORSES + THE + LAST + TROLL + HARASSES + THE + FOREST + HEART + FREE + AT + LAST + OF + THE + LAST + TROLL + ALL + OFFER + THEIR + FIRE + HEAT + TO + THE + ASSISTERS + FAR + OFF + THE + TROLL + FASTS + ITS + LIFE + SHORTER + AS + STARS + RISE + THE + HORSES + REST + SAFE + AFTER + ALL + SHARE + HOT + FISH + AS + THEIR + AFFILIATES + TAILOR + A + ROOFS + FOR + THEIR + SAFE == FORTRESSES")
  ]

-- main :: IO ()
-- main = putStr "Hello"
