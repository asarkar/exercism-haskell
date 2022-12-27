module School (School, add, empty, grade, sorted) where

import qualified Data.List as L
import qualified Data.Map as M

newtype School = School (M.Map Int [String])

add :: Int -> String -> School -> School
add gradeNum student (School roster) = School $ M.insertWith (++) gradeNum [student] roster

empty :: School
empty = School M.empty

grade :: Int -> School -> [String]
grade gradeNum (School roster) = L.sort students
  where
    students = M.findWithDefault [] gradeNum roster

sorted :: School -> [(Int, [String])]
sorted s@(School roster) = map (\g -> (g, grade g s)) grades
  where
    grades = M.keys roster
