module School (School, add, empty, grade, sorted) where

--data Student = Student (Int, String) deriving (Show, Eq)

--data School = School [(Int, String)] deriving (Show)

type Name = String
type Grade = Int
type Student = (Grade, Name)
type School = [Student]

joe = (6,"Joe") :: Student
sam = (5, "Sam") :: Student
beth = (2,"Beth") :: Student

sc = [joe, sam]

add :: Grade -> Name -> School -> School
add gradeNum name school = student : school
  where student = (gradeNum, name) :: Student
  --add gradeNum student school = empty

empty :: [Student]
empty = []

grade :: Grade -> School -> [Name]
grade gradeNum school = [(grade, name) | (grade, name) <- school, grade==gradeNum] 

sorted :: School -> [(Grade, [Name])]
sorted school = error "You need to implement this function."

main :: IO()
main = do
  print("*****")

{- SPECS
          let fromList = foldr (uncurry add) empty
          let fromGrade g = fromList . zip (repeat g)

          it "add student" $
            sorted (add 2 "Aimee" empty) `shouldBe` [(2, ["Aimee"])]

          it "add more students in same class" $
            sorted (fromGrade 2 ["James", "Blair", "Paul"])
            `shouldBe` [(2, ["Blair", "James", "Paul"])]

          it "add students to different grades" $
            sorted (fromList [(3, "Chelsea"), (7, "Logan")])
            `shouldBe` [(3, ["Chelsea"]), (7, ["Logan"])]

          it "empty list if no students" $
            sorted empty `shouldBe` []

          it "get students in a grade" $
            grade 5 (fromList [(5, "Franklin"), (5, "Bradley"), (1, "Jeff")])
            `shouldBe` ["Bradley", "Franklin"]

          it "get students in a non-existent grade" $
            grade 1 empty `shouldBe` []

          it "sorted school" $
            sorted (fromList [ (4, "Jennifer"   )
                             , (6, "Kareem"     )
                             , (4, "Christopher")
                             , (3, "Kyle"       ) ] )
            `shouldBe` [ (3, ["Kyle"                   ] )
                       , (4, ["Christopher", "Jennifer"] )
                       , (6, ["Kareem"                 ] ) ]
-}