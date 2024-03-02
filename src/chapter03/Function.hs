-- パターンマッチ
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

tell :: (Show a) => [a] -> String -- (Show a)は型クラス制約。Show型の任意の型aを使うということ。
tell [] = "The list is empty"
tell (x : []) = "The list has one element: " ++ show x
tell (x : y : []) = "The list has two element: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- as
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x : _) = "The first letter of " ++ all ++ " is " ++ [x]

-- ガード
-- 引数の構造で場合分けはパターン
-- 引数の値で場合分けはガード
bmiTell :: Double -> Double -> String
bmiTell 0 0 = "no"
bmiTell weight height
  | bmi <= skinny = "You're underweight"
  | bmi <= normal = "You're supposedly normal"
  | bmi <= fat = "You're fat"
  | otherwise = "You're fat"
 where
  -- whereのスコープはそのパターンのみ。bmiTell 0 0 のパターンでは使えない
  bmi = weight / height ^ 2
  (skinny, normal, fat) = (18.5, 25.0, 30.0) -- whereの中でもパターンマッチができる

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
 where
  bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
