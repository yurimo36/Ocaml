(* 目的：係数 a, b, c を 与えられたら、判別式の値を返す *)
(* hanbetsushiki : int -> int -> int -> int *)
let hanbetsushiki a b c = b*b - 4*a*c 
(* テスト *)
let test1 = hanbetsushiki 1 2 3 = -8
let test2 = hanbetsushiki 1 6 9 = 0
let test3 = hanbetsushiki 2 5 1 = 17


(* 目的：係数 a, b, c を 与えられたら、解の個数を整数で返す *)
(* kai_no_kosuu : int -> int -> int -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0 then 2
  else if hanbetsushiki a b c = 0 then 1
  else 0
(* テスト *)
let test4 = kai_no_kosuu 1 2 3 = 0
let test5 = kai_no_kosuu 1 6 9 = 1
let test6 = kai_no_kosuu 2 5 1 = 2


(* 目的：身長 (m) と体重 (kg) を与えられたら、その人の bmi を計算し 体型を文字列で返す *)
(* taikei : float -> float -> string *)
let bmi x y = y /. ( x*.x )
let taikei x y = if bmi x y < 18.5 then "低体重"
  else if bmi x y >= 25.0 then "肥満"
    else "普通"
(* テスト *)
let test1 = taikei 1.5 45. = "普通"
let test2 = taikei 1.5 40. = "低体重" 
let test3 = taikei 1.5 57. = "肥満"
