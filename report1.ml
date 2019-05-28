(* 目的：所持金が与えられたら126円のチョコレートがいくつ買えるかを求める *)
(* chocolate : int -> int *)
let chocolate money = money / 126
(* テスト *)
let test1 = (chocolate 100 = 0)
let test2 = (chocolate 378 = 3)
let test3 = (chocolate 1000 = 7)


(* 目的：文字列が与えられたらその名前を使って「こんにちは、○○さん。」と返す*)
(* aisatsu : string -> string *)
let aisatsu name = "こんにちは、" ^ name ^ "さん。"
(* テスト *)
let test4 = (aisatsu "ゆりこ" = "こんにちは、ゆりこさん。")
let test5 = (aisatsu "ココア" = "こんにちは、ココアさん。")
let test6 = (aisatsu "くう" = "こんにちは、くうさん。")



(* 目的：身長と体重が与えられたらBMI指数を返す*)
(* bmi : float -> float -> float *)
let bmi x y = y /. ( x *. x )
(* テスト *)
let test7 = (bmi 1.5 45. = 20.)
let test8 = (bmi 1.5 42.75 = 19.)
let test9 = (bmi 2. 78. = 19.5)
