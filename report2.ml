(* 目的：税抜き価格を受け取ったら、税込み価格を返す*)
(* shohizei: int -> int *)
let shohizei x = x * 108 / 100
(* テスト *)
let test1 = shohizei 100 = 108
let test2 = shohizei 98 = 105
let test3 = shohizei 10 = 10
(* 目的：チョコレートを買うための所持金が 500 円あるとして、チョコレートひとつの 税抜き の価格と 買いたいチョコレートの数を与えられたとき、 それだけの数のチョコレートを 買える場合には "買える" という文字列、 買えない場合には "買えない" という文字列 を返す *)
(* choco: int -> int -> string *)
let choco x y = if 500 - ( shohizei (x*y)) < 0 then "買えない"
    else "買える"
(* テスト *)
let test4 = choco 92 5 = "買える"
let test5 = choco 93 5 = "買えない"
let test6 = choco 463 1 = "買える"
