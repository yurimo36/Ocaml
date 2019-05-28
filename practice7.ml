let rec map f lst = match lst with
    [] -> []
  | first :: rest -> f first :: map f rest

(* 目的：整数のリストを受け取ったら、 すべての要素を２倍したリストを返す関数 *)
(* double : int list -> int list *)
let time2 n = n * 2                     
let double lst = map time2 lst

(* テスト *)
let test1 = double [] = []
let test2 = double [3; -2; -1; 4; -5] = [6; -4; -2; 8; -10]
let test3 = double [0; 2; 7; 9] = [0; 4; 14; 18]



(* 目的：整数のリストを受け取ったら、その中の負の数をすべて 0 にしたリストを返す関数 *)
(* negative_zero : int list -> int list *)
let change_zero n = if n < 0 then 0 else n                    
let negative_zero lst = (*match lst with
    [] -> []
  | first :: rest -> if first < 0 then 0 :: negative_zero rest
                                  else first :: negative_zero rest*)
map change_zero lst


(* テスト *)
let test1 = negative_zero [] = []
let test2 = negative_zero [3; -2; -1; 4; -5] = [3; 0; 0; 4; 0]
let test3 = negative_zero [0; 2; -7; 9] = [0; 2; 0; 9]



(* 目的：整数のリストと数字 n を受け取ったら、 n のみ 0 にしたようなリストを返す関数 *)
(* num_zero : int list -> int list *)                
let num_zero lst n =
let zero x = if x = n then 0
                      else x
    in map zero lst

(* テスト *)
let test1 = num_zero [] 0 = []
let test2 = num_zero [3; 4; -1; 4; -5] 4  = [3; 0; -1; 0; -5]
let test3 = num_zero [0; 2; 7; 9] 7 = [0; 2; 0; 9]
