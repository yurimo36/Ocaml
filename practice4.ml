(* 目的：整数のリストと数字をひとつ受け取ったら、その数字がリストの中に入っているかを返す関数 *)
(* contain : int list -> int -> bool *)
let rec contain lst x = match lst with
    [] -> false
  | first :: rest ->
    if first = x then true
      else contain rest x

(* テスト *)
let test1 = contain [] 0  = false
let test2 = contain [1; 2; 3] 4  = false
let test3 = contain [0; 1] 1 = true


(* 目的：整数のリストと数字をひとつ受け取ったら、その数字がリストの中にいくつ入っているかを返す関数 *)
(* count : int list -> int ->int *)
let rec count lst x = match lst with
    [] -> 0
  | first :: rest ->
    if first = x then 1 + count rest x
      else count rest x

(* テスト *)
let test1 = count [] 5 = 0
let test2 = count [1; 2; 3] 2 = 1 
let test3 = count [1; 2; 3; 1; 2; 3] 3 = 2


(*let rec sum lst = match lst with
    [] -> 0
  | first :: rest ->
    if first = first + sum rest*)
