(* 目的：整数のリストを受け取ったら、 その中の負の数をすべて 0 にしたリストを返す関数 *)
(* negative_zero : int list -> int list *)
let rec negative_zero lst = match lst with
   	[] -> []
  | first :: rest -> if first < 0 then 0 :: negative_zero rest
  	else first :: negative_zero rest
 

(* テスト *)
let test1 = negative_zero [] = []
let test2 = negative_zero [1; 2; -3; 4; -5] = [1; 2; 0; 4; 0]
let test3 = negative_zero [-2; -1; 0; 9] = [0; 0; 0; 9]



(* 目的：整数のリストを受け取ったら、 その中の要素にすべて 1 を加えたリストを返す関数 *)
(* add1_list : int list -> int ist *)
let rec add1_list lst = match lst with
    [] -> []
  | first :: rest -> first+1 :: add1_list rest


(* テスト *)
let test1 = add1_list [] = [] 
let test2 = add1_list [3; 2; 1; 4; 5] = [4; 3; 2; 5; 6]
let test3 = add1_list [-4; -3; -2; -1; 0] = [-3; -2; -1; 0; 1]
