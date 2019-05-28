(* report09a.ml *)

(* 問1 *)
(* クイックソート *)
(* 目的 : 受け取った整数のリストを昇順に並べる *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst = match lst with
    [] -> []
  | first :: rest ->

    (* 目的 : nより小さい要素を取り出す *)
    (* take_less : int -> int list -> int list *)
    let take_less n lst =
      let less x = x < n in List.filter less lst
        
    (* 目的 : nより大きい要素を取り出す *)
    (* take_greater : int -> int list -> int list *)
    in let take_greater n lst =
         let greater x = x > n
         in List.filter greater lst

    (* 目的 : nと等しい要素を取り出す *)
    (* take_equal : int -> int list -> int list *)
    in let take_equal n lst =
         let equal x = x = n
         in List.filter equal lst


    in quick_sort (take_less first rest)
       @(take_equal first rest)
       @[first]
       @quick_sort (take_greater first rest)

(* テスト *)
let test0911 = quick_sort [4; 9; 8; 2; 3] = [2; 3; 4; 8; 9]
let test0912 = quick_sort [2; 2; 3; 3; 2]
let test0913 = quick_sort [2; 3; 1; 1; 4; 4; 1; 2]
