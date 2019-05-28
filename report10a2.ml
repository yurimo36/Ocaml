(* report10a.ml *)

(* 型 tree_t *)
type ('a, 'b) tree_t = Empty
                     | Node of ('a, 'b) tree_t * 'a * 'b * ('a, 'b) tree_t

let tree0 = Empty
let tree1 = Node (tree0, 'a', 1, tree0)
let tree2 = Node (tree0, 'b', 2, tree0) 
let tree3 = Node (tree0, 'c', 3, tree0)

(* 問1 *)
(* 関数 insert *)
(* 目的 : ('a, 'b) tree_t 型の木と 'a 型のキー、'b 型の値を受け取ったら、
         そのキーと値を挿入した木を返す *)
(* insert : ('a, 'b) tree_t -> 'a -> 'b -> ('a, 'b) tree_t *)
let rec insert tree x y = match tree with
    Empty -> Node (Empty, x, y, Empty)
  | Node (left, a, b, right)
    -> if x = a then Node (left, x, y, right)
    else if x < a then Node (insert left x y, a, b, right)
    else Node (left, a, b, insert right x y)


(* テスト *)
let test1011 = insert tree0 'a' 1 
let test1012 = insert tree1 'a' 1 
let test1013 = insert tree1 'b' 2
let test1014 = insert tree3 'b' 2



(* 問2 *)
(* 関数 inserts *)
(* 目的 : キーと値をひとつではなく「キーと値のペア」をリストで受け取り、 
   それを順に挿入した木を返す *)
(* inserts : ('a, 'b) tree_t -> ('a * 'b) list -> ('a, 'b) tree_t *)
let rec inserts tree lst =
  match lst with
    [] -> tree
  | first :: rest -> match first with (x, y) -> 
    let newtree = insert tree x y
    in inserts newtree rest
            
(* テスト *)
let test1021 = inserts tree0 [('a', 1)]
let test1022 = inserts tree1 [('a', 1)]
let test1023 = inserts tree1 [('b', 2)]
let test1024 = inserts tree3 [('b', 2)] 


(* 問3 *)
(* 関数 search *)
(* 目的 : ('a, 'b) tree_t 型の木と 'a 型のキーを 受け取ったら、対応する値を返す *)
(* search : ('a, 'b) tree_t -> 'a -> 'b *)
let rec search tree x = match tree with
    Empty -> 0
  | Node (left, a, b, right) ->
    if x = a then b
    else if x < a then search left x
    else search right x

(* テスト *)
let test1031 = search tree0 'a'
let test1032 = search tree1 'a'
let test1033 = search (Node (tree2, 'a', 1, tree3)) 'c'
let test1034 = search (Node (tree2, 'a', 1, tree3)) 'd'
