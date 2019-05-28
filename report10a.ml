(* ２分木を表す型 *)
type ('a, 'b) tree_t = Empty
                     | Node of ('a, 'b) tree_t * 'a * 'b * ('a, 'b) tree_t

(* ２分木の例 *)
let tree1 = Node (Empty, 'a', 3, Empty)
let tree2 = Node (Empty, 'e', 1, Empty)
let tree3 = Node (tree1, 'c', 5, tree2)



(* 目的：('a, 'b) tree_t 型の木と 'a 型のキー、'b 型の値を受け取ったら、
そのキーと値を挿入した木を返す関数 *)
(* insert : ('a, 'b) tree_t -> 'a -> 'b -> ('a, 'b) tree_t *)
let rec insert tree x y = match tree with
    Empty -> Node (Empty, x ,y ,Empty)

  | Node (left, a, b, right) ->
    if x < a then Node (left, a, b, insert right x y) 
    else if x > a then Node (insert left x y, a, b, right)
    else (* x = a *) Node (left, x, y, right)

(* テスト *)
let test1 = insert Empty 'a' 1
let test2 = insert tree1 'a' 3           
let test3 = insert tree2 'b' 4
let test4 = insert tree3 'f' 8 



(* 目的 : キーと値をひとつではなく「キーと値のペア」をリストで受け取り、 
   それを順に挿入した木を返す *)
(* inserts : ('a, 'b) tree_t -> ('a * 'b) list -> ('a, 'b) tree_t *)
let rec inserts tree lst =
  match lst with
    [] -> tree
    | first :: rest ->
    match first with
    