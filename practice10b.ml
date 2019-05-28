(* ２分木を表す型 *)
type tree_t = Empty | Node of tree_t * char * int * tree_t

(* ２分木の例 *)
let tree1 = Node (Empty, 'a', 3, Empty)
let tree2 = Node (Empty, 'e', 1, Empty)
let tree3 = Node (tree1, 'c', 5, tree2)



(* 目的：tree_t 型の木と文字 c を受け取ったら、対応する数字を返す関数 *)
(* search : tree_t -> char -> tree_t *)
let rec search tree c = match tree with
    Empty -> -1
  | Node (left, ch, n, right) ->
    if c < ch then search left c
    else if ch < c then search right c
    else n

(* テスト *)
let test1 = search Empty 'a' = -1
let test2 = search tree3 'a' = 3            
let test3 = search tree3 'b' = -1
let test4 = search tree3 'c' = 5
