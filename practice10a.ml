(* 木を表す型 *)
type tree_t = Empty | Node of tree_t * int * tree_t

(* 木の例 *)
let tree1 = Node (Empty, 3, Empty)
let tree2 = Node (Empty, 1, Empty)
let tree3 = Node (tree1, 5, tree2)



(* 目的:木の中の整数の合計を返す関数 *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Node (l, n, r) -> sum_tree l + n + sum_tree r

(* テスト *)
let test1 = sum_tree Empty = 0
let test2 = sum_tree tree1 = 3
let test3 = sum_tree tree3 = 9



(* 目的：tree_t 型の木を受け取ったら、 すべての要素を２倍にした木を返す関数 *)
(* double_tree : tree_t -> tree_t *)
let rec double_tree tree = match tree with
    Empty -> Empty
  | Node (left, n, right) ->
    Node (double_tree left, 2 * n, double_tree right)

(* テスト *)
let test1 = double_tree Empty = Empty
let test2 = double_tree tree1 = Node (Empty, 6, Empty)
let test3 = double_tree tree2 = Node (Empty, 2, Empty)
let test4 = double_tree tree3 = Node (Node (Empty, 6, Empty), 10, Node (Empty, 2, Empty))
