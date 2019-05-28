(* 目的：整数のペアのリスト（整数ベクトルのリスト）を受け取ったら、 すべてのペアを要素ごとに加えたペア（ベクトルの総和）を 返す関数 *)
(* add_pairs : (int * int) list -> int * int *)
let rec add_pairs lst = match lst with
    [] -> (0, 0)
  | (x, y) :: rest ->
    (* match add_pairs rest with
          (a, b) -> (x+a, y+b) *)
    let (a, b) = add_pairs rest in
    (x+a, y+b)

let add_pairs lst =
  (* let add (x, y) (a, b) = (x+a, y+b) in
     List.fold_right add lst (0, 0) *)
    List.fold_right (fun (x, y) (a, b) -> (x+a, y+b)) lst (0, 0)


(* テスト *)
let test1 = add_pairs [] = (0, 0)
let test2 = add_pairs [(2, 3)] = (2, 3)
let test3 = add_pairs [(1, 3); (2, 2); (4, 3)] = (7, 8)



(* 目的：ふたつの自然数を受け取ったら、その最大公約数を返す関数 *)
(* gcd : int -> int -> int *)
let rec gcd m n = if n=0 then m
                  else gcd n (m mod n)

(* テスト *)
let test1 = gcd 4 0 = 4
let test2 = gcd 18 12 = 6
let test3 = gcd 91 78 = 13
