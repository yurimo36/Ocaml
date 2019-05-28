#use "metro.ml";;

type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
};;

let rec map f lst = match lst with
    [] -> []
  | first :: rest -> f first :: map f rest


(* 目的：ekimei_list を受け取ったら、その駅名を使って eki_t 型の リストを作る関数 *)
(* make_eki_list2 : ekimei_t list -> eki_t list *)
let make_eki_list2 lst =
  let make_eki_list1 {kanji=a; kana=b; romaji=c; ken=d; shozoku=e} =
    {namae=(a,d); saitan_kyori=infinity; temae_list=[]}
  in map make_eki_list1 lst

(* テスト *)
let test = make_eki_list2 global_ekimei_list



(* 目的：eki_t 型のリストと起点の駅名ペアを受け取ったら、 起点のみsaitan_kyoriを 0. に、temae_list を起点の駅名ペアのみからなるリストにし、 起点以外はもとと同じであるような駅のリスト （要素が eki_t 型のリスト） を返す関数 *)
(* shokika2 : eki_t list -> string * string -> eki_t list *)
let shokika2 lst (x, y) =
  let shokika1 {namae=(a, b); saitan_kyori=c; temae_list=d} =
    if a = x && b = y
    then {namae=(a, b); saitan_kyori=0.; temae_list=[(a,b)]}
    else {namae=(a, b); saitan_kyori=c; temae_list=d}
  in map shokika1 lst

(* テスト *)
let test = shokika2 (make_eki_list2 global_ekimei_list) ("渋谷", "東京")



(* 目的：make_eki_list2 の処理と shokika2 の処理を一度にやってしまう関数 *)
(* make_initial_eki_list2 : ekimei_list -> string * string -> eki_t list *)
let make_initial_eki_list2 lst (x, y) =
    shokika2 (make_eki_list2 lst) (x, y)

                                    
(* テスト *)
let test1 = make_initial_eki_list2 global_ekimei_list ("渋谷", "東京") 
let test2 = make_initial_eki_list2 global_ekimei_list ("銀座", "東京") 
