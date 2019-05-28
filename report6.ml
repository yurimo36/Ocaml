(* 候補： #use "metro.ml" or "higashi.ml" or "global.ml" or "hokkaido.ml" *)
#use "metro.ml"
(* 駅名ペア（漢字の駅名の文字列と県名の文字列のペア）namae、 最短距離（実数）saitan_kyori、 駅名ペア（漢字の文字列と県名の文字列のペア）のリスト temae_list の ３つをフィールドとして持つレコード *)
type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
};;


(* 目的：ekimei_list を受け取ったら、その駅名を使って eki_t 型の リストを作る関数 *)
(* make_eki_list2 : ekimei_t list -> eki_t list *)
let rec make_eki_list2 lst = match lst with
    [] -> []
  | {kanji=a; kana=b; romaji=c; ken=d; shozoku=e} :: rest -> {namae=(a,d); saitan_kyori=infinity; temae_list=[]} :: make_eki_list2 rest 


(* テスト *)
let test = make_eki_list2 global_ekimei_list



(* 目的：eki_t 型のリストと起点の駅名ペアを受け取ったら、 起点のみsaitan_kyoriを 0. に、temae_list を起点の駅名ペアのみからなるリストにし、 起点以外はもとと同じであるような駅のリスト （要素が eki_t 型のリスト） を返す関数 *)
(* shokika2 : eki_t list -> list *)
let rec shokika2 lst (x, y) = match lst with
    [] -> []
  | {namae=(a, b); saitan_kyori=c; temae_list=d} :: rest ->
    if a = x && b = y
    then {namae=(a, b); saitan_kyori=0.; temae_list=[(a,b)]} :: shokika2 rest (x, y)
    else {namae=(a, b); saitan_kyori=c; temae_list=d} :: shokika2 rest (x, y)

(* テスト *)
let test1 = shokika2 (make_eki_list2 global_ekimei_list) ("池袋", "東京")
