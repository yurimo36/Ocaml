(* 目的:n より小さい要素を取り出す *)
(* take_less : int -> int list -> int list *)
let take_less n lst =
  let less x = x < n in
  List.filter less lst
    
(* 目的:n より大きい要素を取り出す *)
(* take_greater: int -> int list -> int list *)
let take_greater n lst =
  let greater x = x > n in
  List.filter greater lst

(* 目的:受け取った整数のリストを昇順に並べる *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst = match lst with
    [] -> []
  | first :: rest ->
     quick_sort (take_less first rest)
      @ [first]
      @ quick_sort (take_greater first rest)

(* テスト *)
let test1 = quick_sort [] = []
let test2 = quick_sort [4; 2; 3] = [2; 3; 4]
let test3 = quick_sort [5; 5; 9; 8; 2; 3] = [2; 3; 5; 5; 8; 9]　(* 正しく整列できていない *)


(* 読み込むもの… metro.ml, type eki_t, make_eki_list2, shokika2 *)
#use "report8a.ml";;

(* 目的：起点のみ最短距離が 0 で他は infinity となっている 駅のリスト（eki_t list 型） と駅間リスト（ekikan_t list 型） を受け取ったら、 
ダイクストラ法を動かし、最終的に 「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリスト（eki_t list 型）を返す ような関数 *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main lst1 lst2 = if 


(* テスト *)
let test1 = dijkstra_main testA 
let test2 = dijkstra_main
let test3 = dijkstra_main


#use "toyo1.ml";;
#use "toyo2.ml";;
#use "tokyomonorail1.ml";;
#use "tokyomonorail2.ml";;

(* 下準備 *)
(* from report4.ml *)
let rec romaji_to_kanji2 ekimei lst = match lst with
    [] -> ("", "")
  | {kanji=a; kana=b; romaji=c; ken=d; shozoku=e} :: rest
    -> if c = ekimei then (a,d)
                     else romaji_to_kanji2 ekimei rest

(* from report5.ml *)
let rec seiretsu_insert lst x = match lst with
    [] -> [x]
    | ({kanji=a; kana=b; romaji=c; ken=d; shozoku=e} as first) :: rest
      -> match x with
      |  {kanji=f; kana=g; romaji=h; ken=i; shozoku=j}
         -> if (a=f) && (d=i) then first :: rest 
            else if (a>f) || ((a>f) && (d>i)) then  x :: (first :: rest)
            else first :: seiretsu_insert rest x
                            
let rec seiretsu2 lst = match lst with
	[] -> []
    |  first :: rest
      -> seiretsu_insert (seiretsu2 rest) first

let rec seek_eki2 eki lst = match lst with
        [] -> []
    |

(* 目的：起点の（ローマ字の）駅名と終点の（ローマ字の）駅名 と
駅名リスト（ekimei_t list 型）・ 駅間リスト（ekikan_t list 型） を受け取ったら、
romaji_to_kanji2 を使って起点と終点の駅名ペアを求め、 
受け取った駅名リストから seiretsu2 を使って重複を取り除き、
 make_eki_list2 と shokika2 （または make_initial_eki_list2）を使って駅のリスト（eki_t list 型）を作り、
dijkstra_main を使って、各駅までの最短路を確定し、
その中から終点の駅（eki_t 型）を探して返す関数 *)
(* dijkstra : string -> string -> ekimei_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra eki1 eki2 ekimei ekikan =
  dijkstra_main (shokika2 (make_eki_list2 (seiretsu2 (ekimei)) ) (romaji_to_kanji2 eki1 ekimei))  ekikan


(* テスト *)
let test1 = dijkstra "myogadani" "shibuya" global_ekimei_list global_ekikan_list 
let test2 = dijkstra "ikebukuro" "omotesando" global_ekimei_list global_ekikan_list
let test3 = dijkstra "nakano" "ichigaya" global_ekimei_list global_ekikan_list


(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
