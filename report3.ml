(* 目的：整数の組として表された xy 平面上の格子点をふたつ受け取り、その ２点間のマンハッタン距離を求める関数 *)
(* manhattan : int * int -> int * int -> int *)
let manhattan x y = match (x,y) with
( (a,b) , (c,d) ) -> abs(a-c) + abs(b-d)

(* テスト *)
let test1 = manhattan (3,4) (1,2) = 4
let test2 = manhattan (2,5) (6,7) = 6
let test3 = manhattan (0,0) (0,0) = 0
                        

(* 駅名の情報を格納するレコード *)
type ekimei_t = {
  kanji : string;
  kana : string;
  romaji : string;
  ken : string;
  shozoku : string;
} ;;

let ekimei1 = {kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; ken = "東京"; shozoku = "丸ノ内線"}
let ekimei2 = {kanji = "護国寺"; kana = "ごこくじ"; romaji = "gokokuji"; ken = "東京"; shozoku = "有楽町線"}
let ekimei3 = {kanji = "中野"; kana = "なかの"; romaji = "nakano"; ken = "東京"; shozoku = "東西線"}


(* 目的：ekimei_t 型のデータを受け取ってきたら 「県名：路線名、駅名（かな）」 の形式の文字列を返す関数 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei = match ekimei with
    { kanji=kanji; kana=kana; romaji=romaji; ken=ken; shozoku=shozoku; } ->
    ken^ "：" ^shozoku^ "、" ^kanji^ "（" ^kana^ "）"

(* テスト *)
let test1 = hyoji ekimei1 = "東京：丸ノ内線、茗荷谷（みょうがだに）"
let test2 = hyoji ekimei2 = "東京：有楽町線、護国寺（ごこくじ）"
let test3 = hyoji ekimei3 = "東京：東西線、中野（なかの）"


(* 駅と駅の接続情報を格納するレコード *)
type ekikan_t = {
  kiten : string;
  kenk : string;
  shuten : string;
  kens : string;
  keiyu : string;
  kyori : float;
  jikan : int;
} ;;
