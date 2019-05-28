(* 候補： #use "metro.ml" or "higashi.ml" or "global.ml" or "hokkaido.ml" *)
#use "metro.ml"


(* 目的：リストを受け取ったら、そのリストの長さを返す関数 *)
(* length : list -> int *)           
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

(* テスト *)
let test1 = length [] = 0    
let test2 = length [7; 7; 7] = 3
let test3 = length [4; 1; 3; 5; 8; 9] = 6
(* 実験 
let test1 = length global_ekimei_list 
let test2 = length global_ekikan_list *)



(* 目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記（文字列）と県名（文字列）のペアを返す関数 *)
(* romaji_to_kanji2 : string -> list -> string * string *)
let rec romaji_to_kanji2 ekimei lst = match lst with
    [] -> ("", "")
  | {kanji=a; kana=b; romaji=c; ken=d; shozoku=e} :: rest
    -> if c = ekimei then (a,d)
                     else romaji_to_kanji2 ekimei rest

(* テスト *)
let test1 = romaji_to_kanji2 "myogadani" global_ekimei_list = ("茗荷谷", "東京")
let test2 = romaji_to_kanji2 "urayasu" global_ekimei_list = ("浦安", "千葉")
let test3 = romaji_to_kanji2 "wakoshi" global_ekimei_list = ("和光市", "埼玉")            



(* 目的：駅名ペアをふたつと駅間リストを受け取って来たら、 駅間リストの中からその２駅（ペア）間の距離を返す関数 *)
(* get_ekikan_kyori2 : string * string -> string * string -> list -> float *)
let rec get_ekikan_kyori2 ( eki1, ken1 ) ( eki2, ken2 ) lst = match lst with
    [] -> infinity
  | {kiten=a; kenk=b; shuten=c; kens=d; keiyu=e; kyori=f; jikan=g} :: rest
    -> if ((a=eki1) && (b=ken1) && (c=eki2) && (d=ken2))
       || ((a=eki2) && (b=ken2) && (c=eki1) && (d=ken1)) then f
        else get_ekikan_kyori2 ( eki1, ken1 ) ( eki2, ken2 ) rest

(* テスト *)
let test1 = get_ekikan_kyori2 ("新大塚", "東京") ("池袋", "東京") global_ekikan_list = 1.8
let test2 = get_ekikan_kyori2 ("原木中山", "千葉") ("妙典", "千葉") global_ekikan_list = 2.1
let test3 = get_ekikan_kyori2 ("四ツ谷", "東京") ("御茶ノ水", "東京") global_ekikan_list = infinity
