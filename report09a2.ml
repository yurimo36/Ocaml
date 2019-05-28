(* report09a2.ml *)

#use "report06.ml";;
(*
(* 型 eki_t *)
type eki_t = {
  namae : string * string;             (* 駅名ペア:漢字の駅名と県名 *)
  saitan_kyori : float;                (* 最短距離 *)
  temae_list : (string * string) list; (* 手前リスト:駅名ペアのリスト *)
}

(* 型 ekikan_t *)
(* 駅間の情報を格納するレコード型 *)
type ekikan_t = {
  kiten  : string; (* 起点 *)
  kenk   : string; (* 起点の県名 *)
  shuten : string; (* 終点 *)
  kens   : string; (* 終点の県名 *)
  keiyu  : string; (* 経由路線名 *)
  kyori  : float;  (* 距離 *)
  jikan  : int;    (* 所要時間 *)
}

*)

(* 関数 saitan_wo_bunri2 *)
(* 目的 : eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と
   「最短距離最小の駅以外からなるリスト」の組 （(eki_t * eki_t list) 型）を返す *)
(* saitan_wo_bunri2 : eki_t list -> eki_t * (eki_t list) *)
let saitan_wo_bunri2 lst =
  (* 目的 : 最短距離を比べて、小さい方をsaitan_no_ekiとし、
             大きい方をnokori_listの先頭に加える*)
  (* saitan_wo_bunri : eki_t -> eki_t * eki t list -> eki_t * eki_t list *)
  let saitan_wo_bunri eki_t (saitan_no_eki, nokori_list) =
    if saitan_no_eki = {namae=("", ""); saitan_kyori=infinity; temae_list=[]}
    then (eki_t, [])
    else if eki_t.saitan_kyori > saitan_no_eki.saitan_kyori
    then (saitan_no_eki, eki_t :: nokori_list)
    else (eki_t, saitan_no_eki :: nokori_list)
  in List.fold_right saitan_wo_bunri lst
    ({namae=("", ""); saitan_kyori=infinity; temae_list=[]}, [])

(* レポート4 問3 : get_skikan_kyori2の定義 *)
(* 目的 : 駅名ペアをふたつと駅間リストを受け取って来たら、 駅間リストの中から
   その２駅（ペア）間の距離を返す *)
(* get_ekikakn_kyori2 : string * string -> string * string
                        -> ekikan_t list -> float *)
let rec get_ekikan_kyori2 pair1 pair2 lst = match pair1 with
    ( eki1, ken1 ) -> match pair2 with
     ( eki2, ken2 ) -> match lst with
      [] -> infinity
     | { kiten = kiten; kenk = kenk; shuten = shuten; kens = kens;
         keiyu = keiyu; kyori = kyori; jikan = jikan } :: rest ->
      if ( kiten = eki1 && kenk = ken1 && shuten = eki2 && kens = ken2 ) ||
         ( kiten = eki2 && kenk = ken2 && shuten = eki1 && kens = ken1 )
       then kyori
      else get_ekikan_kyori2 pair1 pair2 rest


(* 関数 koushin *)
(* 目的 : （直前に最短距離を確定した）点 p（eki_t 型）と 最短距離が未確定の点の集合 
   V（eki_t list 型）、および 駅間のリスト（ekikan_t list 型）を受け取ったら、  
   V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す    *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin p v_lst ekikan_lst =
  
  (* koushin1 *)
(* 目的 :（直前に最短距離を確定した）点p（eki_t 型）と最短距離が未確定の点q（eki_t 型）、
   および 駅間のリスト（ekikan_t list 型）を受け取ったら、 （第４回の課題で作った）get_ek
   ikan_kyori2 を使って q が p に接続しているかを調べ、 接続していたら最短距離と手前リスト
   が次のようになっている新しい q を 返す関数 koushin1 をデザインレシピに従って作れ。
   （接続していなかったら q をそのまま返す。）
   現在、q が保持している最短距離と、p 経由で q に行った場 合の距離（p の最短距離に pq 間の
   距離を加えたもの）を比べ、 新しい q の最短距離はそのうち小さい方とする。
   さらに p 経由で行った場合の方が短かった場合は、 p の手前リストの先頭に q（の駅名ペア）を加
   えたものを q の temae_list にする。 *)
(* koushin1 : eki_t -> eki_t -> ekikan_t list -> eki_t *)
  let koushin1 p q lst =
    let kyori_pq =
      get_ekikan_kyori2 p.namae q.namae lst             (* 駅間距離/実数or無限大 *)
    in if q.saitan_kyori <= p.saitan_kyori +. kyori_pq
    then q
    else {namae = q.namae;
          saitan_kyori = p.saitan_kyori +. kyori_pq;
          temae_list = q.namae :: p.temae_list}
  in  List.map (fun q -> koushin1 p q ekikan_lst) v_lst

(* 関数 romaji_to_kanji2 *)
(* 目的 : ローマ字の文字列による駅名と駅名リスト（前回、作ったekimei_t型のリスト）
   を受け取ったら、（教科書とは違って）その駅の漢字表記（文字列）と県名（文字列）の
   ペアを 返す *)
(* romaji_to_kanji2 : string -> ekimei_t list -> string * string *)
let rec romaji_to_kanji2 romaji lst = match lst with
    [] -> ("", "")
  | {kanji = j; romaji = r; ken = k} :: rest ->
    if romaji = r then ( j, k )
    else romaji_to_kanji2 romaji rest

(* 関数 seiretsu2 *)
(* 目的 : ekimei_listを受け取ったら、それを順に整列し、 さらに重複した駅を取り除いた ekimei_list を返す *)
(* seiretsu2 : ekimei_t list -> ekimei_t list *)
let rec seiretsu2 lst =

  (* 目的 : すでに昇順に順に並んでいる駅名リストとekimei_t型のデータxを受け取ったら、重複を取り除いて、昇順になる位置にxを挿入したリストを返す *)
  (* insert_ekimei2 : ekimei_t list -> ekimei_t -> ekimei_t kist *)
  let rec insert_ekimei2 lst x = match lst with
      [] -> x :: []
    | ({kanji = kanji1; ken = ken1} as first) :: rest ->
      match x with
        {kanji = kanji2; ken = ken2} ->
        if ken2 < ken1                           then x :: lst
        else if (ken2 = ken1 && kanji2 < kanji1) then x :: lst 
        else if (ken2 = ken1 && kanji2 = kanji1) then lst
        else if (ken2 = ken1)                    then first :: ( insert_ekimei2 rest x )
        else                                          first :: ( insert_ekimei2 rest x )
  in  match lst with
    [] -> []
  | first :: rest -> insert_ekimei2 ( seiretsu2 rest ) first


(* ------------------------------------------------------------------------ *)


(* 問2 *)
(* 目的 : 起点のみ最短距離が 0 で他は infinity となっている 駅のリスト（eki_t list 型）
   と駅間リスト（ekikan_t list 型） を受け取ったら、 「起点からの最短距離と『起点からその
   駅に至る 駅名の（逆順の）リスト』が入った駅」のリスト（eki_t list 型）を返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_t_lst ekikan_t_lst =
  if eki_t_lst = [] then eki_t_lst                  (* 自明なケース *)
  else                                              (* 再起によるケース *)
    (* vから最短距離最小の点(eki_t型)を確定し、pとする *)
    match (saitan_wo_bunri2 eki_t_lst) with
      (p, nokori_list) -> 
    (* pに接続している点の最短距離を更新し、未確定の駅の集合をvとする *)
    let v = koushin p nokori_list ekikan_t_lst          
    in p :: (dijkstra_main v ekikan_t_lst)


(* テスト *)
let test0921 = dijkstra_main
    [{namae=("A", "a"); saitan_kyori=0.;       temae_list=[("A", "a")]};
     {namae=("B", "b"); saitan_kyori=infinity; temae_list=[]};
     {namae=("C", "c"); saitan_kyori=infinity; temae_list=[]};
     {namae=("D", "d"); saitan_kyori=infinity; temae_list=[]};
     {namae=("E", "e"); saitan_kyori=infinity; temae_list=[]}]
    [{kiten="A"; kenk="a"; shuten="B"; kens="b"; keiyu="ABCE"; kyori=10.; jikan=10};
     {kiten="B"; kenk="b"; shuten="C"; kens="c"; keiyu="ABCE"; kyori=2.; jikan=2};
     {kiten="C"; kenk="c"; shuten="E"; kens="e"; keiyu="ABCE"; kyori=1.; jikan=1};
     {kiten="A"; kenk="a"; shuten="D"; kens="d"; keiyu="ADE"; kyori=4.; jikan=4};
     {kiten="D"; kenk="d"; shuten="E"; kens="e"; keiyu="ADE"; kyori=3.; jikan=3};
     {kiten="B"; kenk="b"; shuten="E"; kens="e"; keiyu="BE"; kyori=2.; jikan=2}]
let test0922 = dijkstra_main test6_4 global_ekikan_list 
let test0923 = dijkstra_main test6_5 global_ekikan_list
let test0924 = dijkstra_main [] global_ekikan_list

(* 停止性 *)
(* dijkstra_mainで再起した時、第一引数であるeki_t_lstがmatch文以下により長さが1づつ減っているので
   いづれ[]となり、終わる。 *)
    

(* ------------------------------------------------------------------------------- *)

(* 問3 *)
(* 目的 : 起点の（ローマ字の）駅名と終点の（ローマ字の）駅名 と駅名リスト（ekimei_t list 型）・ 
   駅間リスト（ekikan_t list 型） を受け取ったら、romaji_to_kanji2 を使って起点と終点の
   駅名ペアを求め、受け取った駅名リストから seiretsu2 を使って重複を取り除き、make_eki_list2
   とshokika2 （または make_initial_eki_list2）を使って 駅のリスト（eki_t list 型）を作り、
   dijkstra_main を使って、各駅までの最短路を確定し、その中から終点の駅（eki_t 型）を探して返す
   （この関数は、別途定義する必要あり） *)
(* dijkstra : char -> char -> ekimei_t list -> ekikan_t list -> eki_t *)

let dijkstra kiten shuten ekimei_list ekikan_list =

  (* 起点の駅名ペア: char * char型 *)
  let kiten_ekimei_pair = 
    romaji_to_kanji2 kiten ekimei_list

  (* 終点の駅名ペア: char * char型 *)
  in let shuten_ekimei_pair =
       romaji_to_kanji2 shuten ekimei_list
      
  (* 受け取ったekimei_listから重複を取り除いたもの: ekimei_t list型 *)
  in let ekimei_list_kai = 
       seiretsu2 ekimei_list
       
  (* ekimei_list_kaiから作る駅のリスト: eki_t list型 *)
  in let eki_t_list2 = 
       shokika2 (make_eki_list2 ekimei_list_kai) kiten_ekimei_pair
         
  (* eki_t_listの各駅までの最短路を確定したもの: eki_t list型 *)
  in let eki_t_list_kai = 
       dijkstra_main eki_t_list2 ekikan_list
       
  (* saitanro_eki_t_listから終点の駅を探す関数 *)
  (* seek_shuten : char -> eki_t list -> eki_t  *)
  in let rec seek_shuten shuten lst = match lst with
        [] -> {namae=("", ""); saitan_kyori=infinity; temae_list=[]}
      | first :: rest ->
        if first.namae = shuten_ekimei_pair then first
        else seek_shuten shuten rest 
       
  in seek_shuten shuten eki_t_list_kai


(* テスト *)
let test0931 = dijkstra "A" "C"
    [{kanji="A"; kana="A"; romaji="A"; ken="a"; shozoku="ABCE"};
     {kanji="B"; kana="B"; romaji="B"; ken="b"; shozoku="ABCE"};
     {kanji="C"; kana="C"; romaji="C"; ken="c"; shozoku="ABCE"};
     {kanji="E"; kana="E"; romaji="E"; ken="e"; shozoku="ABCE"};
     {kanji="A"; kana="A"; romaji="A"; ken="a"; shozoku="ADE"};
     {kanji="D"; kana="D"; romaji="D"; ken="d"; shozoku="ADE"};
     {kanji="E"; kana="E"; romaji="E"; ken="e"; shozoku="ADE"};
     {kanji="B"; kana="B"; romaji="B"; ken="b"; shozoku="BE"};
     {kanji="E"; kana="E"; romaji="E"; ken="e"; shozoku="BE"}]
    [{kiten="A"; kenk="a"; shuten="B"; kens="b"; keiyu="ABCE"; kyori=10.; jikan=10};
     {kiten="B"; kenk="b"; shuten="C"; kens="c"; keiyu="ABCE"; kyori=2.; jikan=2};
     {kiten="C"; kenk="c"; shuten="E"; kens="e"; keiyu="ABCE"; kyori=1.; jikan=1};
     {kiten="A"; kenk="a"; shuten="D"; kens="d"; keiyu="ADE"; kyori=4.; jikan=4};
     {kiten="D"; kenk="d"; shuten="E"; kens="e"; keiyu="ADE"; kyori=3.; jikan=3};
     {kiten="B"; kenk="b"; shuten="E"; kens="e"; keiyu="BE"; kyori=2.; jikan=2}]
        
let test0932 = dijkstra "yoyogiuehara" "yoyogikoen"
    [{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; ken="東京"; shozoku="千代田線"};{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikoen"; ken="東京"; shozoku="千代田線"}]
    [{kiten="代々木上原"; kenk="東京"; shuten="代々木公園"; kens="東京"; keiyu="千代田線"; kyori=1.0; jikan=2}]

let test0933 = dijkstra "yoyogiuehara" "nogizaka" global_ekimei_list global_ekikan_list

let test0934 = dijkstra "yoyogiuehara" "ikebukuro" global_ekimei_list global_ekikan_list
