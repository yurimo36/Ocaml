#use "report4.ml";;
  #use "report6.ml";;
    #use "metro.ml";;
(* 目的：(直前に最短距離を確定した）点 p（eki_t 型）と 最短距離が未確定の点 q（eki_t 型）、および 駅間のリスト（ekikan_t list 型）を受け取ったら、 （第４回の課題で作った） get_ekikan_kyori2 を使って q が p に接続しているかを調べ、 接続していたら最短距離と手前リストが条件を満たすようになっている新しい q を 返す関数 *)
(* koushin1 : eki_t -> eki_t -> ekikan_t list -> eki_t *)
let koushin1 p q lst = match p with
    {namae=(a, b); saitan_kyori=c; temae_list=d} ->
                       match q with
    {namae=(e, f); saitan_kyori=g; temae_list=h} ->
      let kyori = get_ekikan_kyori2 (a, b) (e, f) lst
      if kyori = infinity then q
      else if  c + kyori(*p経由*) < g(*q保持*)
      then {namae=(e, f); saitan_kyori=c + kyori; temae_list=(a, b) :: d}
      else q


(* テスト *)
let test1 = koushin1 {namae = ("渋谷", "東京"); saitan_kyori = 0.; temae_list = [("渋谷", "東京")]}
    {namae = ("表参道", "東京"); saitan_kyori = infinity; temae_list = []};
  global_ekikan_list

 

(* 目的：直前に最短距離を確定した）点 p（eki_t 型）と 最短距離が未確定の点の集合 V（eki_t list 型）、および 駅間のリスト（ekikan_t list 型）を受け取ったら、 V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す関数 *)
(* koushin : eki_t  -> eki_t list -> ekikan_t list -> eki_t list *)
let rec koushin p v lst = match 


(* テスト *)
let test1 = koushin*)
