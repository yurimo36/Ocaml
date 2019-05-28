#use "report6.ml";;

(* 目的：eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組 （(eki_t * eki_t list) 型）を返す関数 *)
(* saitan_wo_bunri2 : eki_t list -> (eki_t * eki_t) list *)
let rec saitan_wo_bunri2 lst = match lst with
    [] -> ({namae=("", ""); saitan_kyori=infinity; temae_list=[]},[])
  | ({namae=(a, b); saitan_kyori=c; temae_list=d}as first) :: rest ->
    match (saitan_wo_bunri2 rest) with (({namae=(e, f); saitan_kyori=g; temae_list=h}as saitan_eki), nokori_eki) ->
      
    if c > g
    then (saitan_eki, first :: nokori_eki)
    else (first , rest)


(* テスト *)
let test1 = saitan_wo_bunri2 []
let test2 = saitan_wo_bunri2 testA
