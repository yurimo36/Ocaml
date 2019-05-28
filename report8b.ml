#use "report6.ml";; 

(* 目的 : eki_t list 型のリストを受け取ったら、 「最短距離最小の駅」と
   「最短距離最小の駅以外からなるリスト」の組 （(eki_t * eki_t list) 型）を返す *)
(* saitan_wo_bunri2 : eki_t list -> eki_t * (eki_t list) *)
let saitan_wo_bunri2 lst =
  let saitan_wo_bunri eki_t (saitan_eki, nokori_eki) =
    if saitan_eki = {namae=("", ""); saitan_kyori=infinity; temae_list=[]}
    then (eki_t, [])
    else if eki_t.saitan_kyori > saitan_eki.saitan_kyori
    then (saitan_eki, eki_t :: nokori_eki)
    else (eki_t, saitan_eki :: nokori_eki)

  in List.fold_right saitan_wo_bunri lst
    ({namae=("", ""); saitan_kyori=infinity; temae_list=[]}, [])

(* テスト *)
let test = saitan_wo_bunri2 testA
