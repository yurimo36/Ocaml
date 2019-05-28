(* 候補： #use "metro.ml" or "higashi.ml" or "global.ml" or "hokkaido.ml" *)
#use "metro.ml"
(* 目的：整数のリストを受け取ったら、（順番を変えることなく） その中の偶数の要素のみを含むリストを返す関数 *)
(* even : int list -> int list *)
let rec even lst = match lst with
 	 [] -> []
	| first :: rest -> if first mod 2 = 0 then first :: even rest
	                                      else even rest

(* テスト *)
let test1 = even [] = []
let test2 = even [5; 6; 7; 8 ;9] = [6; 8]
let test3 = even [2; 1; 6; 4; 7] = [2; 6; 4]



(* 目的：すでに昇順に並んでいるリスト lst と数字 n を 受け取ったら、lst を前から順に見ていき、 昇順になる位置に n を挿入したリストを返す関数 *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
	 [] -> [n]
  | first :: rest -> if first >= n then n :: (first :: rest)
                                   else first :: insert rest n

(* テスト *)
let test1 = insert [] 0 =[0]
let test2 = insert [-3; -2; 4; 5] 1 = [-3; -2; 1; 4; 5]
let test3 = insert [-1; 0; 1] 2 = [-1; 0; 1; 2]



(* 目的：リストを受け取ったら、挿入法により昇順に整列したリストを返す関数 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
	 [] -> []
  | first :: rest ->  insert (ins_sort rest) first


(* テスト *)
let test1 = ins_sort [] = []
let test2 = ins_sort [8; 6; 1; 5; 4] = [1; 4; 5; 6; 8]
let test3 = ins_sort [9; 7; 3; 2; 0] = [0; 2; 3; 7; 9]
                                       
let kenmei = ins_sort ["北海道"; "青森"; "岩手";"宮城"; "秋田"; "山形"; "福島"; "茨城"; "栃木"; "群馬"; "埼玉"; "千葉"; "東京"; "神奈川";"新潟"; "富山"; "石川"; "福井"; "山梨"; "長野"; "岐阜";"静岡";"愛知"; "三重"; "滋賀"; "京都"; "大阪";"兵庫"; "奈良";"和歌山"; "鳥取"; "島根";"岡山"; "広島";"山口"; "徳島"; "香川"; "愛媛"; "高知"; "福岡"; "佐賀";"長崎"; "熊本"; "大分"; "宮崎"; "鹿児島"; "沖縄"; ]




(* 目的：すでに昇順に並んでいるリスト ekimei_list と ekimei を 受け取ったら、 ekimei_list を前から順に見ていき、 昇順になる位置に ekimei が含まれる要素を挿入したリストを返す関数 *)
(* seiretsu_insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec seiretsu_insert lst x = match lst with
    [] -> [x]
    | ({kanji=a; kana=b; romaji=c; ken=d; shozoku=e} as first) :: rest
      -> match x with
      |  {kanji=f; kana=g; romaji=h; ken=i; shozoku=j}
         -> if (a=f) && (d=i) then first :: rest 
            else if (a>f) || ((a>f) && (d>i)) then  x :: (first :: rest)
            else first :: seiretsu_insert rest x
                   
(* 目的：ekimei_list を受け取ったら、それを条件に沿って整列し、さらに重複した駅を取り除いた ekimei_list を返す関数 *)
(* seiretsu2 : ekimei_t list -> ekimei_t list *)           
let rec seiretsu2 lst = match lst with
	[] -> []
    |  first :: rest
       -> seiretsu_insert (seiretsu2 rest) first
                 
          
(* テスト *)
let test = seiretsu2 global_ekimei_list
