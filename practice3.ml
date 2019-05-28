(* 目的：実数の組として表された xy 平面上の座標（ベクトル）をふたつ受け取ったら、 それらを加えた座標（ベクトル）を返す関数  *)
(* add_vector : float * float -> float * float -> float * float *)
let add_vector x y = match (x,y) with
    ( (a,b) , (c,d) ) -> ( a+.c , b+.d )
                         
(* テスト *)
let test1 = add_vector (2.0,3.0) (4.0,5.0) = (6.0,8.0)
let test2 = add_vector (7.0,9.0) (-2.0,-10.0) = (5.0,-1.0)
let test3 = add_vector (8.0,-3.0) (-9.0,7.0) = (-1.0,4.0)


(* 名前・身長・体重を格納するレコード型 *)
type person_t = {
  name : string;
  shincho : float;
  taiju : float;
}
(* 学生データの例 *)
let gakusei1 = {name="A子"; shincho=1.5; taiju=40.}
let gakusei2 = {name="B子"; shincho=1.5; taiju=50.}             
let gakusei3 = {name="C子"; shincho=1.5; taiju=60.}


(* 目的：person_t 型のデータを受け取ったら、その人の bmi を計算し 「○○さんの体型は××です。」 という文字列を返す関数 *)
(* taikei_hyoji : person_t -> string *)
let bmi x y = y /. ( x *. x )
let taikei x y = if bmi x y < 18.5 then "低体重"
  else if bmi x y >= 25.0 then "肥満"
  else "普通"
let taikei_hyoji person = match person with
    { name=n; shincho=s; taiju=t; } ->
    n^ "さんの体型は" ^taikei s t^ "です。"
 
(* テスト *)
let test1 = taikei_hyoji gakusei1 = "A子さんの体型は低体重です。"
let test2 = taikei_hyoji gakusei2 = "B子さんの体型は普通です。"
let test3 = taikei_hyoji gakusei3 = "C子さんの体型は肥満です。"

