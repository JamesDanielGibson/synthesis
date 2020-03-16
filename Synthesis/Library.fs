module Synthesis

let abelar a = 
    match a>12 with
    |true -> 
        match a<3097 with
        |true -> 
            match a%12 with
            |0 -> true
            |_->false
        |_->false
    |_->false

let area (b:float) (h:float) =
    match b<0.0 with
    |true-> failwith "negative numbers"
    |_->
        match h<0.0 with
        |true-> failwith "negative number"
        |_-> 0.5*b*h

let zollo a = match a<0 with
    |true-> a*(-1)
    |_-> a*2

let min a b = 
    match a > b with
    |true -> b
    |_->a
    
    
let max a b =
    match a>b with
    |true -> a
    |_->b

let ofTime a b c = (a*60*60+b*60+c)
    

let toTime t =
    let h = t/3600
    let m = (t-(h*3600))/60
    let s = t-(h*3600 + m*60)
    match s< 0 with 
    |true -> (0,0,0)
    |_-> h,m,s

let digits a =
    let rec inner (b:int)  = 
        match b<10 && b> -10 with
        |false -> 1+inner (b/10) 
        |_->1
    inner a

let minmax a =
    failwith "not implemented"
     

let isLeap a = 
    match a%1 with
    |0->match a%4 with
       |0 -> match a%100 with
           |0 -> match a%400 with
               |0-> true
               |_->false
           |_->true
       |_-> false
    |_ ->failwith ""

let month a =
    match a with
       |1 -> ("January",31)
       |2 -> ("February",28)
       |3 -> ("March",31)
       |4 -> ("April",30)
       |5 -> ("May",31)
       |6 -> ("June",30)
       |7 -> ("July",31)
       |8 -> ("August",31)
       |9 -> ("September",30)
       |10 -> ("October",31)
       |11 -> ("November",30)
       |12 -> ("December",31)
       |_ -> failwith "not a month"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"