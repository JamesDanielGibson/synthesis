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
    let rec inner b count = 
        match b/10>0 with
        |true -> inner (b/10) (count+1)
        |_->count
    inner a

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"