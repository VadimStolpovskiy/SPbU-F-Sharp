module Homework.ConsList

type ConsList<'value> =
    | Cons of head: 'value * tail: ConsList<'value>
    | Empty

let rec getLength lst =
    match lst with
    | Empty -> 0
    | Cons(_, tl) -> getLength tl + 1

let rec consListToList lst =
    match lst with
    | Empty -> []
    | Cons(hd, tl) -> hd :: consListToList tl

let rec listToConsList lst =
    match lst with
    | [] -> Empty
    | lst -> Cons(List.head lst, listToConsList (List.tail lst))

let rec partition predicate lst =
    match lst with
    | Empty -> Empty, Empty
    | Cons(hd, tl) ->
        let parts = partition predicate tl

        if predicate hd then
            Cons(hd, fst parts), snd parts
        else
            fst parts, Cons(hd, snd parts)

// №3. Concatenation
let rec concat lst1 lst2 =
    match lst1 with
    | Cons(hd, tl) -> Cons(hd, concat tl lst2)
    | Empty -> lst2
