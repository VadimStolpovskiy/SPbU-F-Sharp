module Homework.Task2

open ConsList
open Homework.OOPList

// №1. Bubble sort
let consBubbleSort lst =

    let rec sort lst =
        match lst with
        | Empty -> Empty
        | Cons(hd, Empty) -> Cons(hd, Empty)
        | Cons(hd1, Cons(hd2, tl)) ->
            if hd1 > hd2 then
                Cons(hd2, sort (Cons(hd1, tl)))
            else
                Cons(hd1, sort (Cons(hd2, tl)))

    let rec loop lst cnt =
        if cnt = 0 then lst else loop (sort lst) (cnt - 1)

    loop lst (ConsList.getLength lst)

let oopBubbleSort lst =

    let rec sort (lst: OOPList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>
        | :? NonEmptyList<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                lst
            else
                let hd1 = lst.Head
                let hd2 = getHead lst.Tail
                let tl2 = getTail lst.Tail

                if hd1 > hd2 then
                    NonEmptyList(hd2, sort (NonEmptyList(hd1, tl2)))
                else
                    NonEmptyList(hd1, sort lst.Tail)

    let rec loop lst cnt =
        if cnt = 0 then lst else loop (sort lst) (cnt - 1)

    loop lst (OOPList.getLength lst)


// №2. Quicksort
let consQuickSort lst =

    let rec sort lst =
        match lst with
        | Empty -> Empty
        | Cons(hd, Empty) -> Cons(hd, Empty)
        | Cons(hd, tl) ->
            let parts = ConsList.partition (fun elem -> elem < hd) tl
            ConsList.concat (sort (fst parts)) (Cons(hd, sort (snd parts)))

    sort lst

let OOPQuickSort lst =

    let rec sort (lst: OOPList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>
        | :? NonEmptyList<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                NonEmptyList(lst.Head, EmptyList())
            else
                let hd = getHead lst.Tail
                let tl = getTail lst.Tail

                let parts = partition (fun elem -> elem < lst.Head) (NonEmptyList(hd, tl))
                OOPList.concat (sort (fst parts)) (NonEmptyList(lst.Head, sort (snd parts)))

    sort lst
