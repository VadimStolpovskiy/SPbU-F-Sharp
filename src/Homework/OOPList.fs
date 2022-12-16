module Homework.OOPList

type OOPList<'value> =
    interface
    end

type NonEmptyList<'value>(head: 'value, tail: OOPList<'value>) =
    interface OOPList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface OOPList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec getLength (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> 0
    | :? NonEmptyList<'value> as lst -> getLength lst.Tail + 1

let rec getHead (lst: OOPList<'value>) =
    match lst with
    | :? NonEmptyList<'value> as lst -> lst.Head

let rec getTail (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>
    | :? NonEmptyList<'value> as lst -> lst.Tail

let rec OOPListToList (lst: OOPList<'value>) =
    match lst with
    | :? EmptyList<'value> -> []
    | :? NonEmptyList<'value> as lst -> [ lst.Head ] @ OOPListToList lst.Tail

let rec listToOOPList lst : OOPList<'value> =
    match lst with
    | [] -> EmptyList()
    | hd :: tl -> NonEmptyList(hd, listToOOPList tl)

let rec partition (lst: OOPList<'value>) pivot =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> OOPList<'value>, EmptyList() :> OOPList<'value>
    | :? NonEmptyList<'value> as lst ->
        let parts = partition lst.Tail pivot

        if lst.Head <= pivot then
            NonEmptyList(lst.Head, fst parts), snd parts
        else
            fst parts, NonEmptyList(lst.Head, snd parts)

// №3. Concatenation
let rec concat (lst1: OOPList<'value>) (lst2: OOPList<'value>) : OOPList<'value> =
    match lst1 with
    | :? NonEmptyList<'value> as lst1 -> NonEmptyList(lst1.Head, concat lst1.Tail lst2)
    | :? EmptyList<'value> -> lst2
