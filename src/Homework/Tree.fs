module Homework.Tree

open Homework.ConsList
open System.Collections.Generic

// №1. A tree with an arbitrary number of children
type Tree<'value> =
    | Node of value: 'value * children: ConsList<Tree<'value>>
    | Leaf of value: 'value

let rec fold folder acc tree =
    let f = fold folder

    match tree with
    | Leaf value -> folder acc value
    | Node(value, children) -> ConsList.fold f (folder acc value) children

let foldBack folder acc tree =

    let rec fold acc tree =
        match tree with
        | Leaf value ->
            let acc' inner = folder (acc inner) value
            acc'
        | Node(value, children) ->
            let acc' inner =  ConsList.foldBack (fold acc) inner children
            let acc'' inner = folder (acc' inner) value
            acc''

    fold id tree acc

// №2. A function that counts the number of different elements stored in nodes
let countDistinct tree =
    let add (set: HashSet<'value>) value =
        set.Add value
        set

    let set = HashSet<'value>()
    let result = fold add set tree
    result.Count

// №3. A function that compiles a ConsList containing all values from all nodes
let toConsListToRoot tree =
    let prefix lst hd = Cons(hd, lst)
    fold prefix Empty tree

let toConsListFromRoot tree =
    let suffix lst hd = concat lst (Cons(hd, Empty))
    fold suffix Empty tree
