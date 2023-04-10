module Homework.Tree

open Homework.ConsList
open System.Collections.Generic

// №1. A tree with an arbitrary number of children
type Tree<'value> =
    | Node of value: 'value * children: ConsList<Tree<'value>>
    | Leaf of value: 'value

let rec fold folder acc tree =
    match tree with
    | Leaf value -> folder value acc
    | Node(value, children) -> ConsList.fold (fold folder) (folder value acc) children

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
    let result = fold Set.add Set.empty tree
    result.Count

// №3. A function that compiles a ConsList containing all values from all nodes
let toConsListToRoot tree =
    let prefix hd lst = Cons(hd, lst)
    fold prefix Empty tree

let toConsListFromRoot tree =
    let suffix hd lst = concat lst (Cons(hd, Empty))
    fold suffix Empty tree
