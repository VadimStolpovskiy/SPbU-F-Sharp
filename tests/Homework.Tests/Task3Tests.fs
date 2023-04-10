module Homework.Tests.Task3Tests

open Expecto
open Homework
open ConsList
open Tree

module TestCases =

    [<Tests>]
    let tests =

        testList
            "samples"

            [
                testCase "Single leaf"
                <| fun _ ->
                    let input = Leaf("leaf")

                    let expected = Cons("leaf", Empty), 1
                    let actual1 = toConsListToRoot input, countDistinct input
                    let actual2 = toConsListFromRoot input, countDistinct input

                    Expect.equal actual1 expected "Conversion or counting of distinct elements did not go as expected"
                    Expect.equal actual2 expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Empty leaf"
                <| fun _ ->
                    let input = Leaf(Empty)

                    let expected = Cons(Empty, Empty), 1
                    let actual1 = toConsListToRoot input, countDistinct input
                    let actual2 = toConsListFromRoot input, countDistinct input

                    Expect.equal actual1 expected "Conversion or counting of distinct elements did not go as expected"
                    Expect.equal actual2 expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Binary tree, all distinct elements (To root)"
                <| fun _ ->
                    let input = Node(1, Cons(Node(2, Cons(Leaf(4), Cons(Leaf(5), Empty))), Cons(Node(3, Cons(Leaf(6), Cons(Leaf(7), Empty))), Empty)))

                    let expected = Cons(7, Cons(6, Cons(3, Cons(5, Cons(4, Cons(2, Cons(1, Empty))))))), 7
                    let actual = toConsListToRoot input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Binary tree, all distinct elements (From root)"
                <| fun _ ->
                    let input = Node(1, Cons(Node(2, Cons(Leaf(4), Cons(Leaf(5), Empty))), Cons(Node(3, Cons(Leaf(6), Cons(Leaf(7), Empty))), Empty)))

                    let expected = Cons(1, Cons(2, Cons(4, Cons(5, Cons(3, Cons(6, Cons(7, Empty))))))), 7
                    let actual = toConsListFromRoot input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Binary tree, same elements"
                <| fun _ ->
                    let input = Node("1", Cons(Node("1", Cons(Leaf("1"), Cons(Leaf("1"), Empty))), Cons(Node("1", Cons(Leaf("1"), Cons(Leaf("1"), Empty))), Empty)))

                    let expected = Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Empty))))))), 1
                    let actual1 = toConsListToRoot input, countDistinct input
                    let actual2 = toConsListFromRoot input, countDistinct input

                    Expect.equal actual1 expected "Conversion or counting of distinct elements did not go as expected"
                    Expect.equal actual2 expected "Conversion or counting of distinct elements did not go as expected"

                testProperty "Number of distinct elements in a tree should be <= number of all elements (To root)"
                <| fun (tree: Tree<int>) ->
                    let lst = toConsListToRoot tree
                    let cnt = countDistinct tree
                    cnt <= getLength lst

                testProperty "Number of distinct elements in a tree should be <= number of all elements (From root)"
                <| fun (tree: Tree<int>) ->
                    let lst = toConsListFromRoot tree
                    let cnt = countDistinct tree
                    cnt <= getLength lst

                testProperty "Number of distinct elements in a tree should be <= 1"
                <| fun (tree: Tree<int>) ->
                    let cnt = countDistinct tree
                    1 <= cnt

                testProperty "Number of elements in list should be <= 1 (To root)"
                <| fun tree ->
                    1 <= getLength (toConsListToRoot tree)

                testProperty "Number of elements in list should be <= 1 (From root)"
                <| fun tree ->
                    1 <= getLength (toConsListFromRoot tree)

                testProperty "Tree.fold and ConsList.fold should work the same way (+)"
                <| fun tree acc ->
                    let treeFold = Tree.fold (+) acc tree
                    let consFold = ConsList.fold (+) acc (toConsListToRoot tree)

                    treeFold = consFold

                testProperty "Tree.fold and ConsList.fold should work the same way (*)"
                <| fun tree acc ->
                    let treeFold = Tree.fold (*) acc tree
                    let consFold = ConsList.fold (*) acc (toConsListToRoot tree)

                    treeFold = consFold

                testProperty "Tree.foldBack and ConsList.foldBack should work the same way (+)"
                <| fun tree acc ->
                    let treeFoldBack = Tree.foldBack (+) acc tree
                    let consFoldBack = ConsList.foldBack (+) acc (toConsListToRoot tree)

                    treeFoldBack = consFoldBack

                testProperty "Tree.foldBack and ConsList.foldBack should work the same way (*)"
                <| fun tree acc ->
                    let treeFoldBack = Tree.foldBack (*) acc tree
                    let consFoldBack = ConsList.foldBack (*) acc (toConsListToRoot tree)

                    treeFoldBack = consFoldBack
            ]
