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
                    let actual = toConsList input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Empty leaf"
                <| fun _ ->
                    let input = Leaf(Empty)

                    let expected = Cons(Empty, Empty), 1
                    let actual = toConsList input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Binary tree, all distinct elements"
                <| fun _ ->
                    let input = Node(1, Cons(Node(2, Cons(Leaf(4), Cons(Leaf(5), Empty))), Cons(Node(3, Cons(Leaf(6), Cons(Leaf(7), Empty))), Empty)))

                    let expected = Cons(7, Cons(6, Cons(3, Cons(5, Cons(4, Cons(2, Cons(1, Empty))))))), 7
                    let actual = toConsList input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testCase "Binary tree, same elements"
                <| fun _ ->
                    let input = Node("1", Cons(Node("1", Cons(Leaf("1"), Cons(Leaf("1"), Empty))), Cons(Node("1", Cons(Leaf("1"), Cons(Leaf("1"), Empty))), Empty)))

                    let expected = Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Cons("1", Empty))))))), 1
                    let actual = toConsList input, countDistinct input

                    Expect.equal actual expected "Conversion or counting of distinct elements did not go as expected"

                testProperty "Number of distinct elements in a tree should be <= number of all elements"
                <| fun tree ->
                    let lst = toConsList tree
                    let cnt = countDistinct tree
                    cnt <= getLength lst

                testProperty "Number of distinct elements in a tree should be <= 1"
                <| fun tree ->
                    let cnt = countDistinct tree
                    1 <= cnt

                testProperty "Number of elements in list should be <= 1"
                <| fun tree ->
                    1 <= getLength (toConsList tree)

                testProperty "Tree.fold and ConsList.fold should work the same way"
                <| fun tree acc ->
                    let treeFold = Tree.fold (+) acc tree
                    let consFold = ConsList.fold (+) acc (toConsList tree)

                    treeFold = consFold
            ]
