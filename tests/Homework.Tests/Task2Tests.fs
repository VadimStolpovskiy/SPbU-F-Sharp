namespace Homework.Tests.Task2Tests

open Expecto
open Homework
open Homework.ConsList
open Homework.OOPList
open Homework.Task2

module TestCases =

    [<Tests>]
    let tests =

        testList
            "samples"

            [
              // №1. Bubble sort

              testProperty "Bubble sort ConsList<int> | Comparison with system sort"
              <| fun (lst: list<int>) ->
                  let expected = List.sort lst
                  let actual = consBubbleSort (listToConsList lst)

                  consListToList actual = expected

              testProperty "Bubble sort ConsList<string> | Comparison with system sort"
              <| fun (lst: list<string>) ->
                  let expected = List.sort lst
                  let actual = consBubbleSort (listToConsList lst)

                  consListToList actual = expected

              testCase "Bubble sort ConsList | Empty)"
              <| fun _ ->
                  let expected = Empty
                  let actual = consBubbleSort Empty

                  Expect.equal actual expected "Actual result is not sorted properly"

              testProperty "Bubble sort OOPList<int> | Comparison with system sort"
              <| fun (lst: list<int>) ->
                  let expected = List.sort lst
                  let actual = oopBubbleSort (listToOOPList lst)

                  OOPListToList actual = expected

              testProperty "Bubble sort OOPList<string> | Comparison with system sort"
              <| fun (lst: list<string>) ->
                  let expected = List.sort lst
                  let actual = oopBubbleSort (listToOOPList lst)

                  OOPListToList actual = expected

              testCase "Bubble sort OOPList | Empty)"
              <| fun _ ->
                  let expected = []
                  let actual = oopBubbleSort (listToOOPList [])

                  Expect.equal (OOPListToList actual) expected "Actual result is not sorted properly"

              // №2. Quicksort

              testProperty "Quicksort ConsList<int> | Comparison with system sort"
              <| fun (lst: list<int>) ->
                  let expected = List.sort lst
                  let actual = consQuickSort (listToConsList lst)

                  consListToList actual = expected

              testProperty "Quicksort ConsList<string> | Comparison with system sort"
              <| fun (lst: list<string>) ->
                  let expected = List.sort lst
                  let actual = consQuickSort (listToConsList lst)

                  consListToList actual = expected

              testCase "Quicksort ConsList | Empty)"
              <| fun _ ->
                  let expected = Empty
                  let actual = consQuickSort Empty

                  Expect.equal actual expected "Actual result is not sorted properly"

              testProperty "Quicksort OOPList<int> | Comparison with system sort"
              <| fun (lst: list<int>) ->
                  let expected = List.sort lst
                  let actual = OOPQuickSort(listToOOPList lst)

                  OOPListToList actual = expected

              testProperty "Quicksort OOPList<string> | Comparison with system sort"
              <| fun (lst: list<string>) ->
                  let expected = List.sort lst
                  let actual = OOPQuickSort(listToOOPList lst)

                  OOPListToList actual = expected

              testCase "Quicksort OOPList | Empty)"
              <| fun _ ->
                  let expected = []
                  let actual = OOPQuickSort(listToOOPList [])

                  Expect.equal (OOPListToList actual) expected "Actual result is not sorted properly"

              // №3. Concatenation

              testProperty "Concat ConsList<int> | Comparison with system concat"
              <| fun (lst1: list<int>) (lst2: list<int>) ->
                  let expected = List.concat [ lst1; lst2 ]
                  let actual = ConsList.concat (listToConsList lst1) (listToConsList lst2)

                  consListToList actual = expected

              testProperty "Concat ConsList<string> | Comparison with system concat"
              <| fun (lst1: list<string>) (lst2: list<string>) ->
                  let expected = List.concat [ lst1; lst2 ]
                  let actual = ConsList.concat (listToConsList lst1) (listToConsList lst2)

                  consListToList actual = expected

              testCase "Concat ConsList | Empty lists"
              <| fun _ ->
                  let expected = Empty
                  let actual = ConsList.concat Empty Empty

                  Expect.equal actual expected "Actual result is not Empty"

              testProperty "Concat OOPList<int> | Comparison with system concat"
              <| fun (lst1: list<int>) (lst2: list<int>) ->
                  let expected = List.concat [ lst1; lst2 ]
                  let actual = OOPList.concat (listToOOPList lst1) (listToOOPList lst2)

                  OOPListToList actual = expected

              testProperty "Concat OOPList<string> | Comparison with system concat"
              <| fun (lst1: list<string>) (lst2: list<string>) ->
                  let expected = List.concat [ lst1; lst2 ]
                  let actual = OOPList.concat (listToOOPList lst1) (listToOOPList lst2)

                  OOPListToList actual = expected

              testCase "Concat OOPList | Empty lists"
              <| fun _ ->
                  let expected = EmptyList()
                  let actual = OOPList.concat (listToOOPList []) (listToOOPList [])

                  Expect.equal (OOPListToList actual) (OOPListToList expected) "Actual result is not Empty" ]
