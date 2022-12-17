namespace Homework.Task1Tests

open System
open Expecto
open Homework.Task1

module TestCases =

    //[<Tests>]
    let tests =

        testList
            "samples"

            // №1, №2
            // Property tests

            [ testProperty "Exponentiation | Naive and quick should return equal results (Bytes)"
              <| fun (x: byte) y ->
                  let naive = exp x y
                  let quick = quickExp x y

                  naive = quick

              testProperty "Exponentiation | Naive and quick should return equal results (Ints)"
              <| fun (x: int) y ->
                  let naive = exp x y
                  let quick = quickExp x y

                  naive = quick

              testProperty "Exponentiation | Naive and quick should return equal results (Floats)"
              <| fun (x: float) y ->
                  let naive = exp x y
                  let quick = quickExp x y

                  if
                      (Double.IsInfinity naive && Double.IsInfinity quick)
                      || (Double.IsNaN naive && Double.IsNaN quick)
                  then
                      Expect.isTrue true
                  else
                      Expect.floatClose Accuracy.high naive quick

              // Test cases

              testCase "Exponentiation | Natural value, natural power"
              <| fun _ ->
                  let naive = exp 2 10
                  let quick = quickExp 2 10

                  Expect.equal naive quick "2 ** 10 = 1024"

              testCase "Exponentiation | Natural value, power is 0"
              <| fun _ ->
                  let naive = exp 2 0
                  let quick = quickExp 2 0

                  Expect.equal naive quick "2 ** 0 = 1"

              testCase "Exponentiation | Natural value, power is 1"
              <| fun _ ->
                  let naive = exp 2 1
                  let quick = quickExp 2 1

                  Expect.equal naive quick "2 ** 1 = 2"

              testCase "Exponentiation | Value is 0, natural power"
              <| fun _ ->
                  let naive = exp 0 10
                  let quick = quickExp 0 10

                  Expect.equal naive quick "0 ** 10 = 0"

              testCase "Exponentiation | Negative value, natural power"
              <| fun _ ->
                  let naive = exp -2 10
                  let quick = quickExp -2 10

                  Expect.equal naive quick "-2 ** 10 = 1024"

              testCase "Exponentiation | Bytes"
              <| fun _ ->
                  let naive = exp 2uy 7
                  let quick = quickExp 2uy 7

                  Expect.equal naive quick "2 ** 7 = 128"

              testCase "Exponentiation | Ints"
              <| fun _ ->
                  let naive = exp 2 30
                  let quick = quickExp 2 30

                  Expect.equal naive quick "2 ** 30 = 1 073 741 824"

              testCase "Exponentiation | Floats"
              <| fun _ ->
                  let naive = exp 2.0 60
                  let quick = quickExp 2.0 60

                  Expect.equal naive quick "2 ** 60 = 1,15 × 10^18"

              // №3

              testProperty "Difference | Max - Min (Ints)"
              <| fun (arr: int array) ->
                  if arr.Length = 0 then
                      true
                  else
                      let expected = Array.max arr - Array.min arr
                      let actual = diff arr

                      actual = expected

              testProperty "Difference | Max - Min (Floats)"
              <| fun (arr: float array) ->
                  let arr' =
                      Array.filter (fun elem -> not (Double.IsInfinity elem) && not (Double.IsNaN elem)) arr

                  if arr'.Length = 0 then
                      true
                  else
                      let expected = Array.max arr' - Array.min arr'
                      let actual = diff arr'

                      expected = actual

              testProperty "Difference | One element"
              <| fun x ->
                  let expected = x - x
                  let actual = diff [| x |]

                  expected = actual

              testCase "Difference | Bytes"
              <| fun _ ->
                  let actual = diff [| 34uy; 5uy; 13uy; 0uy; 67uy; 128uy |]
                  let expected = 128uy

                  Expect.equal actual expected "128 - 0 = 128"

              testCase "Difference | Ints"
              <| fun _ ->
                  let actual = diff [| 43; -689; 3; 0; 573; 93 |]
                  let expected = 1262

                  Expect.equal actual expected "573 - (-689) = 1262"

              testCase "Difference | Floats"
              <| fun _ ->
                  let actual = diff [| 0.1; -49.3; 9.7; -293.5; -4.4; 344.5 |]
                  let expected = 638.0

                  Expect.equal actual expected "344.5 - (-293.5) = 638"

              // №4

              testProperty "Odds | Number of elements"
              <| fun x y ->
                  if x > y then
                      true
                  else
                      let actual = odds x y

                      let expected =

                          if x % 2 = 0 && y % 2 = 0 then
                              (y - x) / 2
                          else
                              (y - x) / 2 + 1

                      actual.Length = expected

              testProperty "Odds | One element"
              <| fun x ->
                  let actual = odds x x

                  if abs (x) % 2 = 1 then
                      let expected = [| x |]

                      actual = expected
                  else
                      let expected = [||]
                      actual = expected

              testCase "Odds | Negative"
              <| fun _ ->
                  let actual = odds -10 -1
                  let expected = [| -9; -7; -5; -3; -1 |]

                  Expect.equal actual expected "Result is not correct"

              testCase "Odds | Positive"
              <| fun _ ->
                  let actual = odds 1 10
                  let expected = [| 1; 3; 5; 7; 9 |]

                  Expect.equal actual expected "Result is not correct"

              testCase "Odds | Negative and Positive"
              <| fun _ ->
                  let actual = odds -10 10
                  let expected = [| -9; -7; -5; -3; -1; 1; 3; 5; 7; 9 |]


                  Expect.equal actual expected "Result is not correct" ]
