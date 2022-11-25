namespace Homework.Tests

open Expecto
open Homework.Task1

module TestCases =

    [<Tests>]
    let tests =

        testList
            "samples"

            // №1, №2

            [ testProperty "Exponentiation | Naive and quick should return equal results"
              <| fun x (y: int) ->
                  if x <> 0 && y <> 0 then
                      quickExp x y = exp x y
                  else
                      skiptest "Incorrect arguments"

              testProperty "Exponentiation | Product of Powers"
              <| fun a (m: int) (n: int) ->
                  if a <> 0 && m <> 0 && n <> 0 then
                      quickExp a m * quickExp a n = quickExp a (m + n)
                  else
                      skiptest "Incorrect arguments"

              testProperty "Exponentiation | Quotient Property"
              <| fun a (m: int) (n: int) ->
                  if a <> 0 && m <> 0 && n <> 0 then
                      quickExp a m / quickExp a n = quickExp a (m - n)
                  else
                      skiptest "Incorrect arguments"

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

              testCase "Exponentiation | Natural value, negative power"
              <| fun _ ->
                  let naive = exp 2 -1
                  let quick = quickExp 2 -1

                  Expect.equal naive quick "2 ** -1 = 0.5"

              testCase "Exponentiation | Big result"
              <| fun _ ->
                  let naive = exp 11 11
                  let quick = quickExp 11 11

                  Expect.equal naive quick "11 ** 11 = 285311670611"

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

              testCase "Difference | Ints"
              <| fun _ ->
                  let actual = diff [| 43; -689; 3; 0; 573; 93 |]

                  Expect.equal actual 1262 "573 - (-689) = 1262"

              testCase "Difference | Floats"
              <| fun _ ->
                  let actual = diff [| 0.1; -49.3; 9.7; -293.5; -4.4; 344.5 |]

                  Expect.equal actual 638.0 "344.5 - (-293.5) = 638"

              // №4

              testCase "Odds"
              <| fun _ ->
                  let subject = odds -10 10

                  Expect.equal subject [| -9; -7; -5; -3; -1; 1; 3; 5; 7; 9 |] "Result is not correct" ]
