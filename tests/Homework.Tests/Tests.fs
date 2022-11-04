namespace Homework.Tests

open Expecto
open Homework.Task1

module TestCases =

    [<Tests>]
    let tests =

        testList
            "samples"

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

              testCase "Difference | Random numbers"
              <| fun _ ->
                  let subject = diff [| 43; -689; 3; 0; 573; 93 |]

                  Expect.equal subject 1262 "573 - (-689) = 1262"

              testProperty "Difference | One number"
              <| fun x ->
                  let expected = 0
                  let actual = diff [| x |]

                  Expect.equal expected actual "Obvious"

              testCase "Odds"
              <| fun _ ->
                  let subject = odds -10 10

                  Expect.equal subject [| -9; -7; -5; -3; -1; 1; 3; 5; 7; 9 |] "Result is not correct" ]
