namespace Homework.Tests

open System
open Expecto
open Homework
open Homework.Task1

module TestCase =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Exponentiation | Natural value, natural power"
              <| fun _ ->
                  let naive = exp 2 10
                  let quick = quickExp 2 10

                  if naive <> quick then
                      failwith "Naive is not equal to quick"

                  Expect.equal naive 1024 "2 ** 10 = 1024"

              testCase "Exponentiation | Natural value, power is 0"
              <| fun _ ->
                  let naive = exp 2 0
                  let quick = quickExp 2 0

                  if naive <> quick then
                      failwith "Naive is not equal to quick"

                  Expect.equal naive 1 "2 ** 0 = 1"

              testCase "Exponentiation | Natural value, power is 1"
              <| fun _ ->
                  let naive = exp 2 1
                  let quick = quickExp 2 1

                  if naive <> quick then
                      failwith "Naive is not equal with quick"

                  Expect.equal naive 2 "2 ** 1 = 2"

              testCase "Exponentiation | Value is 0, natural power"
              <| fun _ ->
                  let naive = exp 0 10
                  let quick = quickExp 0 10

                  if naive <> quick then
                      failwith "Naive is not equal with quick"

                  Expect.equal naive 0 "0 ** 10 = 0"

              testCase "Exponentiation | Negative value, natural power"
              <| fun _ ->
                  let naive = exp -2 10
                  let quick = quickExp -2 10

                  if naive <> quick then
                      failwith "Naive is not equal with quick"

                  Expect.equal naive 1024 "-2 ** 10 = 1024"

              testCase "Exponentiation | Natural value, negative power"
              <| fun _ ->
                  let naive = exp 2 -1
                  let quick = quickExp 2 -1

                  if naive <> quick then
                      failwith "Naive is not equal with quick"

                  Expect.equal naive 0.5 "2 ** -1 = 0.5"

              testCase "Exponentiation | Big result"
              <| fun _ ->
                  let naive = exp 11 11
                  let quick = quickExp 11 11

                  if naive <> quick then
                      failwith "Naive is not equal with quick"

                  Expect.equal naive 285311670611.0 "11 ** 11 = 285311670611"

              testCase "Difference | Random numbers"
              <| fun _ ->
                  let subject = diff [| 43; -689; 3; 0; 573; 93 |]

                  Expect.equal subject 1262 "573 - (-689) = 1262"

              testCase "Difference | One number"
              <| fun _ ->
                  let subject = diff [| System.Random().Next(-1000, 1000) |]

                  Expect.equal subject 0 "Obvious"

              testCase "Odds"
              <| fun _ ->
                  let subject = odds -10 10

                  Expect.equal subject [| -9; -7; -5; -3; -1; 1; 3; 5; 7; 9 |] "Result is not correct" ]
