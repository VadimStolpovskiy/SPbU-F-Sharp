module Homework.Task1

// №1
let exp value pow =

    let mutable result = 1.0

    for i = 1 to abs pow do
        result <- value * result

    if pow > 0 then result else 1.0 / result

// №2
let rec quickExp value pow =

    let result =
        if pow = 0 then
            1.0
        else
            let half = quickExp value (abs pow / 2)

            if pow % 2 = 0 then half * half else value * half * half

    if pow > 0 then result else 1.0 / result

// №3
let diff (arr: int array) =

    if arr.Length = 0 then
        failwith "The array is empty"

    let mutable tempMin = arr[0]
    let mutable tempMax = arr[0]

    for i = 0 to arr.Length - 1 do
        tempMin <- min tempMin arr[i]
        tempMax <- max tempMax arr[i]

    tempMax - tempMin

// №4
let odds a b =

    if a % 2 = 1 then [| a..2..b |] else [| a + 1 .. 2 .. b |]
