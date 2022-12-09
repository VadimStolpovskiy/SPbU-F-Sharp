module Homework.Task1

// №1
let inline exp value pow =

    let mutable result = LanguagePrimitives.GenericOne

    for i = 1 to abs pow do
        result <- value * result

    result

// №2
let inline quickExp value pow =

    let rec loop pow =
        let result =
            if pow = 0 then
                LanguagePrimitives.GenericOne
            else
                let half = loop (abs pow / 2)

                if pow % 2 = 0 then half * half else value * half * half

        result

    loop pow

// №3
let inline diff (arr: 'a array) =

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

    if abs (a) % 2 = 1 then
        [| a..2..b |]
    else
        [| a + 1 .. 2 .. b |]
