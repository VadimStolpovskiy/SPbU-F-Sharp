module Homework.Task1

// №1
let exp (value: float) pow : float =
    let mutable result = 1.0

    if pow < 0 then
        for i = pow to -1 do
            result <- 1.0 / (value * result)
    else
        for i = 1 to pow do
            result <- value * result

    result

// №2
let rec quickExp (value: float) pow : float =
    if pow = 1 then
        value
    elif pow = 0 then
        1
    else
        let result = quickExp value (pow / 2)

        if pow % 2 = 0 then
            if pow < 0 then 1.0 / (result * result) else result * result
        else if pow < 0 then
            1.0 / (value * result * result)
        else
            value * result * result


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
