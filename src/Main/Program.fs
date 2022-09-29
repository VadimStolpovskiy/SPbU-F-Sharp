namespace Main
open System

module Program =
    
    // Task 1
    
    let exp b n =
            let mutable result = 1
            for i = 1 to n do
                  result <- b * result
            result
    
    // Task 2        
    
    let quickExp b n = b ** n
    
    // Task 3
    let min x y =
        if x < y then x
        else y
    
    let max x y =
        if x > y then x
        else y
    
    let diff (arr: int array) =
        let mutable Min = arr[0]
        let mutable Max = arr[0]
        
        for i = 0 to arr.Length - 1 do
            Min <- min Min arr[i]
            Max <- max Max arr[i]
        Max - Min
        
     // Task 4
    
    let odds a b =
        let filter = 
            [|a..b|]
            |> Array.filter (fun elem -> elem % 2 = 1)
        filter
    
    [<EntryPoint>]
    let main (argv: string array) = 0
