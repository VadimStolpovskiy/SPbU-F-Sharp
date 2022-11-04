namespace Homework.Tests

module ExpectoTemplate =

    open Expecto
    open System
    open Homework.Task1

    [<EntryPoint>]
    let main argv =

        runTestsInAssembly defaultConfig argv
