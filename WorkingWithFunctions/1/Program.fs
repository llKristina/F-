open System

let pi = 3.14159

let areaOfCircle r =
    pi * r * r

let volumeOfCylinder r h =
     r * h


[<EntryPoint>]
let main argv =

    printfn "Введите радиус круга:"
    let r = Console.ReadLine() |> float

    printfn "Введите высоту цилиндра:"
    let h = Console.ReadLine() |> float

    let v =  r |> areaOfCircle |> volumeOfCylinder h 
    System.Console.WriteLine(v) 

  
    0 
