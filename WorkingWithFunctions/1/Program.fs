open System

let pi = 3.14159

let areaOfCircle r =
    pi * r * r

let volumeOfCylinder r h =
    areaOfCircle r * h

let curryCylinderVolume h = 
     fun r -> volumeOfCylinder h r

[<EntryPoint>]
let main argv =

    printfn "Введите радиус круга:"
    let r = Console.ReadLine() |> float

    printfn "Введите высоту цилиндра:"
    let h = Console.ReadLine() |> float

    let area = areaOfCircle r
    let volume = volumeOfCylinder r h


    printfn "Площадь круга: %.2f" area
    printfn "Объем цилиндра: %.2f" volume

  
    0 
