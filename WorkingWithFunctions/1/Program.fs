open System

let pi = 3.14159

let areaOfCircle r =
    pi * r * r

let volumeOfCylinder r h =
    areaOfCircle r * h


[<EntryPoint>]
let main argv =

    printfn "Введите радиус круга:"
    let r = float(Console.ReadLine())

    printfn "Введите высоту цилиндра:"
    let h = float(Console.ReadLine())

    let area = areaOfCircle r
    let volume = volumeOfCylinder r h


    printfn "Площадь круга: %.2f" area
    printfn "Объем цилиндра: %.2f" volume

    0 
