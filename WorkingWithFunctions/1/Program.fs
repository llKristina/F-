open System

let pi = 3.14159

let areaOfCircle r =
    pi * r * r

let volumeOfCylinder r h =
     r * h

let rec sumOfDigits n =
    if n = 0 then
        0  
    else
        n % 10 + sumOfDigits (n / 10)


[<EntryPoint>]
let main argv =

    printfn "Введите радиус круга:"
    let r = Console.ReadLine() |> float

    printfn "Введите высоту цилиндра:"
    let h = Console.ReadLine() |> float

    let v =  r |> areaOfCircle |> volumeOfCylinder h 
    System.Console.WriteLine(v) 

    let n = 3456
    let sum1 = sumOfDigits n
    System.Console.WriteLine($"Рекурсия вверх: {sum1}") 

    0 
