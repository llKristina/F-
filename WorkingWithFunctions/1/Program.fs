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

let tailDigitalSum n : int =
     let rec digitalSubSum n currentSum = 
         if n = 0 then currentSum
         else
             let currentNum = n / 10
             let digital = n % 10
             let accumulator = currentSum + digital
             digitalSubSum currentNum accumulator
     digitalSubSum n 0


let rec quanDelVV x index=
    match index with
    | index when (x%index = 0 && index < x) -> 1 + quanDelVV x (index+1)
    | index when (index >= x) -> 0
    | _ -> 0 + quanDelVV x (index+1)
        
let quanDelVN x = 
    let rec quanDelVN x index sum =
        let isNeed = (x%index = 0) && (index < x)
        let new_sum = sum + 1
        match isNeed with
            | true -> quanDelVN x (index+1) (new_sum)
            | false when index < x -> quanDelVN x (index+1) sum
            | _ -> sum
    quanDelVN x 1 0

// 7

let main7 digit funct init =
    let rec step digit rez=
        let next_rez = funct rez (digit%10) 
        let next_digit = digit/10
   
        match next_digit with
        | next_digit when next_digit > 0 -> step next_digit next_rez
        | _ -> next_rez

    step digit init
    


[<EntryPoint>]
let main argv =

    //printfn "Введите радиус круга:"
    //let r = Console.ReadLine() |> float

    //printfn "Введите высоту цилиндра:"
    //let h = Console.ReadLine() |> float

    //let v =  r |> areaOfCircle |> volumeOfCylinder h 
    //System.Console.WriteLine(v) 

    //let n = 3456
    //let sum1 = sumOfDigits n
    //System.Console.WriteLine($"Рекурсия вверх: {sum1}") 

    //let sum2 = tailDigitalSum n
    //Console.WriteLine($"Рекурсия вниз: {sum2}")

    System.Console.WriteLine(quanDelVV 10 1)

    System.Console.WriteLine(quanDelVN 10)
    
    // 8
    let sum a b = a+b
    System.Console.WriteLine("Task 7")
    System.Console.WriteLine(main7 711 (fun x y -> x+y) 0)
    System.Console.WriteLine(main7 271 (fun x y -> x*y) 1)
    System.Console.WriteLine(main7 711 (fun x y -> match x>y with| true -> x| false -> y) 0)
    System.Console.WriteLine(main7 711 (fun x y -> match x<y with| true -> x| false -> y) 9)
  
    0 
