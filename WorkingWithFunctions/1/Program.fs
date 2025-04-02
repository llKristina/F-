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

//9
let main9 digit funct init funcItsNeed=
     let rec step digit rez=
         let itsNeed = funcItsNeed (digit%10)
         let next_digit = digit/10
 
         let next_rez = funct rez (digit%10) 
        
         match (next_digit, itsNeed) with
         | (next_digit,true) when next_digit > 0 -> step next_digit next_rez
         | (next_digit,false) when next_digit > 0 -> step next_digit rez 
         | (next_digit,true) -> next_rez
         | _ -> rez
 
     step digit init
 

 // 11
//let generateResponse (language: string) =
    //match language.ToLower() with
    //| "f#" | "prolog" -> "Ты — подлиза!"
   // | "python" -> "Неплохой выбор, но F# лучше!"
    //| "javascript" -> "Frontend или Node.js?"
    //| "c#" -> "Хороший язык, особенно с .NET"
    //| "ruby" -> "Rails или чистый Ruby?"
    
//13
let rec gcd a b =
    if b = 0 then abs a
    else gcd b (a % b)

let traverseCoprimes n operation init =
    let rec loop i acc =
        if i > n then acc
        else
            if gcd i n = 1 then
                loop (i + 1) (operation acc i)
            else
                loop (i + 1) acc
    loop 1 init

// 14
let eulerPhi n =
    traverseCoprimes n (fun acc _ -> acc + 1) 0

//15 
let isCoprime a b = gcd a b = 1

let main15 number operation initialValue condition =
    let rec processDigits remainingValue currentResult =
        let currentDigit = remainingValue % 10
        let meetsConditions = isCoprime currentDigit number && condition currentDigit
        let nextValue = remainingValue / 10
        
        let updatedResult = 
            match meetsConditions with
            | true -> operation currentResult currentDigit
            | false -> currentResult
        
        match (nextValue, meetsConditions) with
        | (0, true) -> updatedResult
        | (0, false) -> currentResult
        | (_, _) -> processDigits nextValue updatedResult
    
    processDigits number initialValue 

// 16
// Метод 1: Сумма простых делителей числа
let isPrime n =
    if n <= 1 then false
    else
        let rec check i =
            if i * i > n then true
            elif n % i = 0 then false
            else check (i + 1)
        check 2

let sumOfPrimeDivisors n =
    let rec loop divisor acc =
        if divisor > n then acc
        elif n % divisor = 0 && isPrime divisor then
            loop (divisor + 1) (acc + divisor)
        else
            loop (divisor + 1) acc
    loop 2 0

// Метод 2: Количество нечетных цифр числа, больших 3
let countOddDigitsGreater3 n =
    let rec loop num acc =
        if num = 0 then acc
        else
            let digit = num % 10
            let newAcc = if digit > 3 && digit % 2 <> 0 then acc + 1 else acc
            loop (num / 10) newAcc
    loop n 0

// Метод 3: Произведение делителей с суммой цифр меньше исходной
let productOfSpecialDivisors n =
    let originalSum = sumOfDigits n
    let rec loop divisor acc =
        if divisor > n then acc
        elif n % divisor = 0 && sumOfDigits divisor < originalSum then
            loop (divisor + 1) (acc * divisor)
        else
            loop (divisor + 1) acc
    loop 1 1

// 20
let getFunction = function
    | 1 -> sumOfPrimeDivisors   
    | 2 -> countOddDigitsGreater3  
    | 3 -> productOfSpecialDivisors  
    | _ -> failwith "Неверный номер функции. Допустимо: 1, 2, 3"

let processInput (input: string) =
    match input.Split(',') with
    | [|x; y|] -> 
        try 
            let n, arg = int x, int y
            getFunction n arg
            |> string
            |> sprintf "Результат: %s"
        with 
        | :? FormatException -> "Ошибка: введите числа в правильном формате"
        | ex -> $"Ошибка: {ex.Message}"
    | _ -> "Ошибка: введите данные в формате 'номер,число'"
    |> Console.WriteLine

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
  

    // 10
    System.Console.WriteLine("Task 10")
    System.Console.WriteLine(main9 7112 (fun x y -> x+y) 0 (fun x -> match x%2 with |1 -> true |0->false))
    System.Console.WriteLine(main9 271 (fun x y -> x*y) 1 (fun x -> true))
    System.Console.WriteLine(main9 111 (fun x y -> match x>y with| true -> x| false -> y) 0 (fun x -> match x with |x when x>5 -> true|_ -> false))
    System.Console.WriteLine(main9 711 (fun x y -> match x<y with| true -> x| false -> y) 9 (fun x -> false))

    // 12 
    let printAndRead (msg: string) = Console.WriteLine(msg); Console.ReadLine()
    //let processInput (input: string) = generateResponse input |> Console.WriteLine
    //printAndRead "Какой твой любимый язык программирования?" |> processInput

    // 13
    let sumCoprimes10 = traverseCoprimes 10 (fun acc x -> acc + x) 0
    Console.WriteLine($"Сумма взаимно простых с 10: {sumCoprimes10}")
    let productCoprimes15 = traverseCoprimes 15 (fun acc x -> acc * x) 1
    Console.WriteLine($"Произведение взаимно простых с 15: {productCoprimes15}")
    let countCoprimes20 = traverseCoprimes 20 (fun acc _ -> acc + 1) 0
    Console.WriteLine($"Количество взаимно простых с 20: {countCoprimes20}")

    // 14
    Console.WriteLine("Тестирование функции Эйлера φ(n):")
    Console.WriteLine($"φ(5) = {eulerPhi 5} (ожидается 4)")
    Console.WriteLine($"φ(9) = {eulerPhi 9} (ожидается 6)") 

    // 15
    let sumTest = main15 15 (+) 0 (fun x -> x > 3)
    Console.WriteLine($"Сумма цифр >3 и взаимно простых с 15: {sumTest}")  
    let productTest = main15 10 (*) 1 (fun x -> x % 2 = 0)
    Console.WriteLine($"Произведение чётных цифр, взаимно простых с 10: {productTest}")  

    //16
    Console.WriteLine($"1. Сумма простых делителей: {sumOfPrimeDivisors 36}")
    Console.WriteLine($"2. Количество нечётных цифр >3: {countOddDigitsGreater3 35}")
    Console.WriteLine($"3. Произведение делителей с суммой цифр < исходной: {productOfSpecialDivisors 36}")
   
   //20
    Console.WriteLine("Введите номер функции (1-3) и число через запятую:")
    Console.WriteLine("1 - Сумма простых делителей")
    Console.WriteLine("2 - Количество нечетных цифр >3")
    Console.WriteLine("3 - Произведение делителей с суммой цифр < исходной")

    Console.ReadLine() |> processInput
    0 
