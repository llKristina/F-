//задание 6
type Tree =
    | Node of string * Tree * Tree
    | Empty

let rec add value tree =
    match tree with
    | Empty -> Node(value,Empty,Empty)
    | Node(data, left, right) when value < data -> Node(data, add value left, right)
    | Node(data, left, right) when value > data -> Node(data, left, add value right)
    |_ -> tree

let rec search value tree =
    match tree with
    | Empty -> false
    | Node(data, left, right) when value = data -> true 
    | Node(data, left, right) when value < data -> search value left
    | Node(data, left, right) when value > data -> search value right

let myTree =
    Empty
    |> add "cat"
    |> add "dog"
    |> add "apple"
    |> add "banana"

let found = search "dog" myTree
let notFound = search "elephant" myTree

printfn "Found 'dog': %b" found
printfn "Found 'elephant' %b" notFound

//задание 7
let mostFrequent lst =
    lst
    |> List.countBy id
    |> List.maxBy snd
    |> fst

let lst = [1;2;3;2;1;2;2]
let result = mostFrequent lst
printfn "%A" result 

//8 задание
let countSquares lst =
    let uniqueElements = List.distinct lst
    lst
    |>List.filter (fun x -> uniqueElements |> List.exists (fun y -> y* y=x))
    |>List.length

let list =[2;4;3;16;5]
let res = countSquares list
printfn "%A" res

//9 задание
let sumOfDigits n =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ -> loop (num / 10) (acc + num % 10)
    loop (abs n) 0

let countDivisors n =
    match n with
    | 0 -> 0
    | _ ->
        let nAbs = abs n
        [1..nAbs] |> List.filter (fun x -> nAbs % x = 0) |> List.length

let createTuples (listA: int list) (listB: int list) (listC: int list) =
    let processedA = 
        listA 
        |> List.sortDescending
        |> List.mapi (fun i x -> (i, x))
    
    let processedB =
        listB
        |> List.sortBy (fun x -> (sumOfDigits x, -abs x))
        |> List.mapi (fun i x -> (i, x))
    
    let processedC =
        listC
        |> List.sortByDescending (fun x -> (countDivisors x, abs x))
        |> List.mapi (fun i x -> (i, x))
    
    let combined =
        List.zip3 processedA processedB processedC
        |> List.map (fun ((ia, a), (ib, b), (ic, c)) ->
            match (ia = ib, ib = ic) with
            | (true, true) -> (a, b, c)
            | _ -> failwith "Списки имеют разную длину")
    
    combined

let listA = [10; 5; 8; 3]
let listB = [23; 45; 19; 32]
let listC = [12; 7; 24; 15]

let result1 = createTuples listA listB listC
printfn "%A" result1

//10 задание
//let listSort () =
//   printfn "Введите строки"
//    let rec readLines acc =
//        match System.Console.ReadLine() with 
//        | "0" -> acc
//        | line -> readLines (line::acc)
//
//    let stringList = readLines [] |> List.rev

//    stringList
//    |> List.sortBy (fun s -> s.Length)
//    |> List.iter (printfn "%s")

//listSort ()

//11 задание
let countAfterMax1 lst =
    let maxVal = List.max lst
    lst 
    |> List.rev
    |> List.takeWhile (fun x -> x <> maxVal)
    |> List.length

let countAfterMax2 lst =
    let rec loop acc maxVal remaining =
        match remaining with
        | [] -> acc
        | h::t ->
            let newMax = max h maxVal
            if h = newMax then loop 0 newMax t
            else loop (acc + 1) newMax t
    loop 0 (List.head lst) (List.tail lst)

let test1 = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
let t1 = countAfterMax1 test1 
let t2 = countAfterMax2 test1
printfn "Элементы после последнего максимума: %A и %A" t1 t2
//12 задание
let findUnique1 lst =
    lst
    |> List.groupBy id
    |> List.find (snd >> List.length >> (=) 1)
    |> fst

let findUnique2 lst =
    let rec loop a b = function
        | [] -> if a <> b then a else b
        | h::t -> if h <> a && h <> b then h else loop a b t
    match lst with
    | x::y::z_ when x = y -> loop x y lst
    | _ -> lst |> List.find (fun x -> x <> lst.[0])

let test2 = [5;5;5;3;5]
let t11 = findUnique1 test2 
let t12 = findUnique2 test2 
printfn "Уникальный элемент: %A и %A" t11 t12


//13 задание
let elementsAfterFirstMax1 lst =
    let maxVal = List.max lst
    lst 
    |> List.skipWhile (fun x -> x <> maxVal)
    |> List.tail

let elementsAfterFirstMax2 lst =
    let rec loop found acc = function
        | [] -> List.rev acc
        | h::t when not found && h = List.max lst -> loop true [] t
        | h::t when found -> loop found (h::acc) t
        | _::t -> loop found acc t
    loop false [] lst

let list1 = [3; 5; 2; 5; 7; 1]
let t21 = elementsAfterFirstMax1 list1 
let t22 = elementsAfterFirstMax2 list1 
printfn "Элементы после первого максимума: %A и %A" t21 t22

//14 задание 
let countEven1 = List.filter (fun x -> x % 2 = 0) >> List.length

let countEven2 lst =
    let rec loop acc = function
        | [] -> acc
        | h::t -> loop (if h % 2 = 0 then acc + 1 else acc) t
    loop 0 lst

let list2 = [2; 1; 4; 6; 3; 8]
let t31 = list2 |> countEven1 
let t32 = countEven2 list2    
printfn "Количество чётных: %A и %A" t31 t32

//15 задание
let averageAbs1 = List.averageBy (float << abs)

let averageAbs2 lst =
    let rec loop sum cnt = function
        | [] -> sum / float cnt
        | h::t -> loop (sum + float (abs h)) (cnt + 1) t
    loop 0.0 0 lst

let list3 = [-3; 4; -2; 5]
let t41 = averageAbs1 list3  
let t42 = averageAbs2 list3  
printfn "Среднее модулей: %A и %A" t41 t42

//16 задание
let frequencyLists1 lst =
    let grouped = lst |> List.countBy id
    (grouped |> List.map fst, grouped |> List.map snd)

let frequencyLists2 lst =
    let rec build acc = function
        | [] -> acc
        | h::t -> 
            if List.exists (fst >> (=) h) acc 
            then build (List.map (fun (x,c) -> if x = h then (x,c+1) else (x,c)) acc) t
            else build ((h,1)::acc) t
    let result = build [] lst |> List.rev
    (List.map fst result, List.map snd result)

let list4 = [1; 2; 1; 3; 2; 2; 4]
let (l1_1, l2_1) = frequencyLists1 list4 
let (l1_2, l2_2) = frequencyLists2 list4 
printfn "Частотные списки:\nL1: %A L2: %A\nL1: %A L2: %A" l1_1 l2_1 l1_2 l2_2

//18 задание
let mas (arr: 'a[]) = Array.rev arr
mas [|"П";"р";"и";"в";"е";"т"|] |> printfn "%A"

//19 задание
let countRussianCharacters (input: string) =
    input
    |> Seq.filter (fun c ->
        (c >= 'А' && c <= 'Я') || 
        (c >= 'а' && c <= 'я') || 
        c = 'Ё' || 
        c = 'ё')
    |> Seq.length

let testString1 = "Hello, мир!"
printfn "Количество символов: %d" (countRussianCharacters testString1)  

//20 задание
let vowels = set ['а'; 'е'; 'ё'; 'и'; 'о'; 'у'; 'ы'; 'э'; 'ю'; 'я';
                   'А'; 'Е'; 'Ё'; 'И'; 'О'; 'У'; 'Ы'; 'Э'; 'Ю'; 'Я']

let countLetters (str: string) =
    let mutable vowelsCount = 0
    let mutable consonantsCount = 0
    
    for c in str do
        if System.Char.IsLetter(c) then
            if vowels.Contains(c) then vowelsCount <- vowelsCount + 1
            else consonantsCount <- consonantsCount + 1
    
    (vowelsCount, consonantsCount)

let calculateDifference (v, c) =
    let total = v + c
    if total = 0 then 0.0
    else (float c / float total) - (float v / float total)

let sortStrings (strings: string list) =
    strings
    |> List.map (fun s -> 
        let counts = countLetters s
        (s, calculateDifference counts))
    |> List.sortBy snd
    |> List.map fst

let testStrings = [
    "Привет, мир!"               
    "Программирование на F#"      
    "АБВГДЕЁЖЗИЙ"                 
    "12345!@#$%"                  
    "ООО Ааа"                    
]

let sorted = sortStrings testStrings

sorted |> List.iter (printfn "%s")

