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
countAfterMax1 test1 
countAfterMax2 test1

