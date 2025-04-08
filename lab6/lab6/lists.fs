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