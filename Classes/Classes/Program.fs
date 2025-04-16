open System
//lab 4
type IPrint =
    abstract member Print : unit -> unit

[<AbstractClass>]
type GeometricFigure() =
    abstract member Area : float

type Rectangle(width: float, height: float) =
    inherit GeometricFigure()
    member this.Width = width
    member this.Height = height
    override this.Area = 
        this.Width * this.Height
     override this.ToString() =
        sprintf "Прямоугольник [Ширина: %.2f, Высота: %.2f, Площадь: %.2f]" 
            this.Width this.Height this.Area
    interface IPrint with
        member this.Print() = printfn "%O" this

type Square(side: float) =
    inherit Rectangle(side, side) 
    member this.Side = side
    override this.ToString() =
        sprintf "Квадрат [Сторона: %.2f, Площадь: %.2f]" 
            this.Side this.Area
    interface IPrint with
        member this.Print() = printfn "%O" this

let pi = 3.14159
type Circle(radius: float) =
    inherit GeometricFigure() 
    member this.Radius = radius
    override this.Area = 
        pi * radius * radius
        override this.ToString() =
        sprintf "Круг [Радиус: %.2f, Площадь: %.2f]" 
            this.Radius this.Area
    interface IPrint with
        member this.Print() = printfn "%O" this

let figures: IPrint  list = [
    Rectangle(5.0, 3.0)
    Square(4.0)
    Circle(2.5)
]

figures |> List.iter (fun f -> f.Print())


type GeometricFigure2 =
    | Rectangle of width: float * height: float
    | Square of side: float
    | Circle of radius: float
    member this.Area =
        match this with
        | Rectangle(w, h) -> w * h
        | Square(s) -> s * s
        | Circle(r) -> pi * r * r
    override this.ToString() =
        match this with
        | Rectangle(w, h) -> 
            sprintf "Прямоугольник [ширина: %.2f, высота: %.2f, площадь: %.2f]" w h this.Area
        | Square(s) -> 
            sprintf "Квадрат [сторона: %.2f, площадь: %.2f]" s this.Area
        | Circle(r) -> 
            sprintf "Круг [радиус: %.2f, площадь: %.2f]" r this.Area

figures 
|> List.map (fun shape -> 
    let area = calculateArea shape
    match shape with
    | Rectangle(w, h) -> $"Прямоугольник {w}x{h}: площадь = {area:.2f}"
    | Square(s) -> $"Квадрат {s}x{s}: площадь = {area:.2f}"
    | Circle(r) -> $"Круг r={r}: площадь = {area:.2f}")
|> List.iter (printfn "%s")


//lab 5
module ListMonad =
    let map f list = List.map f list

    let apply fs xs =
        [ for f in fs do
          for x in xs do
              yield f x ]

    let pure' x = [x]

    let bind f list = List.collect f list
    let return' x = [x]

    module Laws =
        let Identity list =
            map id list = list

        let Composition f g list =
            map (f >> g) list = (map f >> map g) list

        let licativeIdentity list =
            apply (pure' id) list = list

        let ApplicativeHomomorphism f x =
            apply (pure' f) (pure' x) = pure' (f x)

        let ApplicativeInterchange f x =
            apply f (pure' x) = apply (pure' (fun g -> g x)) f

        let MonadLeftIdentity f x =
            bind f (return' x) = f x

        let MonadRightIdentity list =
            bind return' list = list

let list = [10; 2; 13; 6; 15]
let double x = x * 2
let multiplyAndAdd x = [x; x * 10]

printfn "Функтор map (удвоение): %A" (ListMonad.map double list)
printfn "Аппликативный apply (декартово произведение): %A" (ListMonad.apply [double; (fun x -> x + 100)] list)
printfn "Монада bind (размножение элементов): %A" (ListMonad.bind multiplyAndAdd list)
printfn "Функтор - закон идентичности: %b" (ListMonad.Laws.Identity list)
printfn "Функтор - закон композиции: %b" (ListMonad.Laws.Composition((+)1) double list)
printfn "Аппликативный - закон гомоморфизма: %b" (ListMonad.Laws.ApplicativeHomomorphism double 5)
printfn "Монада - левосторонняя идентичность: %b" (ListMonad.Laws.MonadLeftIdentity multiplyAndAdd 5)