open System

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