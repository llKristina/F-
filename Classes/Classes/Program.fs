open System

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

type Square(side: float) =
    inherit Rectangle(side, side) 
    member this.Side = side
    override this.ToString() =
        sprintf "Квадрат [Сторона: %.2f, Площадь: %.2f]" 
            this.Side this.Area

let pi = 3.14159
type Circle(radius: float) =
    inherit GeometricFigure() 
    member this.Radius = radius
    override this.Area = 
        pi * radius * radius
        override this.ToString() =
        sprintf "Круг [Радиус: %.2f, Площадь: %.2f]" 
            this.Radius this.Area

let figures: GeometricFigure list = [
    Rectangle(5.0, 3.0)
    Square(4.0)
    Circle(2.5)
]

figures |> List.iter (fun fig -> printfn "%O" fig)