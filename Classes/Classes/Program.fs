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