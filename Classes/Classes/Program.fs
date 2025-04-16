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

//figures |> List.iter (fun f -> f.Print())


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

//figures |> List.iter (fun f -> printfn "%O" f)


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

//printfn "Функтор map (удвоение): %A" (ListMonad.map double list)
//printfn "Аппликативный apply (декартово произведение): %A" (ListMonad.apply [double; (fun x -> x + 100)] list)
//printfn "Монада bind (размножение элементов): %A" (ListMonad.bind multiplyAndAdd list)
//printfn "Функтор - закон идентичности: %b" (ListMonad.Laws.Identity list)
//printfn "Функтор - закон композиции: %b" (ListMonad.Laws.Composition((+)1) double list)
//printfn "Аппликативный - закон гомоморфизма: %b" (ListMonad.Laws.ApplicativeHomomorphism double 5)
//printfn "Монада - левосторонняя идентичность: %b" (ListMonad.Laws.MonadLeftIdentity multiplyAndAdd 5)

// lab 6
open FParsec

type BoolExpr =
    | True
    | False
    | And of BoolExpr * BoolExpr
    | Or of BoolExpr * BoolExpr

let ptrue = stringReturn "true" True
let pfalse = stringReturn "false" False

let pexpr, pexprRef = createParserForwardedToRef() 

let pterm = 
    choice [
        ptrue
        pfalse
        between (pchar '(') (pchar ')') pexpr
    ]

let pand = stringReturn " AND " (fun x y -> And(x, y))
let por = stringReturn " OR " (fun x y -> Or(x, y))

do pexprRef := chainl1 pterm (pand <|> por) 

let parseBoolExpr input =
    match run pexpr input with
    | Success(result, _, _) -> result
    | Failure(error, _, _) -> failwithf "Ошибка разбора: %s" error

let expr1 = parseBoolExpr "true AND false"
let expr2 = parseBoolExpr "(true OR false) AND true"

//printfn "%A" expr1  
//printfn "%A" expr2  

//lab 7
type AgentMessage =
    | Greet of string
    | Calculate of int * int
    | GetRandomNumber of AsyncReplyChannel<int>
    | Shutdown

let agent = MailboxProcessor<AgentMessage>.Start(fun inbox ->
    let rec loop () = async {
        let! msg = inbox.Receive()
        
        match msg with
        | Greet name -> 
            printfn "Привет, %s!" name
            return! loop()
            
        | Calculate (a, b) ->
            printfn "Результат сложения %d и %d: %d" a b (a + b)
            return! loop()
            
        | GetRandomNumber replyChannel ->
            let rnd = Random()
            replyChannel.Reply(rnd.Next(1, 100))
            return! loop()
            
        | Shutdown ->
            printfn "Агент завершает работу..."
            return () 
    }
    loop ()
)

//agent.Post(Greet "Иван")
//agent.Post(Calculate(5, 7))
//async {
//    let! number = agent.PostAndAsyncReply(GetRandomNumber)
//    printfn "Получено случайное число: %d" number
//} |> Async.Start
//Async.Sleep 100 |> Async.RunSynchronously
//agent.Post(Shutdown)

//task 5
open System
open System.Text.RegularExpressions

type Passport
    (
        series: string,
        number: string,
        issueDate: string,
        issuedBy: string,
        departmentCode: string,
        firstName: string,
        lastName: string,
        middleName: string option,
        birthDate: string,
        birthPlace: string
    ) =

    member val Series = series with get, set
    member val Number = number with get, set
    member val IssueDate = issueDate with get, set
    member val IssuedBy = issuedBy with get, set
    member val DepartmentCode = departmentCode with get, set
    member val FirstName = firstName with get, set
    member val LastName = lastName with get, set
    member val MiddleName = middleName with get, set
    member val BirthDate = birthDate with get, set
    member val BirthPlace = birthPlace with get, set

    member this.Validate() =
        let datePattern = @"^\d{2}\.\d{2}\.\d{4}$"       
        let seriesPattern = @"^\d{4}$"
        let numberPattern = @"^\d{6}$"
        let depCodePattern = @"^\d{3}-\d{3}$"            
        let namePattern = @"^[А-Яа-яA-Za-z\-]+$"

        Regex.IsMatch(this.Series, seriesPattern) &&
        Regex.IsMatch(this.Number, numberPattern) &&
        Regex.IsMatch(this.IssueDate, datePattern) &&
        Regex.IsMatch(this.BirthDate, datePattern) &&
        Regex.IsMatch(this.DepartmentCode, depCodePattern) &&
        Regex.IsMatch(this.FirstName, namePattern) &&
        Regex.IsMatch(this.LastName, namePattern) &&
        (match this.MiddleName with
         | Some mn -> Regex.IsMatch(mn, namePattern)
         | None -> true)


    member this.CompareTo(other: Passport) =
        let key1 = this.Series + this.Number
        let key2 = other.Series + other.Number
        String.Compare(key1, key2)

 
    override this.ToString() =
        let mid = match this.MiddleName with Some m -> m | None -> ""
        sprintf "Паспорт гражданина РФ\nФИО: %s %s %s\nДата рождения: %s\nМесто рождения: %s\nСерия: %s\nНомер: %s\nДата выдачи: %s\nКем выдан: %s\nКод подразделения: %s"
            this.LastName this.FirstName mid this.BirthDate this.BirthPlace this.Series this.Number this.IssueDate this.IssuedBy this.DepartmentCode


[<EntryPoint>]
let main argv =
    let passport = Passport(
        "1234", "567890", "15.04.2015", "ОУФМС России по г. Москве", "770-001",
        "Иван", "Иванов", Some "Иванович", "01.01.1990", "г. Москва"
    )

    printfn "%s" (passport.ToString())

    if passport.Validate() then
        printfn "Паспорт прошёл валидацию"
    else
        printfn "Паспорт НЕ прошёл валидацию"

    0
