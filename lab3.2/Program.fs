open System

let endsWithDigit number digit =
    abs number % 10 = digit  
    
let rec getNumbersFromUser () =
    let rec readNumber () =
        printf "Введите число (или 'q' для завершения): "
        let input = Console.ReadLine()

        if input.ToLower() = "q" then 
            None  
        else
            match Int32.TryParse(input) with
            | (true, number) -> Some number  
            | _ ->                           
                printfn "Ошибка: Введите целое число!"  
                readNumber ()

    Seq.initInfinite (fun _ -> readNumber ()) 
    |> Seq.takeWhile Option.isSome            
    |> Seq.choose id                          

let rec getDigitFromUser () =
    printf "Введите цифру (0-9): "
    let input = Console.ReadLine()
    
    match Int32.TryParse(input) with
    | (true, digit) when digit >= 0 && digit <= 9 -> digit  
    | _ -> 
        printfn "Ошибка: Введите одну цифру от 0 до 9!"  
        getDigitFromUser ()  
        
let numbers = getNumbersFromUser ()  
let digit = getDigitFromUser ()     

let sumMatchingNumbers = 
    numbers 
    |> Seq.fold (fun acc number -> 
        if endsWithDigit number digit then acc + number 
        else acc  
    ) 0  

printfn "Сумма чисел, оканчивающихся на %d: %d" digit sumMatchingNumbers
