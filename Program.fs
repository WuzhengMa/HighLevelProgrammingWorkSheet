//-------------------Exercise 1.1------------------------
let square (x) = x * x
let pairs = [ (1,2) ; (1,3) ; (2,3); (3,4) ; (4,7) ]
//let makeTriple (a,b) = 
//    let p = square a - square b
//    let q = 2 * a * b
//    let r = square a + square b
//    square p + square q = square r    
let pythagTriples = 
    let makeTriple (a,b) = 
        let p = square a - square b
        let q = 2 * a * b
        let r = square a + square b
        square p + square q = square r
    List.map makeTriple pairs   //pythagTriples is boolean list, makeTriple is boolean, pairs is (int, int) list
printfn "Triples are: %A" pythagTriples

//-------------------Exercise 1.2------------------------
let intList = [1..5]
let makePairs a0 =
//    let tmp1 b = (a0,b) 
//It is OK for b to be changed to another name (except a0), but it is not OK for a0 to be changed to another name in tmp1 
//There is no difference between tmp1 as a value  and tmp1 as a function
    let tmp2 lst = List.map (fun b -> (a0,b)) lst   
    tmp2 intList
//-------------------------------------------------------

let retainPositive lst = List.collect (fun a -> if a > 0 then [a] else []) lst
printfn "retain Positive returns: %A" (retainPositive [ 1; -2; -4; 0; 11]) // should return [1; 11]

//-----------------------Tick Excercise--------------------------------

let sineSeries n x = 
    let power a i = a ** float i
    let times a b = a * b
    let sum a b = a + b
    let term i = (power -1.0 i) * (power x (2 * i + 1)) / List.reduce times [1.0..float (2*i+1)]
    [1..n] |> List.map term |> List.reduce sum
printfn "sineSeries returns: %A" (sineSeries ) 

System.Console.ReadLine() // prevent the program from terminating



