//SineSeries
let sineSeries n x = 
    let power a i = a ** float i
    let times a b = a * b
    let sum a b = a + b
    let term i = (power -1.0 i) * (power x (2 * i + 1)) / List.reduce times [1.0..float (2*i+1)]
    [1..n] |> List.map term |> List.reduce sum