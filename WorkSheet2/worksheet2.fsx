let primeFactors n =
    let rec pf factLst (c:int64) (i:int64) =
        match i % c = 0L with
        | true -> pf (c :: factLst) c (i/c)
        | false when i = 1L -> factLst
        | _ -> pf factLst (c+1L) i
    pf [] 2L n

let findLargestPrimeFactor (list:int64 list) = 
    let largestPrime (list:int64 list) = list.Head
    list |> List.map(fun (x:int64) -> largestPrime(primeFactors(x:int64)))