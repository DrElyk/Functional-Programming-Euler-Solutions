// Solutions in F#

// https://projecteuler.net/problem=1
let problem1 =
    let myList = [1..999]
    let isMultiple x = x % 3 = 0 || x % 5 = 0
    List.sum(List.filter (isMultiple) (myList))

// https://projecteuler.net/problem=2
let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0, 1)
let problem2 = 
    let lessThan x = x < 4000000
    let isEven x = x % 2 = 0
    let fibsLessThan = Seq.filter isEven fibonacciSeq
    Seq.sum (Seq.takeWhile lessThan fibsLessThan)

// https://projecteuler.net/problem=3
let rec isPrimeHelp (x:bigint) (y:bigint) = if y = 1I then true else if x % y = 0I then false else isPrimeHelp x (y-1I)
let isPrime (n:bigint) = if n <= 1I then false else isPrimeHelp n (n/2I)
let problem3 (n:bigint) = 
    let myList = [0I..(n/2I)]
    let filter (x:bigint) = isPrime x && n % x = 0I
    Seq.filter filter myList

// https://projecteuler.net/problem=4
let problem4 = 
    let threeDigits = [100..999]
    let toString (x:int) = x.ToString()
    let isPali (x:int) = (toString x).ToCharArray() = Array.rev ((toString x).ToCharArray()) 
    List.max (List.filter (isPali) (List.collect (fun x -> [for i in threeDigits -> i * x]) threeDigits))

// https://projecteuler.net/problem=5
let problem5 =
    let numbers = Seq.unfold (fun i -> Some (i, i + 1)) 1
    let divBy20 x = (List.exists (fun n -> n = false) [for i in [1..10] -> x % i = 0]) = true
    (Seq.max (Seq.takeWhile (divBy20) (numbers))) + 1

// https://projecteuler.net/problem=6
let problem6 n = 
    let sumSquares = List.sum [for i in [1..n] -> i*i]
    let squareSum = (((List.sum [1..n])|>float)**(2|>float))|>int
    squareSum - sumSquares

// https://projecteuler.net/problem=7
let primes = 
    Seq.initInfinite (fun x -> bigint(x + 2))
    |> Seq.filter isPrime
    |> Seq.map (fun i -> int(i))
let problem7 (n:int) = 
    Seq.item (n-1) primes