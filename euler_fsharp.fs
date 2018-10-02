// Solutions in F#

// https://projecteuler.net/problem=1
let threeOrFive =
    let myList = [1..999]
    let isMultiple x = x % 3 = 0 || x % 5 = 0
    List.sum(List.filter (isMultiple) (myList))

// https://projecteuler.net/problem=2
let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0, 1)
let evenFib = 
    let lessThan x = x < 4000000
    let isEven x = x % 2 = 0
    let fibsLessThan = Seq.filter isEven fibonacciSeq
    Seq.sum (Seq.takeWhile lessThan fibsLessThan)

// https://projecteuler.net/problem=3
let rec isPrimeHelp (x:bigint) (y:bigint) = if y = 1I then true else if x % y = 0I then false else isPrimeHelp x (y-1I)
let isPrime (n:bigint) = if n <= 1I then false else isPrimeHelp n (n/2I)
let largestPrimeFactor (n:bigint) = 
    let myList = [0I..(n/2I)]
    let filter (x:bigint) = isPrime x && n % x = 0I
    Seq.filter filter myList

// https://projecteuler.net/problem=4
let largestPalindrome = 
    let threeDigits = [100..999]
    let toString (x:int) = x.ToString()
    let isPali (x:int) = (toString x).ToCharArray() = Array.rev ((toString x).ToCharArray()) 
    List.max (List.filter (isPali) (List.collect (fun x -> [for i in threeDigits -> i * x]) threeDigits))