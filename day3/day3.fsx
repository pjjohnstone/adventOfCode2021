open System.IO

let lines = File.ReadAllLines("day3/input.txt") |> Array.toList

let example = [
  "00100"
  "11110"
  "10110"
  "10111"
  "10101"
  "01111"
  "00111"
  "11100"
  "10000"
  "11001"
  "00010"
  "01010"
]

let countColumn (lines: string list) column =
  match lines with
  | [] -> ([],[])
  | _ ->
    lines
    |> List.map (fun l -> l[column])
    |> List.partition (fun d -> d = '1')

let gammaRate ((ones: char list),(zeroes: char list)) =
  if ones.Length >= zeroes.Length then '1' else '0'

let epsilonRate ((ones: char list),(zeroes: char list)) =
  if ones.Length < zeroes.Length then '1' else '0'

let stopPos = 11

let (|Stop|Continue|) pos =
  if pos > stopPos then Stop else Continue

let rec buildResultRec lines pos (gRate,eRate) =
  match lines with
  | [] -> (gRate, eRate)
  | _ ->
    match pos with
    | Stop -> (gRate, eRate)
    | Continue ->
      let gammaValue = countColumn lines pos |> gammaRate
      let epsilonValue = countColumn lines pos |> epsilonRate
      buildResultRec lines (pos+1) ((gRate@[gammaValue]),(eRate@[epsilonValue]))

let buildResult lines =
  buildResultRec lines 0 ([],[])

let convertToDecimal (chars: char list) =
  System.Convert.ToInt32(System.String.Concat(Array.ofList(chars)), 2)

let mostCommonBit (lines: string list) pos =
  countColumn lines pos
  |> gammaRate

let leastCommonBit (lines: string list) pos =
  countColumn lines pos
  |> epsilonRate

let rec getLifeSupportRatingRec sorter (lines: string list) pos =
  match lines with
  | [] -> []
  | _ ->
    match lines.Length with
    | 1 -> lines
    | _ ->
      lines
      |> List.filter (fun l -> l[pos] = sorter lines pos)
      |> fun newLines -> getLifeSupportRatingRec sorter newLines (pos + 1)

let getOxygenRating lines =
  let oxygenBits = getLifeSupportRatingRec mostCommonBit lines 0
  oxygenBits.Head
  |> Seq.toList
  |> convertToDecimal

let getCo2Rating lines =
  let co2Bits = getLifeSupportRatingRec leastCommonBit lines 0
  co2Bits.Head
  |> Seq.toList
  |> convertToDecimal

buildResult lines
|> fun (gRate,eRate) ->
  let gammaRate = convertToDecimal gRate
  let epsilonRate = convertToDecimal eRate
  printfn "Gamma rate is: %i" gammaRate
  printfn "Epsilon rate is: %i" epsilonRate
  printfn "Power consumption is: %i" (gammaRate * epsilonRate)

let oxygenRating = getOxygenRating lines
let co2Rating = getCo2Rating lines
printfn "Oxygen generator rating is: %i" oxygenRating
printfn "CO2 scrubber rating is: %i" co2Rating
printfn "Life support rating is: %i" (oxygenRating * co2Rating)