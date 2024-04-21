open System
open System.IO

let punctuations = ['.'; ','; '!'; '?'; ';'; ':']

let rnd = new Random()

//let insertPunctuations (sentence: string) (numPunctuations: int) =
//    let words = sentence.Split(' ')
//    let mutable punctuatedWords = words |> Array.toList
//    for _ in 1 .. numPunctuations do
//        let index = rnd.Next(1, punctuatedWords.Length)
//        let punctuation = punctuations.[rnd.Next(punctuations.Length)]
//        punctuatedWords <- punctuatedWords.Insert(index, string(punctuation)) // The type List<_> does not define 'Insert'
//    String.Join(" ", punctuatedWords)

let rec insertPunctuationsRec (words: string list) (numPunctuations: int) =
    match words, numPunctuations with
    | _, 0 -> words
    | [], _ -> []
    | _ ->
        let index = rnd.Next(1, List.length words)
        let punctuation = punctuations.[rnd.Next(punctuations.Length)]
        let (before, after) = List.splitAt index words
        before @ [string(punctuation)] @ insertPunctuationsRec after (numPunctuations - 1)

let insertPunctuations (sentence: string) (numPunctuations: int) =
    let words = sentence.Split(' ') |> List.ofArray
    let punctuatedWords = insertPunctuationsRec words numPunctuations
    String.Join(" ", punctuatedWords)


let augmentData (filePath: string) (numNewExamples: int) =
    seq { use sr = new StreamReader(filePath)
          while not sr.EndOfStream do
              let line = sr.ReadLine()
              let parts = line.Split('\t')
              yield line
              if parts.Length = 2 then
                  let label = parts.[0]
                  let sentence = parts.[1]
                  let numPunctuations = Math.Min(sentence.Length / 3, 1)
                  for _ in 1 .. numNewExamples do
                      yield label + "\t" + insertPunctuations sentence numPunctuations }

[<EntryPoint>]
let main argv =
    printfn "Enter the file name:"
    let fileName = Console.ReadLine()
    printfn "Enter the number of new examples to generate for each existing example:"
    let numNewExamples = Console.ReadLine() |> Int32.Parse
    let augmentedData = augmentData fileName numNewExamples
    printfn "Enter the output file name:"
    let outputFileName = Console.ReadLine()
    use sw = new StreamWriter(outputFileName)
    augmentedData |> Seq.iter (fun line -> sw.WriteLine(line))
    printfn "The augmented data has been written to %s" outputFileName
    0 // return an integer exit code

    //seed, unaugmented accuracy,   augmented accuracy 
    //0,    0.7827050997782705, 0.8004434589800443
    //1,    0.7671840354767184, 0.7982261640798226
    //2,    0.7760532150776053, 0.7937915742793792
    //3,    0.7649667405764967, 0.7893569844789357

    //average: 0.7727272727272727, 0.7954545454545454