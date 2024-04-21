open System.Diagnostics
open NumSharp
open FSharp.Data
open XPlot.Plotly
open System.Linq


let dotProduct v1 v2 = 
    Array.map2 (*) v1 v2 |> Array.sum

let magnitude v : float32 = 
    v |> Array.map (fun x -> x * x) |> Array.sum |> sqrt

let cosineSimilarity v1 v2 = 
    let numerator = dotProduct v1 v2
    let denominator = magnitude v1 * magnitude v2
    numerator / denominator

let strip chars = String.collect (fun c -> if Seq.exists((=)c) chars then "" else string c)

let tryParseFloat (s: string) =
    match System.Double.TryParse(s) with
    | (true, v) -> Some v
    | _ -> None

// Load the data. The data is in the form of embeddings from a pre-trained transformer model.
let myData = np.Load<float32[,]>(@"C:\Users\khans\Desktop\FunctionalProgramming\transformers\author_embeddings.npy");

// Compute the cosine similarities between all authors. The value at element [i, j] is the
// similarity between the ith and jth authors
let computeCosineSimilarityMatrix (matrix : float32[,]) : float32[,] =
    let n = Array2D.length1 matrix
    Array2D.init n n (fun i j ->
        let v1 = matrix[i,*]
        let v2 = matrix[j,*]
        cosineSimilarity v1 v2
    )
let result = computeCosineSimilarityMatrix myData

// Construct a graph that represents which authors are similars. Nodes are author indices
// Assign an edge between authors that are similar (>0.74 cosine similarity)
// The graph is a map that maps ints to list of ints (the ints are author indices)
let similarityThreshold = 0.74f
let constructGraph (cosineSimilarities : float32[,]) : Map<int, int list> =
    let n = Array2D.length1 cosineSimilarities
    let edges = 
        [ for i in 0 .. n - 1 do
            //for j in i + 1 .. n - 1 do
            for j in 0 .. n - 1 do
                if cosineSimilarities.[i, j] > similarityThreshold then yield i, j ]
    edges
    |> Seq.groupBy fst
    |> Map.ofSeq
    |> Map.map (fun _ edges -> List.map snd (List.ofSeq edges))
let graph = constructGraph result

// Load the original author data
// CsvProvider automatically creates a type for us. Has a field Row, which has fields for each of the columns
type Books = CsvProvider<"C:\\Users\\khans\\Desktop\\FunctionalProgramming\\transformers\\book-1000.csv">
let libData = Books.Load("C:\\Users\\khans\\Desktop\\FunctionalProgramming\\transformers\\book-1000.csv")

// Choose the first author in each group as the golden entity, then copy it as the author of all rows in the group
// We can do this because the graph we constructed had bi-directional edges. The adjacency list looks forwards
// and backwards

// Convert the data to a list so we can use list functions
let dataList = libData.Rows |> Seq.toList

// Create a map from row index to row
let indexToRowMap = dataList |> List.mapi (fun i row -> i, row) |> Map.ofList

// Function to update a row with a new author
let updateRow newAuthor (row: Books.Row) = Books.Row(row.Source, row.ISBN, row.Title, newAuthor) 

// Function to update the rows in the map according to the index map
let updateRows indexMap (indexToRowMap: Map<int, Books.Row>) =
    // accumulator, key, value (which is a list)
    indexMap |> Map.fold (fun acc key valueList ->
        let author = indexToRowMap.[key].Author     
        valueList |> List.fold (fun acc value ->
            let row = indexToRowMap.[value]         
            let newRow = updateRow author row
            Map.add value newRow acc) acc) indexToRowMap

// Update the rows
let updatedIndexToRowMap = updateRows graph indexToRowMap

// Convert the map back to a list
let updatedDataList = updatedIndexToRowMap |> Map.toSeq |> Seq.map snd |> Seq.toList

// Convert the list back to a CsvFile
let updatedData = new Books(updatedDataList)

updatedData.Save("books-cleaned.csv")


// Testing phase: We can use the ISBNs. Books with the same ISBN should have the same author.
// Count the number of unique authors per ISBN. We should get as many 1's as possible
let countsOriginal =
    dataList
    |> Seq.groupBy (fun row -> row.ISBN)
    |> Seq.map (fun (isbn, rows) -> rows |> Seq.map (fun row -> row.Author) |> Seq.distinct |> Seq.length)

let countsCleaned =
    updatedDataList
    |> Seq.groupBy (fun row -> row.ISBN)
    |> Seq.map (fun (isbn, rows) -> rows |> Seq.map (fun row -> row.Author) |> Seq.distinct |> Seq.length)

// Create histograms
let histogramOG = Histogram(x = countsOriginal, name = "Original Counts")
let histogramCleaned = Histogram(x = countsCleaned, name = "Cleaned Counts")

// Create a chart
let chart = Chart.Plot([|histogramOG; histogramCleaned|])

// Display the chart
chart.Show()

