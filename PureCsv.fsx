  

type CsvFile (headers:string[], records:string[][]) =

        let tupleSwap2 x y = y, x

        let headerMap = Array.mapi tupleSwap2 headers |> Map.ofArray

        member x.GetHeader column = headers.[column]
        
        member x.GetRecord (row, header) =  Array.get records row |> Array.get <| Map.find header headerMap

        member x.GetRecord (row, column) = Array.get records row |> Array.get <| column


let parse (text:string) : CsvFile = CsvFile ([||], [||])