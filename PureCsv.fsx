open System

type CsvFile (headers:string[], records:string[][]) =

        let tupleSwap2 x y = y, x

        let headerMap = Array.mapi tupleSwap2 headers |> Map.ofArray

        member x.GetHeader column = headers.[column]
        
        member x.GetRecord (row, header) =  Array.get records row |> Array.get <| Map.find header headerMap

        member x.GetRecord (row, column) = Array.get records row |> Array.get <| column


module private Parser =
                               
    
    type State = {
        text : string
        index : int
    }

    type Result<'t> = 
        Success of State * 't 
        | Failure of State * string

    type ParseBuilder () = 
        member x.Bind (parser, getNextParser) =
            fun state ->
                match parser state with
                | Success (state, value) -> getNextParser value state
                | Failure (state, message) -> Failure (state, message)
        member x.Return (value) state = Success (state, value)  

    let parse = ParseBuilder ()               

    let matchStringAtIndex (text:string) (index:int) (test:string) = text.IndexOf(test, index) = index
                                                       
    let getChar state = state.text.[state.index]
  


    let parseChar c = 
        fun state ->
            if getChar state = c 
            then Success ({ state with index = state.index + 1 }, c) 
            else Failure (state, "")  

    let parseString cs =
        fun state ->
            if matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, cs) 
            else Failure (state, "")   

    let skipChar c = 
        fun state ->
            if getChar state = c 
            then Success ({ state with index = state.index + 1 }, ()) 
            else Failure (state, "") 

    let skipString cs =
        fun state ->
            if matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, ()) 
            else Failure (state, "")
         
    let parseOneOf cs = 
        let set = Set.ofSeq cs
        fun state -> 
            let c = getChar state
            if set.Contains c
            then Success ({ state with index = state.index + 1 }, c) 
            else Failure (state, "")

    let (<|>) p1 p2 state =
        match p1 state with
        | Success (state, value) as result -> result
        | Failure (state, message) -> p2 state
        


    let parseCRLF = parse {
        let! s = parseString "\r\n"
        return s
    }

    let parseEscapedBody = parse {
        return ""                
    }

    let parseEscaped = parse {
        do! skipChar '\"'
        let! body = parseEscapedBody
        do! skipChar '\"'
        return body
    }

                                      
    let parseRecord state = parse {
        return Array.zeroCreate 1
    }
 
    let parseHeader state = parse {
        return Array.zeroCreate 1
    }

    let parseFile state = parse {
        return CsvFile ([||], [||]) 
    }
          

let parse (text:string) : CsvFile = CsvFile ([||], [||])