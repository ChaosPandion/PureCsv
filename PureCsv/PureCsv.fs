module PureCsv

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

    let bind parser getNextParser state = 
        match parser state with
        | Success (state, value) -> 
            getNextParser value state
        | Failure (_, message) -> 
            Failure (state, message)

    let _return value state = 
        Success (state, value)  

    type ParseBuilder () = 
        member x.Bind (parser, getNextParser) state = 
            bind parser getNextParser state
        member x.Return value state = 
            _return value state
        member x.Delay func state = 
            func () state

    let parse = ParseBuilder ()               

    let matchStringAtIndex (text:string) (index:int) (test:string) = text.IndexOf(test, index) = index
                                                       
    let getChar state = state.text.[state.index]
             
    let parseChar c = 
        fun state ->
            if state.index < state.text.Length && getChar state = c 
            then Success ({ state with index = state.index + 1 }, c) 
            else Failure (state, "")  

    let parseString cs =
        fun state ->
            if state.index < state.text.Length && matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, cs) 
            else Failure (state, "")   

    let skipChar c = 
        fun state ->
            if state.index < state.text.Length && getChar state = c 
            then Success ({ state with index = state.index + 1 }, ()) 
            else Failure (state, "") 

    let skipString cs =
        fun state ->
            if state.index < state.text.Length && matchStringAtIndex state.text state.index cs
            then Success ({ state with index = state.index + cs.Length }, ()) 
            else Failure (state, "")
         
    let parseOneOf cs = 
        let set = Set.ofSeq cs
        fun state -> 
            if state.index >= state.text.Length 
            then Failure (state, "")
            else 
                let c = getChar state
                if set.Contains c
                then Success ({ state with index = state.index + 1 }, c) 
                else Failure (state, "")

    /// This is a 'choice' combinator.
    let (<|>) p1 p2 state =
        match p1 state with
        | Success (state, value) as result -> result
        | Failure (state, message) -> p2 state

    let optional p state = 
        match p state with
        | Success (state, value) -> Success (state, Some value) 
        | Failure (_, message) -> Success (state, None)  
          
    let parseCRLF = parse {
        let! s = parseString "\r\n"
        return s
    }  

    /// TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
    let TEXTDATA = 
        [0x2D..0x7E] 
        |> List.map char 
        |> List.append ['\u0020'; '\u0020'] 
        |> List.append 
        <| ([0x23..0x2B] |> List.map char)

    let parseTEXTDATA = parseOneOf TEXTDATA 

    let parse2DQUOTE = parse {
        do! skipChar '\"'  
        do! skipChar '\"'
        return '\"'
    }

    let parseEscapedBodyChar = parse {
        let! c = parseOneOf ",\r\n" <|> parse2DQUOTE <|> parseTEXTDATA
        return c               
    }

    let rec parseEscapedBody () = parse {
        let! head = parseEscapedBodyChar
        let! tail = parseEscapedBody () <|> _return []
        return head::tail              
    }

    let parseEscaped = parse {
        do! skipChar '\"'
        let! body = parseEscapedBody () <|> _return []
        do! skipChar '\"'
        return body
    }  

    let rec parseNonEscaped () = parse {
        let! head = parseTEXTDATA
        let! tail = parseNonEscaped () <|> _return []
        return head::tail              
    }

    let parseField = parse {
        let! r = parseEscaped <|> parseNonEscaped () <|> _return []
        return r
    }         

    let parseName = parse {
        let! r = parseField
        return r
    }
                                      
    let rec parseRecordTrail () = parse {
        do! skipChar ','
        let! head = parseField
        let! tail = parseRecordTrail () <|> _return []
        return head::tail
    }
 
    let parseRecord = parse {
        let! head = parseField
        let! tail = parseRecordTrail () <|> _return [] 
        return head::tail
    }   

    let rec parseRecordsTrail () = parse { 
        let! _ = parseCRLF 
        let! head = parseRecord
        let! tail = parseRecordsTrail () <|> _return [] 
        return head::tail
    }
 
    let parseRecords = parse {
        let! head = parseRecord
        let! tail = parseRecordsTrail () <|> _return [] 
        return head::tail
    }

    let rec parseHeaderTrail () = parse {
        do! skipChar ','
        let! head = parseName
        let! tail = parseHeaderTrail () <|> _return []
        return head::tail
    }
 
    let parseHeader = parse {
        let! head = parseName
        let! tail = parseHeaderTrail () <|> _return []
        return head::tail
    }

    let parseFile = parse {
        let! header = optional parseHeader
        let! _ = optional parseCRLF 
        let! records = parseRecords
        let listToString cs = String(Array.ofList cs)
        let listToStringArray cs = cs |> List.map listToString |> Array.ofList
        let header = match header with | Some hs -> listToStringArray hs | None -> [| |] 
        let records = records |> List.map listToStringArray |> Array.ofList
        return CsvFile (header, records) 
    }
       
open Parser   

let parse text =
    if text = null then
        nullArg "text"
    let state = { text = text; index = 0 }
    match parseFile state with
    | Success (state, value) -> value
    | Failure (state, message) -> failwith message
     