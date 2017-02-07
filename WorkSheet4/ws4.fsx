type Command = 
    | Read of string 
    | Write of string * int 
    | Assign of string * string
    | Parse of string

type Response =
    | VarValue of int // for commands that return data
    | ParseError // if command string is invalid
    | DataError // if the required data does not exist
    | OK // for valid commads that return no data

type DataType = 
    {key:string; value:int}

/// character is white sppace
let isWhiteSpace (c : char) = List.contains c [ ' '; '\n'; '\t'; '\r'; '\f' ]

/// character is alphabetic
let isAlpha (c : char) = List.contains c ([ 'a'..'z' ] @ [ 'A'..'Z' ])

/// character is a decimal digit
let isDigit (c : char) = List.contains c [ '0'..'9' ]

/// character is alphanumeic (allowed in symbol)
let isAlphaNum (c : char) = isAlpha c || isDigit c

/// convert string into char list
let explode (str : string) = str |> List.ofSeq

/// convert char list into string
let implode (x : char list) = x |> System.String.Concat


let environment command = 
    let mutable state = Map.empty

    let readData varName = 
        let found = state.TryFind varName 
        match found with
        | Some value -> VarValue (value)
        | None -> DataError

    let writeData (varName, value) = 
        let addedMap = state.Add(varName, value)
        let found = addedMap.TryFind varName
        match found with
        | Some value -> OK
        | None -> DataError

    let assignData (varDest, varSrc) = 
        let SrcData = 
            let found = state.TryFind varSrc
            match found with
            | Some value -> VarValue (value)
            | None -> DataError

        let removedTree = state.Remove(varDest)

        match SrcData with
        | VarValue (value) -> 
            let addedMap = removedTree.Add(varDest, value)
            let found = addedMap.TryFind varDest
            match found with
            | Some value -> OK
            | None -> DataError
        | _ -> DataError

    let parseString (command:string) = 
        let commandArr = command.Split [|' '; '\n'; '\t'; '\r'; '\f'|] |> List.ofArray
        
	let parseCommandType command = 
	    match commandArr with
	    | "READ" :: tail -> parseVariableName ("READ", tail)
	    | "WRITE" :: tail -> parseVariableName ("WRITE", tail)
	    | "ASSIGN" :: tail -> parseVariableName ("ASSIGN", tail)
	    | _ -> ParseError

        let parseVariableName (commandType, varList) = 
            match varList with
            | varName :: rest when varName <> "" && commandType = "READ" -> readData (varName)
            | varName :: rest when varName <> "" && commandType = "WRITE" -> 
	      match rest with
	      | value :: tail when value <> "" -> writeData (varName, value)
	    | varName1 :: rest when varName1 <> "" && commandType = "ASSIGN" ->
	      match rest with
	      | when varName2 :: tail when varName2 <> "" -> assignData(varName1, varName2)
	    | _ -> ParseError

    let rec parseCommand command =
        match command with
        | Read (varName) -> readData varName
        | Write (varName, value) -> writeData (varName, value)
        | Assign (varDest, varSrc) -> assignData (varDest, varSrc)
        | Parse (commandStr) -> parseString (commandStr)
        | _ -> ParseError
    parseCommand command

printfn "anything: %A" (environment (Assign("x", "x1")))
System.Console.ReadLine() // prevent the program from terminating