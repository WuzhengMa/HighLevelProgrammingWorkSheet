    type Command = 
        | Read of string 
        | Write of (string * int) 
        | Assign of (string * string)
        | Parse of (string)

    type Response =
        | VarValue of int // for commands that return data
        | ParseError // if command string is invalid
        | DataError // if the required data does not exist
        | OK // for valid commads that return no data
    
    let makeEnvironment () = 
        let mutable state = Map.empty

        let environment command = 
            /// character is a decimal digit
            let rec isNumber (c : char list) = 
                let rec isDigit (c : char list) = 
                    match c with
                    | ch :: rest when List.contains ch [ '0'..'9' ] -> isDigit(rest)
                    | [] -> true
                    | _ -> false

                match c with
                | ch :: rest when List.contains ch ['+';'-'] -> isDigit (rest)
                | c -> isDigit (c)
            
            let readData varName = 
                let found = state.TryFind varName 
                match found with
                | Some value -> VarValue (value)
                | None -> DataError

            let writeData (varName, value) = 
                state <- state.Add(varName, value)
                let found = state.TryFind varName
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
                    state <- removedTree.Add(varDest, value)
                    let found = state.TryFind varDest
                    match found with
                    | Some value -> OK
                    | None -> DataError
                | _ -> DataError

            let parseString (command:string) = 
                let commandArr = command.Split [|' '; '\n'; '\t'; '\r'; '\f'|] |> List.ofArray
        
                let parseVariableName (commandType, varList) = 
                    match varList with
                    | varName :: rest when varName <> "" && commandType = "READ" -> readData (varName)
                    | varName :: rest when varName <> "" && commandType = "WRITE" -> 
                        match rest with
                        | value :: tail when value <> "" && isNumber (value|>List.ofSeq) -> writeData (varName, (value|>int))
                        | _ -> ParseError
                    | varName1 :: rest when varName1 <> "" && commandType = "ASSIGN" ->
                        match rest with
                        | varName2 :: tail when varName2 <> "" -> assignData(varName1, varName2)
                        | _ -> ParseError
                    | _ -> ParseError

                let parseCommandType command = 
                    match commandArr with
                    | "READ" :: tail -> parseVariableName ("READ", tail)
                    | "WRITE" :: tail -> parseVariableName ("WRITE", tail)
                    | "ASSIGN" :: tail -> parseVariableName ("ASSIGN", tail)
                    | _ -> ParseError      
                parseCommandType command

            let rec parseCommand command =
                match command with
                | Read (varName) -> readData varName
                | Write (varName, value) -> writeData (varName, value)
                | Assign (varDest, varSrc) -> assignData (varDest, varSrc)
                | Parse (commandStr) -> parseString (commandStr)
                | _ -> ParseError
            parseCommand command
        environment
