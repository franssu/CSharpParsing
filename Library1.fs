namespace CSharpParsing

module Library1 =
    type Name = string
    type VarName = Name
    type TypeName = Name
    type MemberName = Name
    type LabelName = Name
    type Literal = 
        | Literal of obj
        | VerbatimLiteral of obj
    let createLiteral = fun x -> Literal(x)
    let createVerbatimLiteral = fun x -> VerbatimLiteral(x)
    type ArgType = ValueArg | RefArg | OutArg
    type Expr = 
        | Value of Literal
        | Variable of VarName
        | MethodInvoke of MemberName * Arg list
        | PropertyGet of MemberName
        | Cast of TypeName * Expr
        | InfixOp of Expr * string * Expr
        | PrefixOp of string * Expr
        | PostfixOp of Expr * string
        | TernaryOp of Expr * Expr * Expr
    and Arg = Arg of ArgType * Expr
    type Define = Define of TypeName * VarName
    let createDefine = fun x y -> Define(x, y)
    type Init = 
        | Assign of Name * (* =,+=, etc. *) Expr
        | Construct of TypeName * Name * Expr
    let createAssign = fun x y -> Assign(x, y)
    let createConstruct = fun x y z -> Construct(x, y, z)
    type Condition = Expr
    type Iterator = Expr
    type Statement =
        | Definition of Define
        | Assignment of Init
        | PropertySet of MemberName * Expr
        | Action of Expr
        //| Block/Scope of Statement list
        | If of Expr * Block
        | IfElse of Expr * Block * Block
        | Switch of Expr * Case list
        | For of Init list * Condition * Iterator list * Block
        | ForEach of Define * Expr * Block
        | While of Expr * Block
        | DoWhile of Block * Expr
        | Throw of Expr
        | Try of Block
        | Catch of TypeName * Block
        | Finally of Block
        | Lock of Expr * Block    
        | Using of Expr * Block
        | Label of LabelName
        | Goto of LabelName
        | Break
        | Continue
        | Return of Expr
    and Case = 
        | Case of Literal * Block
        | Default of Block
    and Block = Statement list
    let createSwitch = fun x y -> Switch(x, y)

    type Access = Public | Private | Protected | Internal
    type Modifier = Static | Sealed | Override | Virtual | Abstract | Const
    // Members
    type ReturnType = TypeName
    type MemberInfo = MemberInfo of Access * Modifier option * ReturnType * Name
    let createMemberInfo = fun w x y z -> MemberInfo(w, x, y, z)
    type IsReadOnly = bool
    type ParamType = ByValue | ByRef | Out | Params
    type Param = Param of ParamType * TypeName * VarName
    type PreConstruct = PreConstruct of Name * Param list
    type Member =
        | Field of Access * Modifier option * IsReadOnly * 
                   ReturnType * Name * Expr option
        | Property of MemberInfo * Block option * Block option
        | Method of MemberInfo * Param list * Block
        | Constructor of Access * Modifier option * Name * Param list * 
                         PreConstruct option * Block
    // Types
    type Members = Member list
    type Implements = Name list
    type EnumValue = EnumValue of Name * obj
    type CSharpType = 
        | Class of Access * Modifier option * Name * Implements * Members
        | Struct of Access * Name * Member list
        | Interface of Access * Name * Implements * Member list
        | Enum of Access * TypeName * EnumValue list
        | Delegate of Access * Name * ReturnType * Param list    
    // Namespace scopes
    type Import = 
        | Import of Name list
        | Alias of Name * Name list
    type NamespaceScope =
        | Namespace of Import list * Name list * NamespaceScope list
        | Types of Import list * CSharpType list

    open FParsec

    let ws = 
        let pspaces =
            let pcomment = (pstring "//" <|> pstring "#") >>. many1Satisfy ((<>) '\n')
            spaces >>. many (spaces >>. pcomment >>. spaces)
        let pmlcomment = 
            let maxCount = System.Int32.MaxValue
            pstring "/*" >>. skipCharsTillString "*/" true (maxCount)
        pspaces >>. many (pspaces >>. pmlcomment >>. pspaces) >>% ()

    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> spaces1

    let pliteral =
        let pnumber : Parser<Literal, unit> =
            let numberFormat = NumberLiteralOptions.AllowMinusSign
                               ||| NumberLiteralOptions.AllowFraction
                               ||| NumberLiteralOptions.AllowExponent
            numberLiteral numberFormat "number"
            |>> fun nl ->
                    if nl.IsInteger then nl.String |> (int >> createLiteral)
                    else nl.String |> (float >> createLiteral)
        let pbool =
            let ptrue = str_ws "true" >>% Literal(true)
            let pfalse = str_ws "false" >>% Literal(false)
            ptrue <|> pfalse
        let pstringliteral =
            let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
            let unescape = function
                           | 'n' -> '\n'
                           | 'r' -> '\r'
                           | 't' -> '\t'
                           | x -> x
            let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
            between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar)) |>> createLiteral
        let pstringverbatimliteral =
            let normalChar = satisfy (fun c -> c <> '"')
            pstring "@" >>. between (pstring "\"") (pstring "\"") (manyChars normalChar) |>> createVerbatimLiteral
        pnumber <|> pbool <|> pstringliteral <|> pstringverbatimliteral

    // Expressions
    let pexpr, pexprimpl = createParserForwardedToRef ()

    let pidentifier =
        let reserved = ["for"; "do"; "while"; "if"; "switch"; "case"; "default";"break" (*;...*)]
        let pidentifierraw =
            let isIdentifierFirstChar c = isLetter c || c = '_'
            let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.' // Todo : improve . management
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        pidentifierraw 
        >>= fun s -> 
            if reserved |> List.exists ((=) s) then fail "keyword" 
            else preturn s
    let pidentifier_ws = pidentifier .>> ws
    let opp = 
        let result = OperatorPrecedenceParser<Expr, unit, unit>()
        pexprimpl := result.ExpressionParser
        let term =
            let pvalue =
                let pcast =
                    let ptypecast = between (str_ws "(") (str_ws ")") pidentifier_ws
                    ptypecast .>>. pexpr |>> Cast
                let pvar = pidentifier |>> Variable
                let pinvoke =
                    let parg =
                        let pargtype = 
                            let pargref = str_ws1 "ref" >>% RefArg
                            let pargout = str_ws1 "out" >>% OutArg
                            opt pargref <|> opt pargout
                            |>> function Some x -> x | None -> ValueArg
                        pargtype .>>. pexpr |>> Arg
                    pidentifier_ws .>>. between (str_ws "(") (str_ws ")") (sepBy parg (str_ws ","))
                    |>> MethodInvoke
                (pliteral |>> Value) <|> attempt pinvoke <|> attempt pvar <|> attempt pcast
            pvalue .>> ws <|> between (str_ws "(") (str_ws ")") pexpr
        result.TermParser <- term
        result
    let inops = ["+"; "-"; "*"; "/"; "%"; "&&"; "||"; ">>"; "<<"; "&"; "|"; "^"; "=="; "!="; "<="; ">="; "<"; ">"; "??"; "."]
    for op in inops do
        opp.AddOperator(InfixOperator(op, ws, 1, Associativity.Left, fun x y -> InfixOp(x, op, y)))
    let preops = ["-"; "++"; "--"]
    for op in preops do
        opp.AddOperator(PrefixOperator(op, ws, 1, true, fun x -> PrefixOp(op, x)))
    opp.AddOperator(PrefixOperator("new", spaces1, 1, true, fun x -> PrefixOp("new", x)))
    let postops = ["++"; "--"]
    for op in postops do
        opp.AddOperator(PostfixOperator(op, ws, 1, true, fun x -> PostfixOp(x, op)))

    let pexpr' = between (str_ws "(") (str_ws ")") pexpr

    // Statement blocks
    let pstatement, pstatementimpl = createParserForwardedToRef()
    let psinglestatement = pstatement |>> fun statement -> [statement]
    let pstatementblock =
        psinglestatement <|>
        between (str_ws "{") (str_ws "}") (many pstatement) 

    // Assignement statements
    let pdefine = pipe2 (pidentifier .>> spaces1) (pidentifier) createDefine
    let pdefinition = pdefine |>> Definition

    let passign = pipe2 (pidentifier_ws .>> (str_ws "=")) pexpr createAssign
    let pconstruct = pipe3 (pidentifier .>> spaces1) (pidentifier_ws .>> (str_ws "=")) pexpr createConstruct
    let passignment = attempt passign <|> attempt pconstruct |>> Assignment

    // Selection statements
    let pif =
        pipe2 (str_ws "if" >>. pexpr') pstatementblock
            (fun e block -> If(e, block))

    let pifelse =
        pipe3 (str_ws "if" >>. pexpr') pstatementblock (str_ws "else" >>. pstatementblock)
            (fun e t f -> IfElse(e, t, f))

    let pcase = str_ws1 "case" >>. pliteral .>> str_ws ":" 
    let pcaseblock = pipe2 pcase (many pstatement) (fun case block -> Case(case, block))
    let pdefault = str_ws "default" >>. str_ws ":" 
    let pdefaultblock = pdefault >>. (many pstatement) |>> Default
    let pcases' = many pcaseblock .>>. opt pdefaultblock 
                  |>> fun (cases,d) -> cases@(Option.toList d)
    let pcases = between (str_ws "{") (str_ws "}") pcases'

    let pswitch = pipe2 (str_ws "switch" >>. pexpr') pcases createSwitch

    // Iteration statements
    let pfor =
        let pforargs =
            let pinit = attempt passign <|> attempt pconstruct
            pipe3 
                (sepBy pinit (str_ws ",") .>> str_ws ";")
                (pexpr .>> str_ws ";")
                (sepBy pexpr (str_ws ","))
                (fun from until steps -> from, until, steps)
        pipe2 
            (str_ws "for" >>. between (str_ws "(") (str_ws ")") pforargs)
            pstatementblock
            (fun (inits, until, iterators) block -> For(inits, until, iterators, block))

    let pforeachargs =
        pipe2 (pdefine .>> (str_ws1 "in")) pexpr (fun define collection -> define, collection)

    let pforeach =
        pipe2 (str_ws "foreach" >>. pforeachargs) pstatementblock
            (fun (define, collection) block -> ForEach(define,collection,block))

    let pwhile = 
        pipe2 (str_ws "while" >>. pexpr') pstatementblock
            (fun e block -> While(e, block))

    let pdowhile =
        pipe2
            (str_ws "do" >>. pstatementblock)
            (str_ws "while" >>. pexpr')
            (fun block e -> DoWhile(block, e))

    // Jump statements
    let preturn = str_ws1 "return" >>. pexpr |>> Return
    let pbreak = str_ws "break" >>% Break
    let pcontinue = str_ws "continue" >>% Continue
    let pgoto = str_ws1 "goto" >>. pidentifier_ws |>> Goto
    let plabel = pidentifier_ws .>> str_ws ":" |>> Label

    // Exception statements
    let ptry =     str_ws "try"     >>. pstatementblock |>> Try
    let pfinally = str_ws "finally" >>. pstatementblock |>> Finally
    let pcatch =
        let pexception = between (str_ws "(") (str_ws ")") pidentifier_ws
        str_ws "catch" >>. pexception .>>. pstatementblock |>> Catch

    // Lock statement
    let pthrow = str_ws1 "throw" >>. pexpr |>> Throw

    let plock = 
        str_ws "lock" >>. pexpr' .>>. pstatementblock |>> Lock

    // Statement implementation
    let paction = pexpr |>> Action

    pstatementimpl :=
        attempt (preturn .>> str_ws ";") <|>
        attempt (pbreak .>> str_ws ";") <|>
        attempt (pcontinue .>> str_ws ";") <|>
        attempt (pgoto .>> str_ws ";") <|>  
        attempt (pdefinition .>> str_ws ";") <|>
        attempt (passignment .>> str_ws ";") <|> 
        attempt (paction .>> str_ws ";") <|>
        attempt plabel <|>
        attempt pifelse <|> attempt pif <|> 
        attempt pswitch <|>
        attempt pfor <|> attempt pforeach <|>
        attempt pwhile <|> attempt pdowhile <|>
        attempt pthrow <|>
        attempt ptry <|> attempt pcatch <|> attempt pfinally <|>
        attempt plock

    // Modifiers
    let pmodifier = 
        let pstatic = str_ws1 "static" >>% Static
        let psealed = str_ws1 "sealed" >>% Sealed
        let poverride = str_ws1 "override" >>% Override
        let pvirtual = str_ws1 "virtual" >>% Virtual
        let pabstract = str_ws1 "abstract" >>% Abstract
        let pconst = str_ws1 "const" >>% Const
        pstatic <|> psealed <|> poverride <|> pvirtual <|> pabstract <|> pconst

    let createParam x y z = Param(x, y, z)

    // Parameters
    let pparamlist =
        let pparam =
            let pby =
                let pref = str_ws "ref" >>% ByRef
                let pout = str_ws1 "out" >>% Out
                let pparams = str_ws1 "params" >>% Params
                (opt pout <|> opt pref <|> opt pparams) 
                |>> function Some x -> x | None -> ByValue
            pipe3 pby pidentifier_ws pidentifier_ws createParam
        str_ws "(" >>. sepBy pparam (str_ws ",") .>> str_ws ")"
    
    let pns = sepBy1 pidentifier_ws (str_ws ".")
    let pimport =
        let popen = str_ws1 "using" >>. pns |>> Import
        let palias = str_ws1 "using" >>. pidentifier_ws >>. str_ws "=" .>>. pns |>> Alias
        (attempt popen <|> attempt palias) .>> str_ws ";"
    let pnsscope, pscopeimpl = createParserForwardedToRef()
    // Scopes
    let pnsblock =
        let pnsscopesblock =
            between (str_ws "{") (str_ws "}") (many pnsscope)
        pipe3 (many pimport) (str_ws1 "namespace" >>. pns) pnsscopesblock
         (fun imports name block -> 
            let types = Types([],[])
            Namespace(imports, name, block))
    let ptypes =
        let ptypedeclaration =
            let paccess =
                let ppublic = str_ws1 "public" >>% Public
                let pprivate = str_ws1 "private" >>% Private
                let pprotected = str_ws1 "protected" >>% Protected
                let pinternal = str_ws1 "internal" >>% Internal
            
                opt (ppublic <|> pprivate <|> pprotected <|> pinternal)
                |>> (fun access -> defaultArg access Internal)
            let pimplements =
                opt (str_ws ":" >>. sepBy1 (pidentifier_ws) (str_ws ","))
                |>> function Some xs -> xs | None -> []

            let pmembersblock =
                let pmember =
                    let pfield =
                        let pfieldpreamble =
                            let preadonly = str_ws1 "readonly"
                            pipe3 paccess (opt pmodifier) (opt preadonly)
                             (fun access modifier ro -> (access, modifier, Option.isSome ro))
                        pipe5 pfieldpreamble pidentifier_ws pidentifier_ws (opt (str_ws "=" >>. pexpr)) (str_ws ";")
                         (fun (access, modifier, ro) rt name expr _ -> Field(access, modifier, ro, rt, name, expr))
                    let pmemberinfo = 
                        pipe4 paccess (opt pmodifier) pidentifier_ws pidentifier_ws createMemberInfo
                    let pmethod = pipe3 pmemberinfo pparamlist pstatementblock (fun mi ps block -> Method(mi, ps, block))
                    let pproperty =
                        let ppropertyblock =
                            let pget = str_ws "get" >>. pstatementblock
                            let pset = str_ws "set" >>. pstatementblock
                            between (str_ws "{") (str_ws "}") ((opt pget) .>>. (opt pset))
                        pipe2 pmemberinfo ppropertyblock (fun mi (gblock, sblock) -> Property(mi, gblock, sblock))
                    let pconstructor = 
                        pipe5 paccess (opt pmodifier) pidentifier_ws pparamlist pstatementblock
                         (fun access modifier name ps block -> 
                            Constructor(access, modifier, name, ps, None, block))
                    attempt pfield <|> attempt pmethod <|> attempt pproperty <|> attempt pconstructor
                between (str_ws "{") (str_ws "}") (many pmember) |>> (fun members -> members)
            let pclass =
                let pclasspreamble = paccess .>>. (opt pmodifier) .>> (str_ws1 "class")
                pipe4 pclasspreamble pidentifier_ws pimplements pmembersblock (fun (access, modifier) name implements block -> Class(access, modifier, name, implements, block))
            let pstruct =
                pipe4 paccess (str_ws1 "struct") pidentifier_ws pmembersblock (fun access _ name block -> Struct(access, name, block))
            let pinterface =
                pipe5 paccess (str_ws1 "interface") pidentifier_ws pimplements pmembersblock
                 (fun access _ name implements block -> Interface(access, name, implements, block))
            let penum =
                let penumblock = 
                    between (str_ws "{") (str_ws "}") (sepBy pidentifier_ws (str_ws ","))
                    |>> fun names -> names |> List.mapi (fun i name -> EnumValue(name, i))
                pipe4 paccess (str_ws1 "enum") pidentifier_ws penumblock (fun access _ name block -> Enum(access, name, block))
            let pdelegate =
                pipe5 paccess (str_ws1 "delegate") pidentifier_ws pidentifier_ws pparamlist
                 (fun access _ ty name ps -> 
                    Delegate(access, ty, name, ps))
            pclass <|> pstruct <|> pinterface <|> penum <|> pdelegate
        pipe2 (many pimport) (many1 ptypedeclaration)
         (fun imports classes -> Types(imports, classes))
    pscopeimpl := ws >>. (pnsblock <|> ptypes)
