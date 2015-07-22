namespace CSharpParsing

module Types =
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
