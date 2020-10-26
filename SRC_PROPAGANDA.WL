(* ################################################################################################################################################################ *)
(* PROPAGANDA, SINGLE PARTICLE NONLINEAR DYNAMICS TOOLKIT, 2017-2020, I.A.MOROZOV@INP.NSK.SU *)
(* ################################################################################################################################################################ *)
(* INITIALIZATION *)
(* ################################################################################################################################################################ *)
ClearSystemCache[] ;                (* -- CLEAR SYSTEM CACHE *)
Set[$HistoryLength,0] ;             (* -- NO HISTORY *)
Get["CompiledFunctionTools`"] ;     (* -- LOAD COMPILED FUNCTION PACKAGE *)
Get["CCompilerDriver`"] ;           (* -- LOAD COMPILER DRIVER PACKAGE *)
Get["MaTeX`"] ;                     (* -- LOAD MATEX PACKAGE *)
(* ################################################################################################################################################################ *)
(* SYSTEM COMPILE OPTIONS  *)
(* ################################################################################################################################################################ *)
Compiler`$CCompilerOptions = List[Rule["SystemCompileOptions","-O3  -ffast-math -march=native"]] ;
(* ################################################################################################################################################################ *)
(* PARALLEL MAP *)
(* ################################################################################################################################################################ *)
ClearAll[I$PARALLEL$MAP] ;
I$PARALLEL$MAP::usage = "
I$PARALLEL$MAP[FUNCTION,LIST] -- (function) apply function <FUNCTION> (function) to list of arguments <LIST> (list) using ParallelMap
" ;
Options[I$PARALLEL$MAP] = List[
  Rule["METHOD","FinestGrained"]  (* -- METHOD *)
] ;
I$PARALLEL$MAP[                   (* -- CUSTOM PARALLEL MAP FUNCTION *)
  FUNCTION_,                      (* -- FUNCTION TO APPLY (FUNCTION) *)
  LIST_List,                      (* -- ARGUMENT (LIST) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  ParallelMap[FUNCTION,LIST,Rule[Method,OptionValue["METHOD"]]]
] ;
(* ################################################################################################################################################################ *)
(* INSERT ELEMENTS *)
(* ################################################################################################################################################################ *)
ClearAll[I$INSERT] ;
I$INSERT::usage = "
I$INSERT[LIST,DATA,MASK] -- (function) insert list of elements <DATA> (list) into list <LIST> (list), inserted elements appear according to list of positions <MASK> (list of integers)
" ;
I$INSERT[                         (* -- INSERT LIST OF ELEMENTS INTO A LIST SO THAT NEW ELEMENTS APPEAR AT GIVEN POSITIONS IN THE RESULT *)
  LIST_List,                      (* -- INITIAL LIST OF ELEMENTS (LIST) *)
  DATA_List,                      (* -- LIST OF ELEMENTS TO INSERT (LIST) *)
  MASK_List                       (* -- LIST OF POSITIONS FOR INSERTED ELEMENTS (LIST OF INTEGERS) *)
] := Catch[
  Block[
    {LENGTH,RESULT,RULE},
    LENGTH = Plus[Length[LIST],Length[DATA]] ;
    RESULT = ConstantArray[0,LENGTH] ;
    RULE = Thread[Rule[MASK,DATA]] ;
    RESULT = ReplacePart[RESULT,RULE] ;
    RULE = Thread[Rule[DeleteCases[Range[LENGTH],Apply[Alternatives,MASK]],LIST]] ;
    ReplacePart[RESULT,RULE]
  ]
] /; And[
  Apply[And,Map[IntegerQ,MASK]],
  LessEqual[Max[MASK],Plus[Length[LIST],Length[DATA]]]
] ;
(* ################################################################################################################################################################ *)
(* INTEGRATOR (MAIN INITIALIZATION) *)
(* ################################################################################################################################################################ *)
ClearAll[I$INTEGRATOR] ;
I$INTEGRATOR::usage =
"
I$INTEGRATOR[DIRECTORY,DIMENSION,CORDER,KNOB,KORDER,MASK] -- (function) initialize integrator using working directory name <DIRECTORY> (string), configuration space dimension <DIMENSION> (integer), total canonical monomial degree <CORDER> (integer), total number of parameters <KNOB> (integer), total combined parameters monomial order <KORDER> (integer) and list of individual maximum orders for each parameter <MASK> (list of integers or empty list)
I$INTEGRATOR[DIRECTORY,DIMENSION] -- (function) initialize integrator using working directory <DIRECTORY> (string) and configuration space dimension <DIMENSION> (integer)
I$INTEGRATOR[DIRECTORY,DIMENSION,CORDER] -- (function) initialize integrator using working directory <DIRECTORY> (string), configuration space dimension <DIMENSION> (integer) and total canonical monomial order <CORDER> (integer)
" ;
Options[I$INTEGRATOR] = List[
 Rule["FLAG",W],                  (* -- APERTURE FLAG (SYMBOL) *)
 Rule["PARAMETER",S],             (* -- FORMAL INTEGRATION PARAMETER (SYMBOL) *)
 Rule["ORDERING","PAIRS"],        (* -- CANONICAL COORDINATES ORDERING ("PAIRS" (DEFAULT) OR "SECTORS") *)
 Rule["QHEAD","Q"],               (* -- HEAD FOR CANONICAL Q-VARIABLES (STRING) *)
 Rule["PHEAD","P"],               (* -- HEAD FOR CANONICAL P-VARIABLES (STRING) *)
 Rule["KHEAD","K"],               (* -- HEAD FOR PARAMETER VARIABLES (STRING) *)
 Rule["DHEAD","D"],               (* -- HEAD FOR DERIVATIVE VARIABLES (STRING) *)
 Rule["FHEAD","F"],               (* -- HEAD FOR PARAMETERS FLAGS (STRING) *)
 Rule["FILTER",Identity],         (* -- COMPONENTS FILTER FUNCTION (FUNCTION) (ACTS ON LIST OF ALL COMPONENTS) *)
 Rule["TRUNCATION","SUM"],        (* -- TRUNCATION TYPE ("SUM" (DEFAULT) OR "MAX"), "SUM" COMBINES CANONICAL AND PARAMETRIC ORDERS, "MAX" USES THEIR MAXIMUM *)
 Rule["VERBOSE",False]            (* -- VERBOSE FLAG (LOGICAL), PRINT INFO *)
] ;
I$INTEGRATOR[                     (* -- INITIALIZE INTEGRATOR *)
  DIRECTORY_String,               (* -- DIRECTORY NAME (STRING) *)
  DIMENSION_Integer,              (* -- CONFIGURATION SPACE DIMENSION (INTEGER) *)
  CORDER_Integer,                 (* -- MAXIMUM TOTAL MONOMIAL DEGREE FOR CANONICAL VARIABLES (INTEGER) *)
  KNOB_Integer,                   (* -- TOTAL NUMBER OF (DEVIATION) PARAMETERS (INTEGER) *)
  KORDER_Integer,                 (* -- MAXIMUM TOTAL MONOMIAL DEGREE FOR PARAMETERS (INTEGER) *)
  MASK_List,                      (* -- LIST OF MAXIMUM INDIVIDUAL ORDERS FOR EACH PARAMETER (LIST OF INTEGERS OR EMPTY LIST) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {VERBOSE},
    (* SET VERBOSE FLAG *)
    VERBOSE = OptionValue["VERBOSE"] ;
    (* SET AND/OR CREATE WORKING DIRECTORY *)
    SetDirectory[NotebookDirectory[]] ;
    I$DIRECTORY = FileNameJoin[List[NotebookDirectory[],DIRECTORY]] ;
    If[Not[DirectoryQ[I$DIRECTORY]],CreateDirectory[I$DIRECTORY]] ;
    SetDirectory[I$DIRECTORY] ;
    (* SET INTEGRATOR SIGNATURE (LIST OF INPUT ARGUMENTS) *)
    I$SIGNATURE = List[DIRECTORY,DIMENSION,CORDER,KNOB,KORDER,MASK] ;
    If[VERBOSE,Print["INTEGRATOR INITIALIZATION"]] ;
    If[VERBOSE,Print[StringTemplate["SIGNATURE               : ``"][I$SIGNATURE]]] ;
    (* SET APERTURE FLAG (SYMBOL) *)
    I$FLAG = OptionValue["FLAG"] ;
    If[VERBOSE,Print[StringTemplate["APERTURE FLAG           : ``"][I$FLAG]]] ;
    (* SET FORMAL INTEGRATION PARAMETER (SYMBOL) *)
    I$PARAMETER = OptionValue["PARAMETER"] ;
    If[VERBOSE,Print[StringTemplate["INTEGRATION PARAMETER   : ``"][I$PARAMETER]]] ;
    (* SET CANONICAL COORDINATES ORDERING (STRING) *)
    I$ORDERING = OptionValue["ORDERING"] ;
    If[VERBOSE,Print[StringTemplate["PHASE SPACE ORDERING    : ``"][I$ORDERING]]] ;
    (* SET VARIABLE HEADS (LIST OF STRINGS) *)
    List[I$Q$HEAD,I$P$HEAD,I$K$HEAD,I$D$HEAD,I$F$HEAD] = List[OptionValue["QHEAD"],OptionValue["PHEAD"],OptionValue["KHEAD"],OptionValue["DHEAD"],OptionValue["FHEAD"]] ;
    If[VERBOSE,Print[StringTemplate["VARIABLE HEADS          : ``"][List[I$Q$HEAD,I$P$HEAD,I$K$HEAD,I$D$HEAD,I$F$HEAD]]]] ;
    (* SET CONFIGURATION SPACE DIMENSION (INTEGER) *)
    I$DIMENSION = DIMENSION ;
    (* SET CANONICAL SPACE DIMENSION (INTEGER) *)
    I$DIMENSION$CANONICAL = Times[Plus[1,1],I$DIMENSION] ;
    If[VERBOSE,Print[StringTemplate["DIMENSION               : ``"][I$DIMENSION]]] ;
    If[VERBOSE,Print[StringTemplate["CANONICAL DIMENSION     : ``"][I$DIMENSION$CANONICAL]]] ;
    (* SET CANONICAL VARIABLES (LIST OF SYMBOLS ACCORDING TO SELECTED ORDERING) *)
    I$VARIABLES$CANONICAL = Composition[
      ToExpression,
      Function[Which[SameQ[I$ORDERING,"PAIRS"],Flatten[Slot[1]],SameQ[I$ORDERING,"SECTORS"],Flatten[Transpose[Slot[1]]]]],
      Function[Array[Function[Through[List[StringTemplate[StringJoin[I$Q$HEAD,"``"]],StringTemplate[StringJoin[I$P$HEAD,"``"]]][Slot[1]]]],Slot[1]]]
    ][I$DIMENSION] ;
    If[VERBOSE,Print[StringTemplate["CANONICAL VARIABLES     : ``"][I$VARIABLES$CANONICAL]]] ;
    (* SET CANONICAL Q & P-VARIABLES (LIST OF SYMBOLS) *)
    I$VARIABLES$CANONICAL$Q = Select[I$VARIABLES$CANONICAL,Function[SameQ[StringTake[ToString[Slot[1]],1],I$Q$HEAD]]] ;
    I$VARIABLES$CANONICAL$P = Select[I$VARIABLES$CANONICAL,Function[SameQ[StringTake[ToString[Slot[1]],1],I$P$HEAD]]] ;
    (* SET PLANES *)
    ClearAll[I$PLANE] ;
    Apply[Set,List[Array[I$PLANE,I$DIMENSION],Transpose[List[I$VARIABLES$CANONICAL$Q,I$VARIABLES$CANONICAL$P]]]] ;
    (* SET SYMPLECTIC MATRIX FOR GIVEN CANONICAL DIMENSION AND CANONICAL COORDINATES ORDERING (MATRIX) *)
    I$SYMPLECTIC$MATRIX = Which[
      SameQ[I$ORDERING,"PAIRS"],
      ArrayFlatten[Outer[Times,IdentityMatrix[I$DIMENSION],List[List[0,1],List[Subtract[0,1],0]]]],
      SameQ[I$ORDERING,"SECTORS"],
      Join[PadLeft[IdentityMatrix[I$DIMENSION],List[I$DIMENSION,I$DIMENSION$CANONICAL]],PadRight[Subtract[0,IdentityMatrix[I$DIMENSION]],List[I$DIMENSION,I$DIMENSION$CANONICAL]]]
    ] ;
    (* SET PARAMETER SPACE DIMENSION/NUMBER OF PARAMETERS (INTEGER) *)
    I$DIMENSION$PARAMETRIC = KNOB ;
    If[VERBOSE,Print[StringTemplate["PARAMETRIC DIMENSION    : ``"][I$DIMENSION$PARAMETRIC]]] ;
    (* SET PARAMETETRIC VARIABLES (LIST OF SYMBOLS) *)
    I$VARIABLES$PARAMETRIC = ToExpression[Map[Function[StringJoin[I$K$HEAD,ToString[Slot[1]]]],Range[I$DIMENSION$PARAMETRIC]]] ;
    If[VERBOSE,Print[StringTemplate["PARAMETRIC VARIABLES    : ``"][I$VARIABLES$PARAMETRIC]]] ;
    (* SET FLAGS FOR PARAMETERS (USED TO GENERATE SEVERAL ELEMENTS FROM A SINGLE DEFINITION) *)
    I$FLAG$PARAMETRIC = ToExpression[Map[Function[StringJoin[I$F$HEAD,ToString[Slot[1]]]],Range[I$DIMENSION$PARAMETRIC]]] ;
    If[VERBOSE,Print[StringTemplate["PARAMETRIC FLAGS        : ``"][I$FLAG$PARAMETRIC]]] ;
    (* FLAG RULES *)
    I$RULE$PARAMETRIC = Dispatch[Thread[Rule[I$VARIABLES$PARAMETRIC,Times[I$FLAG$PARAMETRIC,I$VARIABLES$PARAMETRIC]]]] ;
    (* SET LIST OF ORDERS FOR CANONICAL VARIABLES AND PARAMETERS (LIST OF INTEGERS) *)
    I$ORDER$LIST = Join[
     ConstantArray[CORDER,I$DIMENSION$CANONICAL],
     If[SameQ[MASK,List[]],ConstantArray[KORDER,I$DIMENSION$PARAMETRIC],Map[Min,Transpose[List[ConstantArray[KORDER,I$DIMENSION$PARAMETRIC],MASK]]]]
    ] ;
    If[VERBOSE,Print[StringTemplate["LIST OF ORDERS          : ``"][Thread[Rule[Flatten[List[I$VARIABLES$CANONICAL,I$VARIABLES$PARAMETRIC]],I$ORDER$LIST]]]]] ;
    (* SET MAXIMUM ORDER OF GENERIC MONOMIAL (CANONICAL VARIABLES AND PARAMETERS) (INTEGER) *)
    I$ORDER = Which[
      SameQ[OptionValue["TRUNCATION"],"SUM"],
      Plus[CORDER,KORDER],
      SameQ[OptionValue["TRUNCATION"],"MAX"],
      Max[List[CORDER,KORDER]]
    ] ;
    I$ORDER$CANONICAL = CORDER ;
    I$ORDER$PARAMETRIC = KORDER ;
    If[VERBOSE,Print[StringTemplate["TRUNCATION              : ``"][OptionValue["TRUNCATION"]]]] ;
    If[VERBOSE,Print[StringTemplate["GENERIC ORDER           : ``"][I$ORDER]]] ;
    (* SET DEVIATION VARIABLES DIMENSION (NUMBER OF CANONICAL VARIABLES AND PARAMETERS WITH NONZERO ORDERS) (INTEGER) *)
    I$DIMENSION$DEVIATION = Length[DeleteCases[I$ORDER$LIST,0]] ;
    If[VERBOSE,Print[StringTemplate["DEVIATION DIMENSION     : ``"][I$DIMENSION$DEVIATION]]] ;
    (* SET DEVIATION VARIABLES (CANONICAL VARIABLES AND PARAMETERS WITH NONZERO ORDERS) (LIST OF SYMBOLS) *)
    I$VARIABLES$DEVIATION = Pick[Flatten[List[I$VARIABLES$CANONICAL,I$VARIABLES$PARAMETRIC]],Unitize[I$ORDER$LIST],1] ;
    If[VERBOSE,Print[StringTemplate["DEVIATION VARIABLES     : ``"][I$VARIABLES$DEVIATION]]] ;
    (* SET ALL POSSIBLE DERIVATIVES OF CANONICAL VARIABLES WITH RESPECT TO DEVIATION VARIABLES UP TO MAXIMUM ORDER <ORDER> (INTEGER) OF GENERIC MONOMIAL *)
    I$COMPONENTS = Flatten[Map[Composition[Reverse,Function[FrobeniusSolve[ConstantArray[1,I$DIMENSION$DEVIATION],Slot[1]]]],Range[I$ORDER]],1] ;
    (* FILTER DERIVATIVES BY CANONICAL VARIABLES (TOTAL CANONICAL MONOMIAL ORDER IS LESS OR EQUAL TO GIVEN CANONICAL ORDER) *)
    I$COMPONENTS = If[UnsameQ[CORDER,0],Select[I$COMPONENTS,Function[LessEqual[Total[Take[Slot[1],I$DIMENSION$CANONICAL]],CORDER]]],I$COMPONENTS] ;
    (* FILTER DERIVATIVES BY PARAMETERS (TOTAL PARAMETRIC MONOMIAL ORDER IS LESS OR EQUAL TO GIVEN ORDER) *)
    I$COMPONENTS = If[And[UnsameQ[CORDER,0],UnsameQ[KORDER,0]],Select[I$COMPONENTS,Function[LessEqual[Total[Take[Slot[1],Subtract[0,I$DIMENSION$PARAMETRIC]]],KORDER]]],I$COMPONENTS] ;
    (* FILTER DERIVATIVES BY PARAMETERS (INDIVIDUAL LIST, PAR_I^POW_J IS REMOVED IF POW_J IS GREATER OR EQUAL TO GIVEN ORDER OF PAR_I) *)
    I$COMPONENTS = Select[I$COMPONENTS,Function[Apply[And,Thread[LessEqual[Slot[1],DeleteCases[I$ORDER$LIST,0]]]]]] ;
    (* APPLY FILTER *)
    I$COMPONENTS = OptionValue["FILTER"][I$COMPONENTS] ;
    (* ADD COMPONENT INDEX (CANONICAL COMPONENT MARKER) TO SELECTED DERIVATIVES *)
    (* COMPONENTS STRUCTURE: {{C_1,D_1},...,{C_N,D_1},{C_1,D_2},...,{C_N,D_2},...,{C_1,D_M},...,{C_N,D_M}} *)
    I$COMPONENTS = Flatten[Transpose[Outer[Composition[Flatten,List],Range[I$DIMENSION$CANONICAL],I$COMPONENTS,1]],1] ;
    (* SET LENGTH OF COMPONENTS (GROUPED BY GENERIC MONOMICAL ORDER) *)
    I$COMPONENTS$LENGTH = Map[Length,GatherBy[I$COMPONENTS,Composition[Total,Rest]]] ;
    If[VERBOSE,Print[StringTemplate["FILTER                  : ``"][UnsameQ[Identity,OptionValue["FILTER"]]]]] ;
    If[VERBOSE,Print[StringTemplate["NUMBER OF COMPONENTS    : ``"][I$COMPONENTS$LENGTH]]] ;
    (* SET DERIVATIVE VARIABLES (DERIVATIVES OF CANONICAL VARIABLES WITH RESPECT TO DEVIATION VARIABLES) (LIST OF SYMBOLS) *)
    (* E.G. D$X$A$B$... -- DERIVATIVE OF X COMPONENT WITH RESPECT TO 1ST DEVIATION VARIABLE 'A' TIMES, 2ND DEVIATION VARIABLE 'B' TIMES, ... *)
    I$VARIABLES$DERIVATIVE = ToExpression[Map[Composition[Function[StringJoin[I$D$HEAD,"$",Slot[1]]],Function[StringRiffle[Slot[1],"$"]],Map[ToString]],I$COMPONENTS]] ;
    (* SET DIMENSION OF DERIVATIVE SPACE (INTEGER) *)
    I$DIMENSION$DERIVATIVE = Length[I$VARIABLES$DERIVATIVE] ;
    If[VERBOSE,Print[StringTemplate["DERIVATIVE DIMENSION    : ``"][I$DIMENSION$DERIVATIVE]]] ;
    (* SET ASTATE VARIABLES (FLAG, PARAMETERS, CANONICAL VARIABLES AND DERIVATIVES OF CANONICAL VARIABLES WITH RESPECT TO DEVIATION VARIABLES) (LIST OF SYMBOLS) *)
    I$VARIABLES = Flatten[List[I$FLAG,I$VARIABLES$PARAMETRIC,I$VARIABLES$CANONICAL,I$VARIABLES$DERIVATIVE]] ;
    (* SET STATE DIMENSION (INTEGER) *)
    I$DIMENSION$VARIABLES = Length[I$VARIABLES] ;
    If[VERBOSE,Print[StringTemplate["STATE DIMENSION         : ``"][I$DIMENSION$VARIABLES]]] ;
    (* SET VARIABLES FOR COMPUTATION OF COMPONENTS REPLACEMENT RULES AND DEFINE REPLACMENT RULES *)
    (* USED TO REPLACE CANONICAL VARIABLES AND PARAMETERS IN GIVEN VECTOR FIELD OR MAP BY GENERIC VARIABLES *)
    ClearAll[I$GP] ;
    ClearAll[I$GV] ;
    ClearAll[I$GENERIC$PAR$LIST] ;
    ClearAll[I$GENERIC$VAR$LIST] ;
    I$GENERIC$PAR$LIST = Array[I$GP,I$DIMENSION$DEVIATION] ;
    I$GENERIC$VAR$LIST = Array[I$GV,I$DIMENSION$CANONICAL] ;
    I$GENERIC$VAR$LIST = Flatten[Outer[Apply,I$GENERIC$VAR$LIST,List[I$GENERIC$PAR$LIST],1]] ;
    I$RULE$COMPONENTS = Transpose[List[I$VARIABLES$DEVIATION,I$GENERIC$PAR$LIST]] ;
    I$RULE$COMPONENTS = Select[I$RULE$COMPONENTS,Composition[Function[MemberQ[Slot[1]][I$VARIABLES$PARAMETRIC]],First]] ;
    I$RULE$COMPONENTS = Join[Transpose[List[I$VARIABLES$CANONICAL,I$GENERIC$VAR$LIST]],I$RULE$COMPONENTS] ;
    I$RULE$COMPONENTS = Dispatch[Map[Apply[Rule],I$RULE$COMPONENTS]] ;
    (* SET VARIABLES FOR COMPUTATION OF DERIVATIVE REPLACEMENT RULES AND DEFINE RULES *)
    (* DEFINES CORRESPONDENCE OF GENERIC DEVIVATIVES AND DEVIVATIVE VARIABLES *)
    I$GENERIC$PAR$LIST = Array[I$GP,I$DIMENSION$DEVIATION] ;
    I$GENERIC$VAR$LIST = Array[I$GV,I$DIMENSION$CANONICAL] ;
    I$GENERIC$VAR$LIST = Flatten[Outer[Apply,I$GENERIC$VAR$LIST,List[I$GENERIC$PAR$LIST],1]] ;
    I$RULE$DERIVATIVE = Map[Function[Operate[Apply[Derivative,Rest[Slot[1]]],Part[I$GENERIC$VAR$LIST,First[Slot[1]]]]],I$COMPONENTS] ;
    I$RULE$DERIVATIVE = Dispatch[Thread[Rule[I$RULE$DERIVATIVE,I$VARIABLES$DERIVATIVE]]] ;
    ClearAll[I$GENERIC$PAR$LIST] ;
    ClearAll[I$GENERIC$VAR$LIST] ;
    (* COMPONENTS FUNCTION BUILDER FROM VECTOR OF DERIVATIVES (NO CONSTANT COMPONENT, ACTS ON A LIST OF DERIVATIVES, RETURNS LIST GROUPED BY TOTAL GENERIC MONOMIAL ORDER) *)
    I$MAKE$FUNCTIONS = Values[GroupBy[I$COMPONENTS,First]] ;
    I$MAKE$FUNCTIONS = Map[
      Function[
        Block[
          {COMPONENT,INDEX,FACTOR,EXPRESSION,LIST},
          COMPONENT = ToString[Part[Slot[1],1,1]] ;
          INDEX = SplitBy[Map[Rest,Slot[1]],Total] ;
          FACTOR = Map[Map[Apply[Multinomial]],INDEX] ;
          EXPRESSION = Map[Map[Function[Apply[Times,Power[I$VARIABLES$DEVIATION,Slot[1]]]]],INDEX] ;
          EXPRESSION = Times[FACTOR,EXPRESSION] ;
          LIST = ToExpression[Apply[Composition[Function[StringJoin[I$D$HEAD,"$",COMPONENT,"$",Slot[1]]],Function[StringRiffle[Slot[1],"$"]],Map[ToString],List],INDEX,List[2]]] ;
          N[Expand[Times[Divide[1,Factorial[Range[Length[LIST]]]],MapThread[Dot,List[LIST,EXPRESSION]]]]]
        ]
      ],
      I$MAKE$FUNCTIONS
    ] ;
    I$MAKE$FUNCTIONS = Apply[Function,List[I$VARIABLES$DERIVATIVE,I$MAKE$FUNCTIONS]] ;
    I$MAKE$FUNCTIONS = Apply[I$MAKE$FUNCTIONS] ;
    (* SET DSOLVE VARIABLES (ONLY CANONICAL VARIABLES) (LIST OF SYMBOLS) *)
    I$DSOLVE$VARIABLES = I$VARIABLES$CANONICAL ;
    (* SET DSOLVE PARAMETERS (GENERIC INITIAL VALUES FOR DSOLVE VARIABLES) (LIST OF SYMBOLS) *)
    I$DSOLVE$PARAMETERS = Flatten[ToExpression[Outer[StringJoin,Map[ToString,I$DSOLVE$VARIABLES],List["$"]]]] ;
    (* SET DSOLVE EQUATIONS FOR INITIALS *)
    I$DSOLVE$INITIALS = Thread[Equal[Through[I$DSOLVE$VARIABLES[0]],I$DSOLVE$PARAMETERS]] ;
    (* SET DSOLVE RULE (REPLACE GENERIC INITIAL VALUES WITH CANONICAL VARIABLES) *)
    I$DSOLVE$RULE = Dispatch[Thread[Rule[I$DSOLVE$PARAMETERS,I$DSOLVE$VARIABLES]]] ;
    (* SET NDSOLVE VARIABLES (CANONICAL VARIABLES AND DERIVATIVES) *)
    I$NDSOLVE$VARIABLES = Flatten[List[I$VARIABLES$CANONICAL,I$VARIABLES$DERIVATIVE]] ;
    (* SET NDSOLVE PARAMETERS (GENERIC INITIAL VALUES FOR NDSOLVE VARIABLES) *)
    I$NDSOLVE$PARAMETERS = Flatten[ToExpression[Outer[StringJoin,Map[ToString,I$NDSOLVE$VARIABLES],List["$"]]]] ;
    (* SET NDSOLVE INITIALS *)
    I$NDSOLVE$INITIALS = Thread[Equal[Through[I$NDSOLVE$VARIABLES[0]],I$NDSOLVE$PARAMETERS]] ;
    (* ADD PARAMETRIC VARIABLES TO NDSOLVE PARAMETERS *)
    I$NDSOLVE$PARAMETERS = Join[I$VARIABLES$PARAMETRIC,I$NDSOLVE$PARAMETERS] ;
    (* CLEAN GLOBAL CODE VARIABLES AND ATTRIBUTES *)
    SetAttributes[I$GP,NHoldAll] ;
    SetAttributes[I$GV,NHoldAll] ;
    ClearAll[I$STATE] ;
    ClearAll[I$GLOBAL] ;
    ClearAll[I$LOCAL] ;
    SetAttributes[I$STATE,NHoldAll] ;
    SetAttributes[I$LOCAL,NHoldAll] ;
    SetAttributes[I$GLOBAL,NHoldAll] ;
    (* CLEAN GLOBAL CODE FUNCTIONS AND SET ATTRIBUTES *)
    ClearAll[I$BLOCK] ;
    ClearAll[I$COMPOUND$EXPRESSION] ;
    ClearAll[I$SEQUENCE] ;
    ClearAll[I$SET] ;
    ClearAll[I$PLUS] ;
    ClearAll[I$TIMES] ;
    ClearAll[I$DIVIDE] ;
    ClearAll[I$POWER] ;
    ClearAll[I$EXP] ;
    ClearAll[I$SIN] ;
    ClearAll[I$COS] ;
    ClearAll[I$TAN] ;
    ClearAll[I$COT] ;
    ClearAll[I$ARCSIN] ;
    ClearAll[I$ARCCOS] ;
    ClearAll[I$ARCTAN] ;
    ClearAll[I$ARCCOT] ;
    ClearAll[I$SINH] ;
    ClearAll[I$COSH] ;
    ClearAll[I$MATH$E] ;
    ClearAll[I$MATH$PI] ;
    ClearAll[I$IF] ;
    ClearAll[I$DO] ;
    ClearAll[I$GREATER] ;
    ClearAll[I$LESS] ;
    ClearAll[I$GREATER$EQUAL] ;
    ClearAll[I$LESS$EQUAL] ;
    ClearAll[I$EQUAL] ;
    ClearAll[I$LIST] ;
    ClearAll[I$THROW] ;
    ClearAll[I$CATCH] ;
    ClearAll[I$COMPILE] ;
    ClearAll[I$SQRT] ;
    ClearAll[I$PART] ;
    ClearAll[I$LOG] ;
    ClearAll[I$LOG10] ;
    SetAttributes[I$POWER,NHoldRest] ;
    (* CODE REPLACEMENT RULES (REPLACE VARIABLES WITH CODE STATE) *)
    I$RULE$STATE = Dispatch[Thread[Rule[I$VARIABLES,Array[I$STATE,I$DIMENSION$VARIABLES]]]] ;
    (* RETURN SIGNATURE (INPUT ARGUMENTS) *)
    I$SIGNATURE
  ]
] /; And[
  GreaterEqual[DIMENSION,0],
  GreaterEqual[KNOB,0],
  GreaterEqual[CORDER,0],
  GreaterEqual[KORDER,0]
] ;
(* ABBREVIATION(S) *)
I$INTEGRATOR[DIRECTORY_String,DIMENSION_Integer,OPTIONS:OptionsPattern[]] := I$INTEGRATOR[DIRECTORY,DIMENSION,0,0,0,List[],OPTIONS] ;
I$INTEGRATOR[DIRECTORY_String,DIMENSION_Integer,CORDER_Integer,OPTIONS:OptionsPattern[]] := I$INTEGRATOR[DIRECTORY,DIMENSION,CORDER,0,0,List[],OPTIONS] ;
(* GLOBAL VARIABLES AND FUNCTIONS DEFINED BY I$INTEGRATOR[] *)
I$SIGNATURE::usage = "I$SIGNATURE -- (variable) integrator signature (list of input arguments for I$INTEGRATOR) " ;
I$DIRECTORY::usage = "I$DIRECTORY -- (variable) working directory (path) " ;
I$FLAG::usage = "I$FLAG -- (variable) aperture flag parameter (symbol) " ;
I$PARAMETER::usage = "I$PARAMETER -- (variable) integration parameter (symbol) " ;
I$ORDERING::usage = "I$ORDERING -- (variable) canonical space ordering (string) " ;
I$Q$HEAD::usage = "I$Q$HEAD -- (variable) canonical q-coordinates head (string) " ;
I$P$HEAD::usage = "I$P$HEAD -- (variable) canonical p-coordinates head (string) " ;
I$K$HEAD::usage = "I$K$HEAD -- (variable) parameter head (string) " ;
I$D$HEAD::usage = "I$D$HEAD -- (variable) derivative head (string) " ;
I$F$HEAD::usage = "I$F$HEAD -- (variable) parameter flag head (string) " ;
I$DIMENSION::usage = "I$DIMENSION -- (variable) configuration space dimension (integer) " ;
I$DIMENSION$CANONICAL::usage = "I$DIMENSION$CANONICAL -- (variable) canonical space dimension (integer) " ;
I$VARIABLES$CANONICAL::usage = "I$VARIABLES$CANONICAL -- (variable) list of canonical variables (list of symbols) " ;
I$VARIABLES$CANONICAL$Q::usage = "I$VARIABLES$CANONICAL$Q -- (variable) list of canonical q-variables (list of symbols) " ;
I$VARIABLES$CANONICAL$P::usage = "I$VARIABLES$CANONICAL$P -- (variable) list of canonical p-variables (list of symbols) " ;
I$PLANE::usage = "I$PLANE[INDEX] -- canonical pair corresponding to <INDEX> (integer) plane " ;
I$SYMPLECTIC$MATRIX::usage = "I$SYMPLECTIC$MATRIX -- (variable) symplectic matrix " ;
I$DIMENSION$PARAMETRIC::usage = "I$DIMENSION$PARAMETRIC -- (variable) parametric dimension (integer) " ;
I$VARIABLES$PARAMETRIC::usage = "I$VARIABLES$PARAMETRIC -- (variable) parametric variables (list of symbols) " ;
I$FLAG$PARAMETRIC::usage = "I$FLAG$PARAMETRIC -- (variable) parametric flag variables (list of symbols) " ;
I$RULE$PARAMETRIC::usage = "I$RULE$PARAMETRIC -- (rule) parametric rules (rules) " ;
I$ORDER$LIST::usage = "I$ORDER$LIST -- (variable) list of orders for all deviation variables (list of integers) " ;
I$ORDER::usage = "I$ORDER -- (variable) maximum generic monomial order (integer) " ;
I$DIMENSION$DEVIATION::usage = "I$DIMENSION$DEVIATION -- (variable) deviation space dimension (integer) " ;
I$VARIABLES$DEVIATION::usage = "I$VARIABLES$DEVIATION -- (variable) list of deviation variables (list of symbols) " ;
I$COMPONENTS::usage = "I$COMPONENTS -- (variable) selected derivative components (list) " ;
I$COMPONENTS$LENGTH::usage = "I$COMPONENTS$LENGTH -- (variable) length of selected derivative components grouped by generic order (list of integers) " ;
I$VARIABLES$DERIVATIVE::usage = "I$VARIABLES$DERIVATIVE -- (variable) list of derivative variables (list of symbols) " ;
I$DIMENSION$DERIVATIVE::usage = "I$DIMENSION$DERIVATIVE -- (variable)derivative space dimension (integer) " ;
I$RULE$COMPONENTS::usage = "I$RULE$COMPONENTS -- (rule) generic rules for components (rules) " ;
I$RULE$DERIVATIVE::usage = "I$RULE$DERIVATIVE -- (rule) generic rules for derivaties (rules) ";
I$MAKE$FUNCTIONS::usage = "I$MAKE$FUNCTIONS -- (function) components function builder (function) " ;
I$VARIABLES::usage = "I$VARIABLES -- (variable) state variables (list of symbols) " ;
I$DIMENSION$VARIABLES::usage = "I$DIMENSION$VARIABLES -- (variable) state dimension (integer) " ;
I$DSOLVE$VARIABLES::usage = "I$DSOLVE$VARIABLES -- (variable) dsolve variables " ;
I$DSOLVE$PARAMETERS::usage = "I$DSOLVE$PARAMETERS -- (variable) dsolve parameters " ;
I$DSOLVE$INITIALS::usage = "I$DSOLVE$INITIALS -- (variable) dsolve initials " ;
I$DSOLVE$RULE::usage = "I$DSOLVE$RULE -- (rule) dsolve rules " ;
I$NDSOLVE$VARIABLES::usage = "I$NDSOLVE$VARIABLES -- (variable) ndsolve variables " ;
I$NDSOLVE$PARAMETERS::usage = "I$NDSOLVE$PARAMETERS -- (variable) ndsolve parameters " ;
I$NDSOLVE$INITIALS::usage = "I$NDSOLVE$INITIALS -- (variable) ndsolve initials " ;
I$RULE$STATE::usage = "I$RULE$STATE -- (rule) replacement rules for code generation " ;
(* ################################################################################################################################################################ *)
(* MONOMIAL EXPANSION *)
(* ################################################################################################################################################################ *)
ClearAll[I$EXPAND]
I$EXPAND::usage = "
I$EXPAND[DEGREE,VARIABLE,EXPRESSION] -- (function) expand expression <EXPRESSION> near zero with respect to list of monomial variables <VARIABLE> (list of symbols) up to total monomial degree <DEGREE> (integer)
I$EXPAND[EXPRESSION] -- (function) expand expression <EXPRESSION> near zero using current integrator signature)
" ;
I$EXPAND[                         (* -- (SERIES) EXPAND EXPRESSION *)
  DEGREE_Integer,                 (* -- TOTAL MONOMIAL DEGREE (INTEGER) *)
  VARIABLE_List,                  (* -- EXPANSION VARIABLES (LIST OF SYMBOLS) *)
  EXPRESSION_                     (* -- EXPRESSION TO EXPAND *)
] := Catch[
  Block[
      {PARAMETER,RULE},
      RULE = Dispatch[Thread[Rule[VARIABLE,Times[PARAMETER,VARIABLE]]]] ;
      ReplaceAll[Normal[Series[ReplaceAll[EXPRESSION,RULE],List[PARAMETER,0,DEGREE]]],Rule[PARAMETER,1]]
  ]
] /; And[
  GreaterEqual[DEGREE,0],
  SameQ[DeleteCases[Map[Head,VARIABLE],Symbol],List[]]
] ;
I$EXPAND[List[DEGREE_Integer,VARIABLE_Symbol],EXPRESSION_] := I$EXPAND[DEGREE,List[VARIABLE],EXPRESSION] ;
I$EXPAND[                         (* -- (SERIES) EXPAND EXPRESSION ACCORDING TO INTEGRATOR SIGNATURE *)
  EXPRESSION_                     (* -- EXPRESSION TO EXPAND *)
] := Block[
  {SERIES, LIST},
  SERIES = I$EXPAND[I$ORDER,I$VARIABLES$CANONICAL,EXPRESSION] ;
  SERIES = I$EXPAND[I$ORDER,I$VARIABLES$DEVIATION,EXPRESSION] ;
  If[
    UnsameQ[List[],Intersection[I$VARIABLES$DEVIATION,I$VARIABLES$CANONICAL]],
    SERIES = I$EXPAND[I$ORDER$CANONICAL,I$VARIABLES$CANONICAL,SERIES] ;
  ] ;
  If[
    UnsameQ[List[],Intersection[I$VARIABLES$DEVIATION,I$VARIABLES$PARAMETRIC]],
	  LIST = DeleteCases[Transpose[List[I$ORDER$LIST,I$VARIABLES$DEVIATION]],List[_,X_]/; MemberQ[I$VARIABLES$CANONICAL,X]] ;
    SERIES = I$EXPAND[I$ORDER$PARAMETRIC,Last[Transpose[LIST]],SERIES] ;
    SERIES = Fold[Composition[Apply[I$EXPAND],Reverse,List],SERIES,LIST] ;
  ] ;
  Collect[SERIES,I$VARIABLES$CANONICAL]
] ;
(* ################################################################################################################################################################ *)
(* SYMPLECTIC TEST (MATRIX) *)
(* ################################################################################################################################################################ *)
ClearAll[I$SYMPLECTIC$MATRIX$Q] ;
I$SYMPLECTIC$MATRIX$Q::usage = "
I$SYMPLECTIC$MATRIX$Q[MATRIX] -- (function) test symplectic condition for given matrix <MATRIX>
" ;
I$SYMPLECTIC$MATRIX$Q[MATRIX_?SquareMatrixQ] := SameQ[Chop[Map[Simplify,Flatten[Subtract[Dot[Transpose[MATRIX],I$SYMPLECTIC$MATRIX,MATRIX],I$SYMPLECTIC$MATRIX]]]],ConstantArray[0,I$DIMENSION$CANONICAL^2]] /; SameQ[Dimensions[I$SYMPLECTIC$MATRIX],Dimensions[ConstantArray[I$VARIABLES$CANONICAL,I$DIMENSION$CANONICAL]]] ;
(* ################################################################################################################################################################ *)
(* SYMPLECTIC TEST (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$SYMPLECTIC$MAP$Q] ;
I$SYMPLECTIC$MAP$Q::usage = "
I$SYMPLECTIC$MAP$Q[MAP] -- (function) test symplectic condition for given map <MAP>
" ;
I$SYMPLECTIC$MAP$Q[MAP_?ArrayQ] := I$SYMPLECTIC$MATRIX$Q[D[MAP,List[I$VARIABLES$CANONICAL]]] /; SameQ[Length[MAP],I$DIMENSION$CANONICAL] ;
(* ################################################################################################################################################################ *)
(* DERIVATIVE OF HAMILTONIAN VECTOR FIELD OR CANONICAL MAP FOR SELECTED COMPONENTS *)
(* ################################################################################################################################################################ *)
ClearAll[I$DERIVATIVE] ;
I$DERIVATIVE::usage = "
I$DERIVATIVE[DATA] -- (function) compute derivatives for given vector field (or canonical map) <DATA> (list), with or without parameters, add flags to parameters, i.e. KNOB_I -> FLAG_I*KNOB_I
" ;
Options[I$DERIVATIVE] := List[
  Rule["MAP",Map],                (* -- MAP FUNCTION (E.G. Map, ParallelMap, I$PARALLEL$MAP) *)
  Rule["PROLOG",Identity],        (* -- PROLOG FUNCTION (ACTS ON INPUT) *)
  Rule["EPILOG",Identity]         (* -- EPILOG FUNCTION (ACTS AFTER EACH APPLICATION OF DERIVATIVE) *)
] ;
I$DERIVATIVE[                     (* -- GENERATE VECTOR FIELD (OR MAP) FOR DERIVATIVES FOR GIVEN VECTOR FIELD (OR CANONICAL MAP) (OUTPUT ORDERING IS IDENTICAL TO I$COMPONENTS OR I$VARIABLES$DERIVATIVE) *)
  DATA_List,                      (* -- VECTOR FIELD OR CANONICAL MAP WITH EXPLICIT DEPENDENCE ON CANONICAL VARIABLES AND PARAMETERS (WITHOUT PARAMETRIC FLAGS) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {FUNCTION,PROLOG,EPILOG,RESULT,PARAMETER,VARIABLE,RULE},
    (* SET FUNCTIONS *)
    FUNCTION = OptionValue["MAP"] ;
    PROLOG = OptionValue["PROLOG"] ;
    EPILOG = OptionValue["EPILOG"] ;
    (* PROCESS INPUT *)
    RESULT = FUNCTION[PROLOG,DATA] ;
    (* ADD FLAGS TO PARAMETERS *)
    RESULT = ReplaceAll[RESULT,I$RULE$PARAMETRIC] ;
    (* DIRECT REPLACEMENT RULES (CONVERT INPUT TO GENERIC VARIABLES) *)
    ClearAll[I$GP] ;
    ClearAll[I$GV] ;
    PARAMETER = Array[I$GP,I$DIMENSION$DEVIATION] ;
    VARIABLE = Array[I$GV,I$DIMENSION$CANONICAL] ;
    VARIABLE = Flatten[Outer[Apply,VARIABLE,List[PARAMETER],1]] ;
    (* APPLY RULES *)
    RESULT = FUNCTION[ReplaceAll[I$RULE$COMPONENTS],RESULT] ;
    (* INVERSE REPLACEMENT RULES *)
    RULE = Dispatch[Map[Reverse,Normal[I$RULE$COMPONENTS]]] ;
    (* COMPUTE DERIVATIVES (FOR EACH COMPONENT) (DERIVATIVE IS COMPUTED FOR GENERIC GENERIC VARIABLES) *)
    RESULT = FUNCTION[
      Function[
        Block[
          {VALUE,LIST,DERIVATIVE},
          VALUE = First[Slot[1]] ;
          LIST = Rest[Slot[1]] ;
          DERIVATIVE = Reverse[Transpose[List[PARAMETER,LIST]]] ;
          DERIVATIVE = DeleteCases[DERIVATIVE,List[_,0]] ;
          DERIVATIVE = Fold[Composition[EPILOG,D],Part[RESULT,VALUE],DERIVATIVE] ;
          Fold[ReplaceAll,DERIVATIVE,List[I$RULE$DERIVATIVE,RULE]]
        ]
      ],
      I$COMPONENTS
    ] ;
    (* OUTPUT *)
    RESULT
  ]
] /; And[
  SameQ[Length[DATA],I$DIMENSION$CANONICAL],
  SameQ[Head[I$COMPONENTS],List]
] ;
(* ################################################################################################################################################################ *)
(* POISSON BRACKET *)
(* ################################################################################################################################################################ *)
ClearAll[I$POISSON$BRACKET] ;
I$POISSON$BRACKET::usage = "
I$POISSON$BRACKET[F,G] -- compute Poisson bracket between two canonical observables <F> and <G> (functions of canonical variables)
" ;
I$POISSON$BRACKET := Function[Dot[D[Slot[1],List[I$VARIABLES$CANONICAL]],I$SYMPLECTIC$MATRIX,D[Slot[2],List[I$VARIABLES$CANONICAL]]]] ;
(* ################################################################################################################################################################ *)
(* GRADIENT FIELD (HAMILTONIAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$GRADIENT$FIELD] ;
I$GRADIENT$FIELD::usage = "
I$GRADIENT$FIELD[HAMILTONIAN] -- compute gradient vector field for given Hamiltonian function <HAMILTONIAN> (function of canonical variables)
" ;
I$GRADIENT$FIELD := Function[D[Slot[1],List[I$VARIABLES$CANONICAL]]] ;
(* ################################################################################################################################################################ *)
(* VECTOR FIELD (HAMILTONIAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$VECTOR$FIELD] ;
I$VECTOR$FIELD::usage = "
I$VECTOR$FIELD[HAMILTONIAN] -- compute Hamiltonian vector field for given Hamiltonian function <HAMILTONIAN> (function of canonical variables)
" ;
I$VECTOR$FIELD := Function[Dot[I$SYMPLECTIC$MATRIX,I$GRADIENT$FIELD[Slot[1]]]] ;
(* ################################################################################################################################################################ *)
(*  EQUATIONS (DSOLVE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$DSOLVE$SYSTEM] ;
I$DSOLVE$SYSTEM::usage = "
I$DSOLVE$SYSTEM[VECTOR] -- generate DSolve[] input template (flow with generic initial conditions) for given hamiltonian vector field <VECTOR> (function of canonical variables)
" ;
I$DSOLVE$SYSTEM := Function[Join[Thread[Equal[D[Through[I$DSOLVE$VARIABLES[I$PARAMETER]],I$PARAMETER],ReplaceAll[Slot[1],Thread[Rule[I$DSOLVE$VARIABLES,Through[I$DSOLVE$VARIABLES[I$PARAMETER]]]]]]],I$DSOLVE$INITIALS]] ;
(* ################################################################################################################################################################ *)
(* USE DSolve[] TO FIND SOLUTION (ACTS ON LIST OF SOVABLE HAMILTONIANS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$DSOLVE] ;
I$DSOLVE::usage = "
I$DSOLVE[LIST] -- solve Hamiltonian equations with DSolve[] for given list of sovable Hamiltonan functions <LIST>
" ;
I$DSOLVE := Map[Composition[Simplify,ReplaceAll[I$DSOLVE$RULE],Function[Through[Slot[1][I$PARAMETER]]],Function[Apply[DSolveValue,List[Slot[1],I$DSOLVE$VARIABLES,I$PARAMETER]]],I$DSOLVE$SYSTEM,I$VECTOR$FIELD]] ;
(* ################################################################################################################################################################ *)
(* VECTOR FIELD DERIVATIVE *)
(* ################################################################################################################################################################ *)
ClearAll[I$VECTOR$FIELD$DERIVATIVE] ;
I$VECTOR$FIELD$DERIVATIVE::usage = "
I$VECTOR$FIELD$DERIVATIVE[VECTOR] -- add vector field for derivatives to given canonical vector field <VECTOR> (function of canonical variables), add flags to parameters, i.e. KNOB_I -> FLAG_I*KNOB_I
" ;
I$VECTOR$FIELD$DERIVATIVE := Function[Flatten[List[ReplaceAll[Slot[1],I$RULE$PARAMETRIC],I$DERIVATIVE[Slot[1]]]]] ;
(* ################################################################################################################################################################ *)
(* MAP DERIVATIVE *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAP$DERIVATIVE] ;
I$MAP$DERIVATIVE::usage = "
I$MAP$DERIVATIVE[MAP] -- compute state map, add map for derivatives to given canonical map <MAP> (function of canonical variables), add aperture flag, parameters and flags for parameters), i.e. KNOB_I -> FLAG_I*KNOB_I
" ;
I$MAP$DERIVATIVE := Function[Flatten[List[I$FLAG,I$VARIABLES$PARAMETRIC,I$VECTOR$FIELD$DERIVATIVE[Slot[1]]]]] ;
(* ################################################################################################################################################################ *)
(* MAKE (STANDARD) INITIAL CONDITION *)
(* ################################################################################################################################################################ *)
(* NOTE: STATE (FULL INITIAL CONDITION) STRUCTURE {<FLAG>,<KNOB_1>,<KNOB_2>,...,<CANONICAL_1>,<CANONICAL_2>,...,<DERIVATIVE_1>,<DERIVATIVE_2>,...} *)
(*       <FLAG>                            -- APERTURE FLAG (0/1)   *)
(*       <KNOB_1>,<KNOB_2>,...             -- PARAMETERS            *)
(*       <CANONICAL_1>,<CANONICAL_2>,...   -- CANONICAL VARIABLES   *)
(*       <DERIVATIVE_1>,<DERIVATIVE_2>,... -- DERIVATIVES           *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$INITIAL] ;
I$MAKE$INITIAL::usage = "
I$MAKE$INITIAL[KNOB,CANONICAL] -- generate standard initial state for given list of parameters <KNOB> (list) and list of canonical coordinates <CANONICAL> (list)
I$MAKE$INITIAL[CANONICAL] -- generate standard initial state for given list of canonical coordinates <CANONICAL> (list)
" ;
I$MAKE$INITIAL[                   (* -- GENERATE INITIAL CONDITION (TAYLOR MAP COMPUTATION) *)
  KNOB_List,                      (* -- LIST OF VALUES FOR PARAMETER VARIABLES *)
  CANONICAL_List                  (* -- LIST OF VALUES FOR CANONICAL VARIABLES *)
] := Catch[
  N[PadRight[Flatten[List[1,KNOB,CANONICAL,If[UnsameQ[I$VARIABLES$DEVIATION,List[]],Transpose[D[I$VARIABLES$CANONICAL,List[I$VARIABLES$DEVIATION]]],List[]]]],I$DIMENSION$VARIABLES]]
] /; And[
  SameQ[Length[KNOB],I$DIMENSION$PARAMETRIC],
  SameQ[Length[CANONICAL],I$DIMENSION$CANONICAL]
] ;
(* ABBREVIATION(S) *)
I$MAKE$INITIAL[CANONICAL_List] := I$MAKE$INITIAL[ConstantArray[0,I$DIMENSION$PARAMETRIC],CANONICAL] ;
(* ################################################################################################################################################################ *)
(* MAKE STATE FROM (CANONICAL) COMPONENTS (STATE REPRESENTATION) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$STATE] ;
I$MAKE$STATE::usage = "
I$MAKE$STATE[KNOB,COMPONENT] -- convert series representation to state for given list of parameters <KNOB> (list) and components <COMPONENT> (list)
I$MAKE$STATE[COMPONENT] -- convert series representation to state for given list of canonical components <COMPONENT> (list)
" ;
I$MAKE$STATE[                     (* -- CONVERT CANONICAL COMPONENTS TO STATE *)
  KNOB_List,                      (* -- PARAMETERS (LIST) *)
  COMPONENT_List                  (* -- CANONICAL COMPONENTS (LIST) *)
] := Block[
  {DERIVATIVE},
  DERIVATIVE = Map[
    Function[
      Block[
        {INDEX,SIGNATURE},
        INDEX = First[Slot[1]] ;
        SIGNATURE = Rest[Slot[1]] ;
        SIGNATURE = Apply[Sequence,Transpose[List[I$VARIABLES$DEVIATION,SIGNATURE]]] ;
        D[Part[COMPONENT,INDEX],SIGNATURE]
      ]
    ],
    I$COMPONENTS
  ] ;
  Flatten[N[ReplaceAll[List[1,KNOB,COMPONENT,DERIVATIVE],Dispatch[Thread[Rule[I$VARIABLES,0]]]]]]
] /; And[
  SameQ[Length[KNOB],I$DIMENSION$PARAMETRIC],
  SameQ[Length[COMPONENT],I$DIMENSION$CANONICAL]
] ;
(* ABBREVIATION(S) *)
I$MAKE$STATE[COMPONENT_List] := I$MAKE$STATE[ConstantArray[0,I$DIMENSION$PARAMETRIC],COMPONENT] ;
(* ################################################################################################################################################################ *)
(* MAKE CANONICAL COMPONENTS FROM STATE *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$COMPONENTS] ;
I$MAKE$COMPONENTS::usage = "
I$MAKE$COMPONENTS[STATE] -- make canonical components (series representation) for given state <STATE> (list)
" ;
I$MAKE$COMPONENTS[                (* -- MAKE CANONICAL COMPONENTS FROM GIVEN STATE (SERIES REPRESENTATION) *)
  STATE_List                      (* -- STATE (LIST) *)
] := Block[
  {ZERO,PARAMETER,DERIVATIVE},
  ZERO = Drop[STATE,1] ;
  ZERO = Drop[ZERO,I$DIMENSION$PARAMETRIC] ;
  ZERO = Take[ZERO,I$DIMENSION$CANONICAL] ;
  If[
    UnsameQ[I$DIMENSION$DERIVATIVE,0],
    PARAMETER = Drop[STATE,1] ;
    PARAMETER = Take[PARAMETER,I$DIMENSION$PARAMETRIC] ;
    DERIVATIVE = Drop[STATE,1] ;
    DERIVATIVE = Drop[DERIVATIVE,I$DIMENSION$PARAMETRIC] ;
    DERIVATIVE = Drop[DERIVATIVE,I$DIMENSION$CANONICAL] ;
    DERIVATIVE = Map[Total,I$MAKE$FUNCTIONS[DERIVATIVE]] ;
    Plus[ZERO,DERIVATIVE],
    ZERO
  ]
] /; SameQ[Length[STATE],I$DIMENSION$VARIABLES] ;
(* ################################################################################################################################################################ *)
(* COMPOSE CANONICAL MAPS *)
(* ################################################################################################################################################################ *)
ClearAll[I$COMPOSE] ;
Options[I$COMPOSE] = List[
  Rule["LINEAR",True],            (* -- LINEAR FLAG (IF 'True' ALL MAPS ARE ASSUMED TO BE LINEAR) *)
  Rule["DEGREE",1],               (* -- EXPANSION DEGREE FOR NONLINEAR CASE (USE NEGATIVE ONE IF NO EXPANSION IS REQUIRED) *)
  Rule["SIMPLIFY",Identity]       (* -- SIMPLIFICATION FUNCTION *)
] ;
I$COMPOSE::usage = "
I$COMPOSE[LIST] -- compose list of canonical maps <LIST> (list)
" ;
I$COMPOSE[                        (* -- COMPOSE LIST OF PHASE SPACE MAPS *)
  LIST_List,                      (* -- LIST OF CANONICAL MAPS TO COMPOSE *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {MATRIX,MAP,RESULT,LOOP},
    If[
      OptionValue["LINEAR"],
      MATRIX = D[LIST,List[I$VARIABLES$CANONICAL]] ;
      MATRIX = Reverse[MATRIX] ;
      MATRIX = Apply[Dot,MATRIX] ;
      RESULT = Dot[MATRIX,I$VARIABLES$CANONICAL] ;
      Throw[RESULT]
    ] ;
    MAP = LIST ;
    MAP = Map[Apply[Function],Transpose[List[ConstantArray[I$VARIABLES$CANONICAL,Length[MAP]],MAP]]] ;
    RESULT = I$VARIABLES$CANONICAL ;
    Do[
      List[
        RESULT = Apply[Part[MAP,LOOP],RESULT] ;
        If[
          UnsameQ[OptionValue["DEGREE"],Subtract[0,1]],
          RESULT = I$EXPAND[OptionValue["DEGREE"],I$VARIABLES$CANONICAL,RESULT] ;
        ] ;
        RESULT = OptionValue["SIMPLIFY"][RESULT] ;
      ],
      List[LOOP,1,Length[LIST]]
    ] ;
    RESULT
  ]
] /; Apply[And,Thread[Equal[Map[Length,LIST],I$DIMENSION$CANONICAL]]] ;
(* ################################################################################################################################################################ *)
(* GENERATE NDSOLVE EQUATIONS FOR GIVEN TOTAL VECTOR FIELD (I.E. WITH DERIVATIVES) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$FLOW] ;
I$NDSOLVE$FLOW::usage = "
I$NDSOLVE$FLOW[VECTOR] -- generate flow equations to be used with NDSolve[] for given total vector field <VECTOR> (list)
" ;
I$NDSOLVE$FLOW := Function[Thread[Equal[D[Through[I$NDSOLVE$VARIABLES[I$PARAMETER]],I$PARAMETER],ReplaceAll[Slot[1],Dispatch[Thread[Rule[I$NDSOLVE$VARIABLES,Through[I$NDSOLVE$VARIABLES[I$PARAMETER]]]]]]]]] ;
(* ################################################################################################################################################################ *)
(* GENERATE NDSOLVE EQUATIONS WITH INITIAL CONDITIONS FOR GIVEN TOTAL VECTOR FIELD (I.E. WITH DERIVATIVES) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$SYSTEM] ;
I$NDSOLVE$SYSTEM::usage = "
I$NDSOLVE$SYSTEM[VECTOR] -- generate NDSolve[] template (equations and initial conditions) for given total vector field <VECTOR>
" ;
I$NDSOLVE$SYSTEM := Function[Join[I$NDSOLVE$FLOW[Slot[1]],I$NDSOLVE$INITIALS]] ;
(* ################################################################################################################################################################ *)
(* SET NDSOLVE METHOD (PREDIFINED OR VALID METHOD FOR TIME INTEGRATION), ALL PREDIFINED METHODS ARE SYMPLECTIC *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$SET$METHOD] ;
I$NDSOLVE$SET$METHOD::usage = "
I$NDSOLVE$SET$METHOD[METHOD] -- set NDSolve[] method with <METHOD> being one of the predefined methods (integer = 1,2,...,8) or any valid NDSolve[] method for \"TimeIntegration\"
" ;
I$NDSOLVE$SET$METHOD := Function[
  Which[
    SameQ[Slot[1],1], (* -- FS SRK10 *)
    List["FixedStep",Rule[Method,List["ImplicitRungeKutta",Rule["DifferenceOrder",10],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]]]],
    SameQ[Slot[1],2], (* -- FS SRK20 *)
    List["FixedStep",Rule[Method,List["ImplicitRungeKutta",Rule["DifferenceOrder",20],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]]]],
    SameQ[Slot[1],3], (* -- FS SRK30 *)
    List["FixedStep",Rule[Method,List["ImplicitRungeKutta",Rule["DifferenceOrder",30],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]]]],
    SameQ[Slot[1],4], (* -- FS SRK40 *)
    List["FixedStep",Rule[Method,List["ImplicitRungeKutta",Rule["DifferenceOrder",40],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]]]],
    SameQ[Slot[1],5], (* -- AS SRK10 *)
    List["ImplicitRungeKutta",Rule["DifferenceOrder",10],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]],
    SameQ[Slot[1],6], (* -- AS SRK20 *)
    List["ImplicitRungeKutta",Rule["DifferenceOrder",20],Rule["Coefficients","ImplicitRungeKuttaGaussCoefficients"],Rule["ImplicitSolver",List["Newton",Rule[AccuracyGoal,MachinePrecision],Rule[PrecisionGoal,MachinePrecision],Rule["IterationSafetyFactor",1]]]],
    SameQ[Slot[1],7], (* -- SPRK4 *)
    List["FixedStep",Rule[Method,List["SymplecticPartitionedRungeKutta",Rule["DifferenceOrder",4],Rule["PositionVariables",Through[I$VARIABLES$CANONICAL$Q[I$PARAMETER]]]]]],
    SameQ[Slot[1],8], (* -- SPRK8 *)
    List["FixedStep",Rule[Method,List["SymplecticPartitionedRungeKutta",Rule["DifferenceOrder",8],Rule["PositionVariables",Through[I$VARIABLES$CANONICAL$Q[I$PARAMETER]]]]]],
    True,             (* -- USER *)
    Slot[1]
  ]
] ;
(* ################################################################################################################################################################ *)
(* SET NDSOLVE OPTIONS (INTEGRATION STEP SIZE, NUMBER OF INTEGRATION STEPS AND INTEGRATION METHOD) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$SET$OPTIONS] ;
I$NDSOLVE$SET$OPTIONS::usage = "
I$NDSOLVE$SET$OPTIONS[STEP,LENGTH,METHOD] -- set (subset of) NDSolve[] options for given number of integration steps <STEP> (integer), integration length <LENGTH> (real) and integration method <METHOD> (valid method)
" ;
I$NDSOLVE$SET$OPTIONS[            (* -- SET NDSOLVE OPTIONS *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_                         (* -- INTEGRATION METHOD (VALID NDSolve[] METHOD OR PREDEFINED) *)
] := List[
  Rule[Method,List[Rule["EquationSimplification","Solve"],Rule["ParametricCaching",None],Rule["ParametricSensitivity",None],Rule["TimeIntegration",I$NDSOLVE$SET$METHOD[METHOD]]]],
  Rule[StartingStepSize,Divide[LENGTH,STEP]],
  Rule[MaxStepFraction,Divide[1,STEP]],
  Rule[Compiled,True],
  Rule[MaxSteps,Infinity],
  Rule[DependentVariables,I$NDSOLVE$VARIABLES]
] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE STATE (TOTAL VECTOR FIELD) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$STATE] ;
I$NDSOLVE$STATE::usage = "
I$NDSOLVE$STATE[VECTOR,STEP,LENGTH,METHOD] -- generate state object from total vector field <VECTOR> (list) to be integrated for number of steps <STEP> (integer) over length <LENGTH> (real) and integration method <METHOD>
" ;
I$NDSOLVE$STATE[                  (* -- GENERATE STATE (TOTAL VECTOR FIELD) *)
  VECTOR_List,                    (* -- TOTAL VECTOR FIELD *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_                         (* -- INTEGRATION METHOD *)
] := Apply[
  ParametricNDSolveValue[
    N[I$NDSOLVE$SYSTEM[ReplaceAll[VECTOR,Dispatch[Thread[Rule[I$FLAG$PARAMETRIC,1]]]]]],
    I$NDSOLVE$VARIABLES,
    List[I$PARAMETER,0,LENGTH],
    I$NDSOLVE$PARAMETERS,
    I$NDSOLVE$SET$OPTIONS[STEP,LENGTH,METHOD]
  ]
] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE STATE (VECTOR FIELD) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$STATE$VECTOR] ;
I$NDSOLVE$STATE$VECTOR::usage = "
I$NDSOLVE$STATE$VECTOR[VECTOR,STEP,LENGTH,METHOD] -- generate state object from vector field <VECTOR> (list) to be integrated for number of steps <STEP> (integer) over length <LENGTH> (real)  and integration method <METHOD>
" ;
I$NDSOLVE$STATE$VECTOR[           (* -- GENERATE STATE (VECTOR FIELD) *)
  VECTOR_List,                    (* -- VECTOR FIELD *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_                         (* -- INTEGRATION METHOD *)
] := I$NDSOLVE$STATE[I$VECTOR$FIELD$DERIVATIVE[VECTOR],STEP,LENGTH,METHOD] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE STATE (HAMILTONIAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$STATE$SCALAR] ;
I$NDSOLVE$STATE$SCALAR::usage = "
I$NDSOLVE$STATE$SCALAR[HAMILTONIAN,STEP,LENGTH,METHOD] -- generate state object from Hamiltonian <HAMILTONIAN> to be integrated for number of steps <STEP> (integer) over length <LENGTH> (real) and integration method <METHOD>
" ;
I$NDSOLVE$STATE$SCALAR[           (* -- GENERATE STATE (HAMILTONIAN) *)
  HAMILTONIAN_,                   (* -- HAMILTONIAN *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_                         (* -- INTEGRATION METHOD *)
] := I$NDSOLVE$STATE$VECTOR[I$VECTOR$FIELD[HAMILTONIAN],STEP,LENGTH,METHOD] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE STATE ITERATION (FINAL STATE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$ITERATE] ;
I$NDSOLVE$ITERATE::usage = "
I$NDSOLVE$ITERATE[OBJECT,LENGTH,INITIAL] -- iterate NDSolve[] state <OBJECT> up to length <LENGTH> (real) (should be less then total length) with initial condinions <INITIAL> and return final state
" ;
I$NDSOLVE$ITERATE[                (* -- STATE ITERATOR (LAST STEP OUTPUT) *)
  OBJECT_,                        (* -- STATE TO ITERATE *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  INITIAL_List                    (* -- INITIAL CONDITION (STATE) *)
] := Catch[
  Block[
    {FLAG,LIST},
    FLAG = First[INITIAL]  ;
    LIST = Drop[INITIAL,1] ;
    If[Less[FLAG,0.5],Throw[INITIAL]] ;
    LIST = Flatten[List[1.0,Take[LIST,I$DIMENSION$PARAMETRIC],Through[OBJECT[LIST][LENGTH]]]] ;
    Developer`ToPackedArray[LIST]
  ]
] /; SameQ[Length[INITIAL],I$DIMENSION$VARIABLES] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE STATE ITERATION (OUTPUT AT EACH INTEGRATION STEP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE$ITERATE$LIST] ;
I$NDSOLVE$ITERATE$LIST::usage = "
I$NDSOLVE$ITERATE$LIST[OBJECT,LENGTH,INITIAL] -- iterate NDSolve[] state <OBJECT> up to length <LENGTH> (real) with initial condinions <INITIAL> (list) and return state at each integration step
" ;
I$NDSOLVE$ITERATE$LIST[           (* -- STATE ITERATOR (OUTPUT AT EACH INTEGRATION STEP) *)
  OBJECT_,                        (* -- STATE TO ITERATE *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  INITIAL_List                    (* -- INITIAL CONDITION *)
] := Catch[
  Block[
    {FLAG,LIST,TIME,NUMBER,VARIABLE,PARAMETER},
    FLAG = First[INITIAL] ;
    If[Less[FLAG,0.5],Throw[INITIAL]] ;
    LIST = Drop[INITIAL,1] ;
    LIST = OBJECT[LIST] ;
    TIME = Flatten[First[LIST]["Grid"]] ;
    NUMBER = Length[TIME] ;
    VARIABLE = Transpose[Through[LIST["ValuesOnGrid"]]] ;
    PARAMETER = Map[Function[ConstantArray[Slot[1],NUMBER]],Take[Drop[INITIAL,1],I$DIMENSION$PARAMETRIC]] ;
    PARAMETER = If[UnsameQ[PARAMETER,List[]],Transpose[PARAMETER],Nothing] ;
    FLAG = Transpose[List[ConstantArray[FLAG,NUMBER]]] ;
    LIST = Map[Flatten,Transpose[List[TIME,FLAG,PARAMETER,VARIABLE]],1] ;
    List[Rest[Last[LIST]],LIST]
  ]
] /; SameQ[Length[INITIAL],I$DIMENSION$VARIABLES] ;
(* ################################################################################################################################################################ *)
(* NDSOLVE INTEGRATOR (RETURNS FUNCTION THAT ACTS ON STATE) *)
(* ################################################################################################################################################################ *)
(* NOTE: ALL PARAMETRIC FLAGS ARE SET TO 1, I.E. FLAG_I \[Rule] 1 *)
(* NOTE: EQUATIONS CAN BE SOLVED INDEPENDENTLY (NOT IMPLEMENTED) *)
(* ################################################################################################################################################################ *)
ClearAll[I$NDSOLVE] ;
I$NDSOLVE::usage = "
I$NDSOLVE[HAMILTONIAN,STEP,LENGTH,METHOD] -- generate NDSolve[] element for given Hamiltonian <HAMILTONIAN>, number of integration steps <STEP> (integer), integration length <LENGTH> (real) and integration method <METHOD>
I$NDSOLVE[VECTOR,STEP,LENGTH,METHOD] -- generate NDSolve[] element for given vector field <VECTOR> (canonical or total), number of integration steps <STEP> (integer), integration length <LENGTH> (real) and integration method <METHOD>
I$NDSOLVE[HAMILTONIAN,STEP,LENGTH,METHOD,ITERATOR] -- generate NDSolve[] element for given Hamiltonian <HAMILTONIAN>, number of integration steps <STEP> (integer), integration length <LENGTH> (real), integration method <METHOD> and iterator function <ITERATOR> (I$NDSOLVE$ITERATE OR I$NDSOLVE$ITERATE$LIST)
I$NDSOLVE[VECTOR,STEP,LENGTH,METHOD,ITRERATOR] -- generate NDSolve[] element for given vector field <VECTOR> (canonical or total), number of integration steps <STEP> (integer), integration length <LENGTH> (real), integration method <METHOD> and iterator function <ITERATOR> (I$NDSOLVE$ITERATE OR I$NDSOLVE$ITERATE$LIST)
" ;
I$NDSOLVE[                        (* -- GENERATE NDSOLVE BASED MAP (ACTS ON INITIAL CONDITION) *)
  HAMILTONIAN_,                   (* -- HAMILTONIAN FUNCTION *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_,                        (* -- INTEGRATION METHO *)
  ITERATOR_:I$NDSOLVE$ITERATE     (* -- ITERATOR FUNCTION (I$NDSOLVE$ITERATE OR I$NDSOLVE$ITERATE$LIST) *)
] := Block[
  {STATE},
  STATE = I$NDSOLVE$STATE$SCALAR[HAMILTONIAN,STEP,LENGTH,METHOD] ;
  ReleaseHold[Hold[Function][Hold[ITERATOR][STATE,LENGTH,Slot[1]]]]
] ;
I$NDSOLVE[                        (* -- GENERATE NDSOLVE BASED MAP (ACTS ON INITIAL CONDITION) *)
  VECTOR_List,                    (* -- VECTOR FIELD OR TOTAL VECTOR FIELD *)
  STEP_Integer,                   (* -- NUMBER OF INTEGRATION STEPS (INTEGER) *)
  LENGTH_Real,                    (* -- INTEGRATION LENGTH (REAL) *)
  METHOD_,                        (* -- INTEGRATION METHO *)
  ITERATOR_:I$NDSOLVE$ITERATE     (* -- ITERATOR FUNCTION (I$NDSOLVE$ITERATE OR I$NDSOLVE$ITERATE$LIST) *)
] := Block[
  {STATE},
  STATE = If[
    SameQ[Length[VECTOR],I$DIMENSION$CANONICAL],
    I$NDSOLVE$STATE$VECTOR[VECTOR,STEP,LENGTH,METHOD],
    I$NDSOLVE$STATE[VECTOR,STEP,LENGTH,METHOD]
  ] ;
  ReleaseHold[Hold[Function][Hold[ITERATOR][STATE,LENGTH,Slot[1]]]]
] ;
(* ################################################################################################################################################################ *)
(* PARAMETRIC CLOSED ORBIT (NUMERICAL ROOT FINDING) *)
(* ################################################################################################################################################################ *)
(* NOTE: <MASK> = {...,1,...,0,...} WITH 1(0) INDICATING AN OSCILLATING (DRIFTING) PLANE(S), E.G. <PLANE> = {1,0} <=> {{Q1,P1},{Q2,P2}}, {Q1,P1} IS OSCILLATING AND {Q2,P2} IS DRIFTING *)
(* ################################################################################################################################################################ *)
ClearAll[I$ORBIT] ;
I$ORBIT::usage = "
I$ORBIT[ORDER,MAP] -- compute parametric closed orbit for given one turn map <MAP> (state map) up to order <ORDER> (integer)
I$ORBIT[ORDER,MAP,MASK] -- compute parametric closed orbit for given one turn map <MAP> (state map) up to order <ORDER> (integer) with drift planes <MASK> (list of integers)
" ;
Options[I$ORBIT] = List[
  Rule["MAX",100],                (* -- MAXIMUM NUMBER OF ITERATIONS (INTEGER) *)
  Rule["METHOD","Newton"],        (* -- SOLVE METHOD *)
  Rule["VERBOSE",False]           (* -- VERBOSE FLAG *)
] ;
I$ORBIT[                          (* -- COMPUTE PARAMETRIC CLOSED ORBIT *)
  ORDER_Integer,                  (* -- ORDER (INTEGER) *)
  MAP_,                           (* -- STATE MAP (FUNCTION) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {COEFFICIENT,RESULT,ZERO,INITIAL,POSITION,MAX,ALL,LOOP,FIX,REPLACE,VALUE,INDEX,SOLUTION},
  SetAttributes[COEFFICIENT,NHoldAll] ;
  (* GENERATE GENERIC CLOSED ORBIT COEFFICIENTS *)
  RESULT = Flatten[List[1.0,Array[COEFFICIENT,Subtract[I$DIMENSION$VARIABLES,1]]]] ;
  (* CONVERT COEFFICIENT REPRESENTATION TO SERIES REPRESENTATION *)
  RESULT = I$MAKE$COMPONENTS[RESULT] ;
  (* ZERO ORDER COEFFICIENTS *)
  ZERO = Chop[ReplaceAll[RESULT,Thread[Rule[I$VARIABLES,0]]]] ;
  (* PARAMETRIC PART OF CLOSED ORBIT (REMOVE CANONICAL VARIABLES) *)
  RESULT = ReplaceAll[RESULT,Thread[Rule[I$VARIABLES$CANONICAL,0]]] ;
  (* GENERATE COEFFICIENT REPRESENTATION OF PARAMETRIC PART (NONPARAMETRIC COEFFICIENTS ARE REPLACED BY ZEROS) *)
  RESULT = I$MAKE$STATE[ConstantArray[0,I$DIMENSION$PARAMETRIC],RESULT] ;
  RESULT = Rest[RESULT] ;
  RESULT = Drop[RESULT,I$DIMENSION$PARAMETRIC] ;
  RESULT = Drop[RESULT,I$DIMENSION$CANONICAL] ;
  RESULT = Rationalize[RESULT] ;
  (* GROUP COEFFICIENTS BY ORDER AND ADD ZERO ORDER COEFFICIENTS *)
  RESULT = Values[GroupBy[Transpose[List[I$COMPONENTS,RESULT]],Composition[Total,Rest,First]]] ;
  RESULT = Map[Composition[Last,Transpose],RESULT] ;
  RESULT = Join[List[ZERO],RESULT] ;
  (* GENERATE (ZERO) INITIAL CONDITIONS FOR COEFFICIENTS (ZEROS) *)
  INITIAL = Map[Variables,RESULT] ;
  INITIAL = Map[Function[Transpose[List[Slot[1],ConstantArray[N[0],Length[Slot[1]]]]]],INITIAL] ;
  (* ADD PARAMETERS TO COEFFICIENTS *)
  RESULT = Flatten[List[1,ConstantArray[0,I$DIMENSION$PARAMETRIC],RESULT]] ;
  (* SET POSITIONS OF GENERIC COEFFICIENTS GROUPED BY ORDER STARTING FROM ZERO ORDER *)
  POSITION = Map[Composition[Flatten,Function[Map[Function[Position[RESULT,Slot[1]]],Slot[1]]],Variables],INITIAL] ;
  (* SET MAXIMUM COMPUTATION ORDER *)
  MAX = Min[List[Subtract[Length[DeleteCases[INITIAL,List[]]],1],ORDER]] ;
  (* SOLUTION HOLDER *)
  ALL = List[] ;
  (* ORDER-BY-ORDER SOLUTION *)
  Do[
    List[
      If[OptionValue["VERBOSE"],Print[LOOP]] ;
      FIX[LOOP] = RESULT ;
      REPLACE[LOOP] = Flatten[Delete[POSITION,1]] ;
      FIX[LOOP] = ReplacePart[FIX[LOOP],Thread[Rule[REPLACE[LOOP],0]]] ;
      VALUE[LOOP] = Part[INITIAL,1] ;
      INDEX[LOOP] = Part[POSITION,1] ;
      SOLUTION[LOOP] = FindRoot[
        Equal[Part[FIX[LOOP],INDEX[LOOP]],Part[MAP[FIX[LOOP]],INDEX[LOOP]]],
        VALUE[LOOP],
        Rule[Evaluated,False],
        Rule[MaxIterations,OptionValue["MAX"]],
        Rule[Method,OptionValue["METHOD"]]
      ] // Quiet ;
      ALL = Flatten[List[ALL,SOLUTION[LOOP]]] ;
      RESULT = ReplaceAll[RESULT,ALL] ;
      POSITION = Rest[POSITION] ;
      INITIAL = Rest[INITIAL] ;
      If[OptionValue["VERBOSE"],Print[RESULT]] ;
    ],
    List[LOOP,0,MAX]
  ] ;
  RESULT = ReplaceAll[RESULT,Rule[COEFFICIENT[_],0]] ;
  N[I$MAKE$COMPONENTS[RESULT]]
] ;
I$ORBIT[                          (* -- COMPUTE PARAMETRIC CLOSED ORBIT *)
  ORDER_Integer,                  (* -- ORDER (INTEGER) *)
  MAP_,                           (* -- STATE MAP (FUNCTION) *)
  MASK_List,                      (* -- LIST OF OSCILLATING PLANES (LIST OF INTEGERS) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {COEFFICIENT,RESULT,ZERO,REMOVE,KEEP,SELECT,INITIAL,POSITION,MAX,ALL,FIX,REPLACE,SOLUTION,VALUE,INDEX,LOOP},
  SetAttributes[COEFFICIENT,NHoldAll] ;
  (* GENERATE GENERIC CLOSED ORBIT COEFFICIENTS *)
  RESULT = Flatten[List[1.0,Array[COEFFICIENT,Subtract[I$DIMENSION$VARIABLES,1]]]] ;
  (* CONVERT COEFFICIENT REPRESENTATION TO SERIES REPRESENTATION *)
  RESULT = I$MAKE$COMPONENTS[RESULT] ;
  (* REMOVE OSCILLATING PLANES *)
  REMOVE = I$VARIABLES$CANONICAL ;
  REMOVE = If[SameQ[I$ORDERING,"PAIRS"],Partition[REMOVE,Plus[1,1]],Transpose[Partition[REMOVE,I$DIMENSION]]] ;
  KEEP = Flatten[Pick[REMOVE,MASK,0]] ;
  REMOVE = Flatten[List[Pick[REMOVE,MASK,1],Complement[KEEP,I$VARIABLES$CANONICAL$Q]]] ;
  KEEP = DeleteCases[KEEP,Apply[Alternatives,I$VARIABLES$CANONICAL$P]] ;
  RESULT = ReplaceAll[RESULT,Thread[Rule[REMOVE,0]]] ;
  (* ADD INITIAL CONDITION FOR DRIFT PLANES *)
  SELECT = Transpose[ConstantArray[MASK,Plus[1,1]]] ;
  SELECT = If[SameQ[I$ORDERING,"PAIRS"],Flatten[SELECT],Flatten[Transpose[SELECT]]] ;
  RESULT = Pick[RESULT,SELECT,1] ;
  SELECT = Pick[I$VARIABLES$CANONICAL,SELECT,0] ;
  SELECT = ReplaceAll[SELECT,Thread[Rule[REMOVE,0]]] ;
  RESULT = Flatten[List[RESULT,SELECT]] ;
  (* ZERO ORDER COEFFICIENTS *)
  ZERO = Chop[ReplaceAll[RESULT,Thread[Rule[I$VARIABLES,0]]]] ;
  (* GENERATE COEFFICIENT REPRESENTATION  *)
  RESULT = Rationalize[I$MAKE$STATE[ConstantArray[0,I$DIMENSION$PARAMETRIC],RESULT]] ;
  RESULT = Rest[Rationalize[RESULT]] ;
  RESULT = Drop[RESULT,I$DIMENSION$PARAMETRIC] ;
  RESULT = Drop[RESULT,I$DIMENSION$CANONICAL] ;
  (* GROUP COEFFICIENTS BY ORDER AND ADD ZERO ORDER COEFFICIENTS *)
  RESULT = Values[GroupBy[Transpose[List[I$COMPONENTS,RESULT]],Composition[Total,Rest,First]]] ;
  RESULT = Map[Composition[Last,Transpose],RESULT] ;
  RESULT = Join[List[ZERO],RESULT] ;
  (* GENERATE (ZERO) INITIAL CONDITIONS FOR COEFFICIENTS (ZEROS) *)
  INITIAL = Map[Variables,RESULT] ;
  INITIAL = Map[Function[Transpose[List[Slot[1],ConstantArray[N[0],Length[Slot[1]]]]]],INITIAL] ;
  (* ADD PARAMETERS TO COEFFICIENTS *)
  RESULT = Flatten[List[1,ConstantArray[0,I$DIMENSION$PARAMETRIC],RESULT]] ;
  (* SET POSITIONS OF GENERIC COEFFICIENTS GROUPED BY ORDER STARTING FROM ZERO ORDER *)
  POSITION = Map[Composition[Flatten,Function[Map[Function[Position[RESULT,Slot[1]]],Slot[1]]],Variables],INITIAL] ;
  (* SET MAXIMUM COMPUTATION ORDER *)
  MAX = Min[List[Subtract[Length[DeleteCases[INITIAL,List[]]],1],ORDER]] ;
  (* SOLUTION HOLDER *)
  ALL = List[] ;
  (* PERFORM ORDER-BY-ORDER SOLUTION *)
  Do[
    List[
      If[OptionValue["VERBOSE"],Print[LOOP]] ;
      FIX[LOOP] = RESULT ;
      REPLACE[LOOP] = Flatten[Delete[POSITION,1]] ;
      FIX[LOOP] = ReplacePart[FIX[LOOP],Thread[Rule[REPLACE[LOOP],0]]] ;
      VALUE[LOOP] = Part[INITIAL,1] ;
      INDEX[LOOP] = Part[POSITION,1] ;
      SOLUTION[LOOP] = FindRoot[
        Equal[Part[FIX[LOOP],INDEX[LOOP]],Part[MAP[FIX[LOOP]],INDEX[LOOP]]],
        VALUE[LOOP],
        Rule[Evaluated,False],
        Rule[MaxIterations,OptionValue["MAX"]],
        Rule[Method,OptionValue["METHOD"]]
      ] // Quiet ;
      ALL = Flatten[List[ALL,SOLUTION[LOOP]]] ;
      RESULT = ReplaceAll[RESULT,ALL] ;
      POSITION = Rest[POSITION] ;
      INITIAL = Rest[INITIAL] ;
      If[OptionValue["VERBOSE"],Print[RESULT]] ;
    ],
    List[LOOP,0,MAX]
  ] ;
  RESULT = ReplaceAll[RESULT,Rule[COEFFICIENT[_],0]] ;
  N[I$MAKE$COMPONENTS[RESULT]]
] /; SameQ[Length[MASK],I$DIMENSION] ;
(* ################################################################################################################################################################ *)
(* PARAMETRIC MATRIX COMPUTATION (PARAMETRIC MATRIX AROUND CLOSED ORBIT) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MATRIX] ;
I$MATRIX::usage = "
I$MATRIX[ORBIT,MAP] -- compute parametric transport matrix for given closed orbit <ORBIT> (series) and one turn map <MAP> (state map)
I$MATRIX[ORBIT,MAP,MASK] -- compute parametric transport matrix for given closed orbit <ORBIT> (series), one turn map <MAP> (state map) and list of drifting coordinates <MASK> (list of integers)
"  ;
I$MATRIX[                         (* -- COMPUTE PARAMETRIC MATRIX *)
  ORBIT_List,                     (* -- CLOSED ORBIT SOLUTION (PARAMETRIC SERIES FOR CANONICAL COMPONENTS) *)
  MAP_                            (* -- ONE TURN MAP *)
] := Block[
  {STATE,MATRIX},
  STATE = I$MAKE$STATE[ConstantArray[0,I$DIMENSION$PARAMETRIC],Plus[ORBIT,I$VARIABLES$CANONICAL]] ;
  MATRIX = MAP[STATE] ;
  MATRIX = I$MAKE$COMPONENTS[MATRIX] ;
  MATRIX = D[MATRIX,List[I$VARIABLES$CANONICAL]] ;
  N[ReplaceAll[MATRIX,Thread[Rule[I$VARIABLES$CANONICAL,0]]]]
] ;
I$MATRIX[                         (* -- COMPUTE PARAMETRIC MATRIX *)
  ORBIT_List,                     (* -- CLOSED ORBIT SOLUTION *)
  MAP_,                           (* -- ONE TURN MAP *)
  MASK_List                       (* -- LIST OF DRIFT COORDINATES (LIST OF SYMBOLS) *)
] := Block[
  {VARIABLE,STATE,MATRIX,KEEP,REMOVE},
  VARIABLE = I$VARIABLES$CANONICAL ;
  VARIABLE = ReplaceAll[VARIABLE,Thread[Rule[Pick[I$VARIABLES$CANONICAL$Q,MASK,0],0]]] ;
  STATE = I$MAKE$STATE[ConstantArray[0,I$DIMENSION$PARAMETRIC],Plus[ORBIT,VARIABLE]] ;
  MATRIX = MAP[STATE] ;
  MATRIX = I$MAKE$COMPONENTS[MATRIX] ;
  MATRIX = D[MATRIX,List[I$VARIABLES$CANONICAL]] ;
  KEEP = DeleteCases[Flatten[Pick[Array[I$PLANE,I$DIMENSION],MASK,0]],Apply[Alternatives,I$VARIABLES$CANONICAL$P]] ;
  REMOVE = DeleteCases[I$VARIABLES$CANONICAL,Apply[Alternatives,KEEP]] ;
  N[ReplaceAll[MATRIX,Thread[Rule[REMOVE,0]]]]
] /; SameQ[Length[MASK],I$DIMENSION] ;
(* ################################################################################################################################################################ *)
(* PERIODIC ORBIT SEARCH (NEWTON) *)
(* ################################################################################################################################################################ *)
ClearAll[I$PERIODIC$ORBIT] ;
I$PERIODIC$ORBIT::usage = "
I$PERIODIC$ORBIT[MAP,PERIOD,NUMBER,REGION,KNOB,DERIVATIVE] -- perform search of orbit with period <PERIOD> (integer) for map <MAP> (state map) with Newton method for given number of random initial conditions <NUMBER> (integer) inside given region of canonical variables <REGION> (region), list of parameters <KNOB> and list of derivatives <DERIVATIVE> if any
" ;
I$PERIODIC$ORBIT[                 (* -- PERIODIC ORBIT SEARCH (NEWTON) *)
  MAP_,                           (* -- MAP (FUNCTION) *)
  PERIOD_Integer,                 (* -- ORBIT PERIOD (INTEGER) *)
  NUMBER_Integer,                 (* -- NUMBER OF RANDOM INITIALS (INTEGER) *)
  REGION_?RegionQ,                (* -- SEARCH REGION (REGION) *)
  KNOB_List,                      (* -- LIST OF PARAMETERS (LIST OF REALS) *)
  DERIVATIVE_List                 (* -- LIST OF DERIVATIVES (LIST OF REALS) *)
] := Block[
  {ERROR,POINT,ITERATOR,ARGUMENT,VARIABLE,DATA},
  (* ERROR *)
  ERROR = Power[N[10],Subtract[0,10]] ;
  (* GENERATE RANDOM INITIAL POINTS *)
  POINT = RandomPoint[REGION,NUMBER] ;
  (* ADD APERTURE FLAG *)
  POINT = Map[Flatten,Transpose[List[ConstantArray[N[1],NUMBER],POINT]]] ;
  (* MAPPING FUCTION *)
  ITERATOR = Composition[
    Function[Flatten[List[First[Slot[1]],Take[Drop[Slot[1],Plus[1,I$DIMENSION$PARAMETRIC]],I$DIMENSION$CANONICAL]]]],
    Function[Nest[MAP,Slot[1],PERIOD]],
    Function[Flatten[List[First[Slot[1]],KNOB,Rest[Slot[1]],DERIVATIVE]]]
  ] ;
  (* DUMMY ARGUMENT *)
  ARGUMENT = Array[VARIABLE,Length[First[POINT]]] ;
  (* PERFORM SEARCH *)
  DATA = Map[
    Function[
        ReplaceAll[
        ARGUMENT,
        Quiet[
          Check[
            FindRoot[
              Equal[ARGUMENT,ITERATOR[ARGUMENT]],
              Transpose[List[ARGUMENT,Slot[1]]],
              Rule[MaxIterations,200],
              Rule[Compiled,True],
              Rule[Method,List["Newton",Rule["StepControl",None]]],
              Rule[Jacobian,"FiniteDifference"],
              Rule[Evaluated,False]
            ],
            List[],
            List[FindRoot::lstol,FindRoot::jsing,FindRoot::cvmit]
          ],
          List[FindRoot::lstol,FindRoot::jsing,FindRoot::cvmit]
        ]
      ]
    ],
    POINT
  ] ;
  (* REMOVE POINTS *)
  DATA = DeleteCases[DATA,ARGUMENT] ;
  DATA = DeleteCases[DATA,List[0.,__]] ;
  DATA = N[Chop[DATA,ERROR]] ;
  (* REMOVE CLOSE POINTS *)
  DATA = DeleteDuplicates[N[Chop[DATA,ERROR]],Function[Less[Norm[Plus[Rest[Slot[1]],Times[-1,Rest[Slot[2]]]]],ERROR]]] ;
  (* SELECT REGION MEMBER POINTS *)
  DATA = Select[DATA,Composition[RegionMember[REGION],Rest]] ;
  (* MAPPING FUCTION *)
  ITERATOR = Composition[
    Function[Flatten[List[First[Slot[1]],Take[Drop[Slot[1],Plus[1,I$DIMENSION$PARAMETRIC]],I$DIMENSION$CANONICAL]]]],
    MAP,
    Function[Flatten[List[First[Slot[1]],KNOB,Rest[Slot[1]],DERIVATIVE]]]
  ] ;
  (* PRIME PERIOD *)
  POINT = DATA ;
  DATA = Map[Function[NestList[ITERATOR,Slot[1],PERIOD]],POINT] ;
  DATA = Select[DATA,Function[SameQ[Plus[1,1],Count[Chop[Subtract[Slot[1],ConstantArray[First[Slot[1]],Length[Slot[1]]]],ERROR],ConstantArray[0,Length[ARGUMENT]]]]]] ;
  DATA = Map[SortBy[Norm],DATA] ;
  DATA = SortBy[Transpose[List[Map[Composition[Norm,First],DATA],DATA]],First] ;
  DATA = DeleteDuplicates[DATA,Function[Equal[First[Slot[1]],First[Slot[2]]]]] ;
  If[
    SameQ[DATA,List[]],
    DATA,
    DATA = Map[First,Last[Transpose[DATA]]] ;
    DATA = Map[Function[Flatten[List[First[Slot[1]],KNOB,Rest[Slot[1]],DERIVATIVE]]],DATA] ;
    Map[Function[NestList[MAP,Slot[1],PERIOD]],DATA]
  ]
] /; And[
  GreaterEqual[PERIOD,1],
  GreaterEqual[NUMBER,1]
] ;
(* ################################################################################################################################################################ *)
(* TABLE(S), DEFINITIONS OF MAPS, SEQUENCIES AND ELEMENTS *)
(* ################################################################################################################################################################ *)
I$TABLE["MAP"]      = If[UnsameQ[Head[I$TABLE["MAP"]],Association],Association[],I$TABLE["MAP"]] ;           (* -- (ASSOCIATION) DEFINITIONS OF MAPS       *)
I$TABLE["SEQUENCE"] = If[UnsameQ[Head[I$TABLE["SEQUENCE"]],Association],Association[],I$TABLE["SEQUENCE"]] ; (* -- (ASSOCIATION) DEFINITIONS OF SEQUENCIES *)
I$TABLE["ELEMENT"]  = If[UnsameQ[Head[I$TABLE["ELEMENT"]],Association],Association[],I$TABLE["ELEMENT"]] ;   (* -- (ASSOCIATION) DEFINITIONS OF ELEMENTS   *)
(* ################################################################################################################################################################ *)
(* TABLE (CLEAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$CLEAN] ;
I$TABLE$CLEAN::usage = "
I$TABLE$CLEAN[TABLE] -- clean table <TABLE> (string), \"MAP\", \"SEQUENCE\" OR \"ELEMENT\"
" ;
I$TABLE$CLEAN[                    (* -- CLEAR SELECTED TABLE *)
  TABLE_String                    (* -- TABLE (STRING) *)
] := CompoundExpression[Set[I$TABLE[TABLE],Association[]],Null] /; MemberQ[List["MAP","SEQUENCE","ELEMENT"],TABLE] ;
(* ################################################################################################################################################################ *)
(* TABLE (APPEND) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$APPEND] ;
I$TABLE$APPEND::usage = "
I$TABLE$APPEND[TABLE,RULE] -- append rule <RULE> (rule) to table <TABLE> (string), replaces if exists
" ;
I$TABLE$APPEND[                   (* -- APPEND RULE TO SELECTED TABLE *)
  TABLE_String,                   (* -- TABLE (STRING) *)
  DATA_Rule                       (* -- DATA (RULE) *)
] := Catch[
  If[
    SameQ[I$TABLE[TABLE],Association[]],
    Set[I$TABLE[TABLE],Association[DATA]],
    Set[I$TABLE[TABLE],Merge[List[I$TABLE[TABLE],Association[DATA]],Last]]
  ] ;
] ;
(* ################################################################################################################################################################ *)
(* TABLE (DELETE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$DELETE] ;
I$TABLE$DELETE::usage = "
I$TABLE$DELETE[TABLE,KEY] -- delete key <KEY> (string) from table <TABLE> (string)
" ;
I$TABLE$DELETE[                   (* -- DELETE DATA FROM SELECTED TABLE *)
  TABLE_String,                   (* -- TABLE (STRING) *)
  KEY_String                      (* -- KEY TO DELETE (STRING) *)
] := Catch[
  Set[I$TABLE[TABLE],KeyDrop[I$TABLE[TABLE],KEY]] ;
] ;
(* ################################################################################################################################################################ *)
(* TABLE (RENAME) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$RENAME] ;
I$TABLE$RENAME::usage = "
I$TABLE$RENAME[TABLE,OLD,NEW] -- rename key <OLD> (string) by key <NEW> (string) in table <TABLE> (string)
" ;
I$TABLE$RENAME[                   (* -- RENAME DATA IN SELECTED TABLE *)
  TABLE_String,                   (* -- TABLE (STRING) *)
  OLD_String,                     (* -- OLD NAME (STRING) *)
  NEW_String                      (* -- NEW NAME (STRING) *)
] := Catch[
  Set[I$TABLE[TABLE],KeyMap[ReplaceAll[Rule[OLD,NEW]],I$TABLE[TABLE]]] ;
] ;
(* ################################################################################################################################################################ *)
(* TABLE (SAVE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$SAVE] ;
I$TABLE$SAVE::usage = "
I$TABLE$SAVE[TABLE] -- save table <TABLE> (string), overwrites existing file
" ;
I$TABLE$SAVE[                     (* -- SAVE SELECTED TABLE *)
  TABLE_String                    (* -- TABLE (STRING) *)
] := Catch[
  Export[StringJoin[I$DIRECTORY,StringTemplate["``.MX"][TABLE]],I$TABLE[TABLE]] ;
] ;
(* ################################################################################################################################################################ *)
(* TABLE (LOAD) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$LOAD] ;
I$TABLE$LOAD::usage = "
I$TABLE$LOAD[TABLE] -- load table <TABLE> (string), overwrites existing definition
" ;
I$TABLE$LOAD[                     (* -- LOAD SELECTED TABLE *)
  TABLE_String                    (* -- TABLE (STRING) *)
] := Catch[
  Set[I$TABLE[TABLE],Import[StringJoin[I$DIRECTORY,StringTemplate["``.MX"][TABLE]]]] ;
] ;
(* ################################################################################################################################################################ *)
(* TABLE (INFO) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TABLE$INFO] ;
I$TABLE$INFO::usage = "
I$TABLE$INFO[TABLE] -- print table <TABLE> (string) info (undefined)
I$TABLE$INFO[TABLE,KEY] -- print <KEY> (string) info from table <TABLE> (undefined)
" ;
I$TABLE$INFO[                     (* -- PRINT INFO FOR GIVEN TABLE *)
  TABLE_String                    (* -- TABLE (STRING) *)
] := Catch[
    Undefined
] ;
I$TABLE$INFO[                     (* -- PRINT INFO FOR GIVEN TABLE AND KEY *)
  TABLE_String,                   (* -- TABLE (STRING) *)
  KEY_String                      (* -- TABLE (STRING) *)
] := Catch[
  Undefined
] ;
ClearAll[I$MAKE$DOUBLE$STEP] ;
I$MAKE$DOUBLE$STEP::usage = "
I$MAKE$DOUBLE$STEP[HAMILTONIAN] -- generate maps for double extended phase space integration for given normal phase space hamiltonian <HAMILTONIAN>
" ;
(* ################################################################################################################################################################ *)
(* MAKE (DOUBLE STEP) *)
(* ################################################################################################################################################################ *)
I$MAKE$DOUBLE$STEP[               (* -- GENERATE MAPS FOR DOUBLE EXTENDED PHASE SPACE INTEGRATION *)
  HAMILTONIAN_                    (* -- HAMILTONIAN FUNCTION (EXPRESSION) *)
] := Block[
  {Q$LIST,P$LIST,X$LIST,Y$LIST,FUNCTION,FIELD,MAP$A,MAP$B},
  Q$LIST = Take[I$VARIABLES$CANONICAL$Q,+Divide[I$DIMENSION,Plus[1,1]]] ;
  P$LIST = Take[I$VARIABLES$CANONICAL$P,+Divide[I$DIMENSION,Plus[1,1]]] ;
  X$LIST = Take[I$VARIABLES$CANONICAL$Q,-Divide[I$DIMENSION,Plus[1,1]]] ;
  Y$LIST = Take[I$VARIABLES$CANONICAL$P,-Divide[I$DIMENSION,Plus[1,1]]] ;
  FUNCTION = Apply[Function,List[Flatten[List[Q$LIST,P$LIST]],HAMILTONIAN]] ;
  FIELD = I$VECTOR$FIELD[Apply[FUNCTION,Flatten[List[Q$LIST,Y$LIST]]]] ;
  MAP$A = Plus[I$VARIABLES$CANONICAL,Times[I$PARAMETER,FIELD]] ;
  FIELD = I$VECTOR$FIELD[Apply[FUNCTION,Flatten[List[X$LIST,P$LIST]]]] ;
  MAP$B = Plus[I$VARIABLES$CANONICAL,Times[I$PARAMETER,FIELD]] ;
  List[MAP$A,MAP$B]
] ;
(* ################################################################################################################################################################ *)
(* MAKE (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$MAP] ;
I$MAKE$MAP::usage = "
I$MAKE$MAP[NAME,LOCAL,MAP] -- generate state map with name <NAME> (string), list of local constants <LOCAL> (list of symbols or empty list) and list of state or canonical maps <MAP> (list)
I$MAKE$MAP[NAME,LOCAL,HAMILTONIAN] -- generate state map with name <NAME> (string), list of local constants <LOCAL> (list of symbols or empty list) and list of hamiltonian functions <HAMILTONIAN> (list) to be solved by DSolve[]
I$MAKE$MAP[NAME,LOCAL,HAMILTONIAN,DEGREE] -- generate state map with name <NAME> (string), list of local constants <LOCAL> (list of symbols or empty list), list of (autonomous) hamiltonial function <HAMILTONIAN> (list) and computation degree <DEGREE> (integer)
" ;
I$MAKE$MAP[                       (* -- MAKE STATE MAP *)
  NAME_String,                    (* -- MAP NAME (STRING) *)
  LOCAL_List,                     (* -- LOCAL CONSTANTS (LIST OF SYMBOLS OR EMPTY LIST) *)
  DATA_List,                      (* -- LIST OF STATE MAPS, CANONICAL MAPS OR HAMILTONIAN FUNCTIONS *)
  DEGREE_:Null                    (* -- COMPUTATION DEGREE (FOR TAYLOR MAPS GENARATION FROM HAMILTONIAN) *)
] := Catch[
  Block[
    {LENGTH,STATE,METHOD},
    LENGTH = Length[DATA] ;
    STATE = Map[
      Composition[
        Function[Apply[Function,List[Flatten[List[I$PARAMETER,LOCAL]],Slot[1]]]],
        Function[
          Which[
            UnsameQ[Head[Slot[1]],List],
            Block[
              {HAMILTONIAN},
              HAMILTONIAN = Slot[1] ;
              If[
                SameQ[DEGREE,Null],
                Set[METHOD,"DSOLVE"] ;
                I$MAP$DERIVATIVE[First[I$DSOLVE[List[HAMILTONIAN]]]],
                Set[METHOD,"TAYLOR"] ;
                I$MAP$DERIVATIVE[
                  Map[
                    Function[Dot[Divide[Power[I$PARAMETER,Range[0,DEGREE]],Factorial[Range[0,DEGREE]]],NestList[Function[I$POISSON$BRACKET[Slot[1],HAMILTONIAN]],Slot[1],DEGREE]]],
                    I$VARIABLES$CANONICAL
                  ]
                ]
              ]
            ],
            SameQ[Length[Slot[1]],I$DIMENSION$CANONICAL],
            Set[METHOD,"DIRECT"] ;
            I$MAP$DERIVATIVE[Slot[1]],
            SameQ[Length[Slot[1]],I$DIMENSION$VARIABLES],
            Set[METHOD,"DIRECT"] ;
            Slot[1]
          ]
        ]
      ],
      If[SameQ[DEGREE,All],I$EXPAND[DATA],DATA]
    ] ;
    Rule[
      NAME,
      Association[
        Rule["SIGNATURE",I$SIGNATURE],
        Rule["TYPE","MAP"],
        Rule["METHOD",METHOD],
        Rule["LOCAL",LOCAL],
        Rule["LENGTH_LOCAL",Length[LOCAL]],
        Rule["MAP",STATE],
        Rule["LENGTH_MAP",LENGTH],
        Rule["CODE",Null]
      ]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* YOSHIDA (PRECISION) *)
(* ################################################################################################################################################################ *)
ClearAll[I$YOSHIDA$PRECISION] ;
I$YOSHIDA$PRECISION::usage = "
I$YOSHIDA$PRECISION = <NUMBER> (integer) --  set precision of Yoshida coefficients
" ;
I$YOSHIDA$PRECISION := 100 ;
(* ################################################################################################################################################################ *)
(* YOSHIDA (FACTORS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$YOSHIDA$FACTORS] ;
I$YOSHIDA$FACTORS::usage = "
I$YOSHIDA$FACTORS[ORDER] -- generate list of Yoshida coefficients {{x_1,y_1,x_1},{x_2,y_2,x_2},...} up to order <ORD> (integer greater or equal to one)
" ;
I$YOSHIDA$FACTORS = Composition[Curry[N][I$YOSHIDA$PRECISION],Map[Function[List[(2-2^(1+2*Slot[1])^(-1))^(-1),-(2^(1+2*Slot[1])^(-1)/(2-2^(1+2*Slot[1])^(-1))),(2-2^(1+2*Slot[1])^(-1))^(-1)]]],Range] ;
(* ################################################################################################################################################################ *)
(* YOSHIDA (COEFFICIENTS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$YOSHIDA$COEFFICIENTS] ;
I$YOSHIDA$COEFFICIENTS::usage = "
I$YOSHIDA$COEFFICIENTS[ORDER] -- generate list of Yoshida coefficients (products of Yoshida factors) for given order <ORD> (integer greater or equal to one)
" ;
I$YOSHIDA$COEFFICIENTS = Composition[Fold[Composition[Flatten,Apply[Outer],Function[Reverse[List[SlotSequence[],Times]]]]],I$YOSHIDA$FACTORS] ;
(* ################################################################################################################################################################ *)
(* YOSHIDA (WEIGHTS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$YOSHIDA$WEIGHTS] ;
I$YOSHIDA$WEIGHTS::usage = "
I$YOSHIDA$WEIGHTS[NUMBER,ORDER] -- generate list of Yoshida weights of order <ORDER> (integer greater or equal to zero) for given second order step with <NUMBER> (integer greater or equal to one) maps
" ;
I$YOSHIDA$WEIGHTS[                (* -- GENERATE YOSHIDA WEIGHTS *)
  NUMBER_Integer,                 (* -- # OF MAPS IN SECOND ORDER APPROXIMATION (INTEGER GREATER OR EQUAL TO ONE) *)
  ORDER_Integer                   (* -- (YOSHIDA) ORDER OF APPROXIMATION (INTEGER GREATER OR EQUAL TO ZERO) *)
] := Block[
  {LENGTH,LIST,INDEX,VALUE,COEFFICIENT,WEIGHT},
  LENGTH = Subtract[NUMBER,1] ;
  LIST = Transpose[List[Range[LENGTH],ConstantArray[N[Divide[1,2],I$YOSHIDA$PRECISION],LENGTH]]] ;
  LIST = Join[LIST,List[List[NUMBER,N[1,I$YOSHIDA$PRECISION]]],Reverse[LIST]] ;
  List[INDEX,VALUE] = Transpose[LIST] ;
  COEFFICIENT = If[SameQ[ORDER,0],List[N[1,I$YOSHIDA$PRECISION]],I$YOSHIDA$COEFFICIENTS[ORDER]] ;
  WEIGHT = Flatten[Map[Function[Transpose[List[INDEX,Times[VALUE,Slot[1]]]]],COEFFICIENT],1] ;
  WEIGHT = SplitBy[WEIGHT,First] ;
  WEIGHT = Transpose[List[Map[Composition[First,First,Transpose],WEIGHT],Map[Composition[Total,Last,Transpose],WEIGHT]]] ;
  WEIGHT
] ;
(* ################################################################################################################################################################ *)
(* MAKE (SEQUENCE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$SEQUENCE] ;
I$MAKE$SEQUENCE::usage = "
I$MAKE$SEQUENCE[NAME,METHOD,UNIQUE,SEQUENCE,WEIGHT] -- generate sequence with name <NAME> (string) using method <METHOD> (string), list of unique maps <UNIQUE> (list of strings), sequence <SEQUENCE> (ordered list of maps) and list of weights <WEIGHT> (list of reals)
I$MAKE$SEQUENCE[NAME,METHOD,UNIQUE,SEQUENCE,ORDER] -- generate sequence with name <NAME> (string) using method <METHOD> (string), list of unique maps <UNIQUE> (list of strings), sequence <SEQUENCE> (ordered list of maps) and Yoshida order <ORDER> (integer)
" ;
I$MAKE$SEQUENCE[                  (* -- GENERATE SEQUENCE FOR GIVEN LIST OF GENERATING MAPS *)
  NAME_String,                    (* -- SEQUENCE NAME (STRING) *)
  "DIRECT",                       (* -- DIRECT *)
  UNIQUE_List,                    (* -- LIST OF UNIQUE MAPS (LIST OF STRINGS) *)
  SEQUENCE_List,                  (* -- SEQUENCE (LIST OF STRINGS) *)
  WEIGHT_List                     (* -- LIST OF WEIGHTS FOR EACH NAME IN SEQUENCE LIST (LIST OF REALS) *)
] := Catch[
  Block[
    {DATA},
    DATA = Transpose[List[SEQUENCE,N[WEIGHT]]] ;
    Rule[
      NAME,
      Association[
        Rule["SIGNATURE",I$SIGNATURE],
        Rule["TYPE","SEQUENCE"],
        Rule["METHOD","DIRECT"],
        Rule["UNIQUE",UNIQUE],
        Rule["LENGTH_UNIQUE",Length[UNIQUE]],
        Rule["LOCAL",Through[Map[I$TABLE["MAP"],UNIQUE]["LOCAL"]]],
        Rule["LENGTH_LOCAL",Through[Map[I$TABLE["MAP"],UNIQUE]["LENGTH_LOCAL"]]],
        Rule["RULE_LOCAL",Thread[Rule[UNIQUE,Through[Map[I$TABLE["MAP"],UNIQUE]["LOCAL"]]]]],
        Rule["ORDERING",DATA],
        Rule["LENGTH",Length[DATA]],
        Rule["CODE",Null]
      ]
    ]
  ]
] ;
I$MAKE$SEQUENCE[                  (* -- GENERATE YOSHIDA INTEGRATION STEP *)
  NAME_String,                    (* -- SEQUENCE NAME (STRING) *)
  "YOSHIDA",                      (* -- YOSHIDA *)
  UNIQUE_List,                    (* -- LIST OF UNIQUE MAPS (LIST OF STRINGS) *)
  SEQUENCE_List,                  (* -- YOSHIDA SEQUENCE (LIST OF STRINGS) *)
  ORDER_Integer                   (* -- YOSHIDA ORDER (INTEGER GREATER OR EQUAL TO ZERO) *)
] := Catch[
  Block[
    {DATA},
    DATA = ReplaceAll[I$YOSHIDA$WEIGHTS[Length[SEQUENCE],ORDER],Thread[Rule[Range[Length[SEQUENCE]],SEQUENCE]]]  ;
    Rule[
      NAME,
      Association[
        Rule["SIGNATURE",I$SIGNATURE],
        Rule["TYPE","SEQUENCE"],
        Rule["METHOD","YOSHIDA"],
        Rule["UNIQUE",UNIQUE],
        Rule["LENGTH_UNIQUE",Length[UNIQUE]],
        Rule["LOCAL",Through[Map[I$TABLE["MAP"],UNIQUE]["LOCAL"]]],
        Rule["LENGTH_LOCAL",Through[Map[I$TABLE["MAP"],UNIQUE]["LENGTH_LOCAL"]]],
        Rule["RULE_LOCAL",Thread[Rule[UNIQUE,Through[Map[I$TABLE["MAP"],UNIQUE]["LOCAL"]]]]],
        Rule["ORDERING",DATA],
        Rule["LENGTH",Length[DATA]],
        Rule["CODE",Null]
       ]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* MAKE ELEMENT *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$ELEMENT] ;
I$MAKE$ELEMENT::usage = "
I$MAKE$ELEMENT[NAME,GLOBAL,ORDERING,RELATION] -- make element with name <NAME> (string), list of global constants <GLOBAL> (list), list of sequencies <ORDERING> (list) and local/global relation <RELATION>
" ;
I$MAKE$ELEMENT[                   (* -- MAKE ELEMENT *)
  NAME_String,                    (* -- ELEMENT NAME (STRING) *)
  GLOBAL_List,                    (* -- LIST OF GLOBAL CONSTANTS (LIST OF SYMBOLS) *)
  ORDERING_List,                  (* -- LIST OF SEQUENCIES *)
  RELATION_List                   (* -- LOCAL & GLOBAL RELATION *)
                                  (*    RELATION     = List[...,RELATION_I,...]   -- LOCAL & GLOBAL RELATION *)
                                  (*    RELATION_I   = List[...,RELATION_I_J,...] -- LOCAL & GLOBAL RELATION FOR SEQUENCE_I *)
                                  (*    RELATION_I_J = List[...,LOCAL_I_J_K,...]  -- LOCAL & GLOBAL RELATION FOR EACH UNIQUE MAP_J IN SEQUENCE_I *)
                                  (*    LOCAL_I_J_K                               -- EXPRESSION IN TERMS OF GLOBAL CONSTANTS *)
] := Catch[
  Rule[
    NAME,
    Association[
      Rule["SIGNATURE",I$SIGNATURE],
      Rule["TYPE","ELEMENT"],
      Rule["GLOBAL",GLOBAL],
      Rule["ORDERING",ORDERING],
      Rule["LENGTH",Length[ORDERING]],
      Rule["RELATION",RELATION],
      Rule["LENGTH_LOCAL",Through[Map[I$TABLE["SEQUENCE"],ORDERING]["LENGTH_LOCAL"]]],
      Rule["RULE_LOCAL",Through[Map[I$TABLE["SEQUENCE"],ORDERING]["RULE_LOCAL"]]]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* EVALUATE (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$EVALUATE$MAP] ;
I$EVALUATE$MAP::usage = "
I$EVALUATE$MAP[NAME,FACTOR,LOCAL,FLAG,INITIAL] -- evaluate map with name <NAME> (string), integration parameter value <FACTOR> (real), list of values for local map constants <LOCAL> (list of reals), parametric flag values <FLAG> (list of reals) and initial state <INITIAL> (list of reals)
" ;
I$EVALUATE$MAP[                   (* -- DERECT MAP EVALUATION *)
  NAME_String,                    (* -- MAP NAME (STRING) *)
  FACTOR_Real,                    (* -- TIME (REAL) *)
  LOCAL_List,                     (* -- VALUES FOR LOCAL CONSTANTS (LIST OF REALS) *)
  FLAG_List,                      (* -- PARAMETRIC FLAG VALUES (LIST OF REALS) *)
  INITIAL_List                    (* -- STATE INITIAL (LIST OF REALS) *)
] := Catch[
  Block[
    {RULE,MAP,RESULT,LOOP},
    RULE = Join[
      List[Rule[I$PARAMETER,FACTOR]],
      Thread[Rule[I$TABLE["MAP"][NAME,"LOCAL"],LOCAL]],
      Thread[Rule[I$FLAG$PARAMETRIC,FLAG]]
    ] ;
    RULE = Dispatch[RULE] ;
    MAP = Through[I$TABLE["MAP"][NAME,"MAP"][Apply[Sequence,Flatten[List[I$PARAMETER,I$TABLE["MAP"][NAME,"LOCAL"]]]]]] ;
    MAP = ReplaceAll[MAP,RULE] ;
    MAP = Map[Function[Apply[Function,List[I$VARIABLES,Slot[1]]]],MAP] ;
    RESULT = INITIAL ;
    Do[
     Set[RESULT,Apply[Part[MAP,LOOP],RESULT]],
     List[LOOP,1,Length[MAP]]
     ] ;
     RESULT
  ]
] ;
(* ################################################################################################################################################################ *)
(* EVALUATE (SEQUENCE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$EVALUATE$SEQUENCE] ;
I$EVALUATE$SEQUENCE::usage = "
I$EVALUATE$SEQUENCE[NAME,MAX,LENGTH,LOCAL,FLAG,INITIAL] -- evaluate sequence with name <NAME> (string), maximum integration step length <MAX> (real), total integration length <LENGTH> (real), values for local map constants <LOCAL> (list for each maps), parametric flag values <FLAG> (list of reals) and initial state <INITIAL> (list of reals)
" ;
I$EVALUATE$SEQUENCE[              (* -- DIRECT SEQUENCE EVALUATION *)
  NAME_String,                    (* -- SEQUENCE NAME (STRING) *)
  MAX_,                           (* -- MAXIMUM INTEGRATION STEP SIZE (REAL) *)
  LENGTH_,                        (* -- TOTAL INTEGRATION LENGTH (REAL) *)
  LOCAL_,                         (* -- VALUES FOR LOCAL CONSTANTS FOR EACH UNIQUE MAP (LIST OF LISTS) *)
  FLAG_,                          (* -- PARAMETRIC FLAG VALUES (LIST OF REALS) *)
  INITIAL_                        (* -- STATE INITIAL (LIST OF REALS) *)
] := Catch[
  Block[
    {NUMBER,STEP,SEQUENCE,WEIGHT,RULE,RESULT,LOOP},
    NUMBER = Ceiling[Divide[Abs[LENGTH],MAX]] ;
    STEP = Divide[LENGTH,NUMBER] ;
    List[SEQUENCE,WEIGHT] = Transpose[I$TABLE["SEQUENCE"][NAME,"ORDERING"]] ;
    WEIGHT = Times[WEIGHT,STEP] ;
    RULE = Dispatch[Thread[Rule[I$TABLE["SEQUENCE"][NAME,"UNIQUE"],LOCAL]]] ;
    RULE = ReplaceAll[SEQUENCE,RULE] ;
    SEQUENCE = Transpose[List[SEQUENCE,WEIGHT,RULE]] ;
    RESULT = INITIAL ;
    Do[
      Set[RESULT,Apply[I$EVALUATE$MAP,Join[Part[SEQUENCE,LOOP],List[FLAG,RESULT]]]],
      NUMBER,
      List[LOOP,1,Length[SEQUENCE]]
    ] ;
    RESULT
  ]
] ;
(* ################################################################################################################################################################ *)
(* EVALUATE (ELEMENT) *)
(* ################################################################################################################################################################ *)
ClearAll[I$EVALUATE$ELEMENT] ;
I$EVALUATE$ELEMENT::usage = "
I$EVALUATE$ELEMENT[NAME,MAX,LENGTH,GLOBAL,FLAG,INITIAL] -- evaluate element with name <NAME> (string), maximum integration step length <MAX> (real for each sequence), total integration length <LENGTH> (real for each sequence), values for global constants <GLOBAL>, parametric flag values <FLAG> (list of reals) and initial state <INITIAL> (list of reals)
" ;
I$EVALUATE$ELEMENT[               (* -- DIRECT ELEMENT EVALUATION *)
  NAME_String,                    (* -- ELEMENT NAME (STRING) *)
  MAX_List,                       (* -- MAXIMUM INTEGRATION STEP FOR EACH SEQUENCE (LIST OF REALS) *)
  LENGTH_List,                    (* -- TOTAL INTEGRATION LENGTH FOR EACH SEQUENCE (LIST OF REALS) *)
  GLOBAL_List,                    (* -- VALUES OF GLOBAL CONSTANTS (LIST OF REALS) *)
  FLAG_List,                      (* -- PARAMETRIC FLAG VALUES (LIST OF REALS) *)
  INITIAL_List                    (* -- STATE INITIAL (LIST OF REALS) *)
] := Catch[
  Block[
    {SEQUENCE,RESULT,LOOP},
    SEQUENCE = List[
      I$TABLE["ELEMENT"][NAME,"ORDERING"],
      MAX,
      LENGTH,
      ReplaceAll[I$TABLE["ELEMENT"][NAME,"RELATION"],Dispatch[Thread[Rule[I$TABLE["ELEMENT"][NAME,"GLOBAL"],GLOBAL]]]]
    ] ;
    SEQUENCE = Transpose[SEQUENCE] ;
    RESULT = INITIAL ;
    Do[
      Set[RESULT,Apply[I$EVALUATE$SEQUENCE,Join[Part[SEQUENCE,LOOP],List[FLAG,RESULT]]]],
      List[LOOP,1,Length[SEQUENCE]]
    ] ;
    RESULT
  ]
] ;
(* ################################################################################################################################################################ *)
(* MATHEMATICA TO CODE REPLACEMENT RULES *)
(* ################################################################################################################################################################ *)
ClearAll[I$RULE$CODE] ;
I$RULE$CODE::usage = "I$RULE$CODE -- WM to CODE replacement rules " ;
I$RULE$CODE = Dispatch[
  List[
    Rule[Plus,I$PLUS],
    Rule[Times,I$TIMES],
    Rule[Power[E,VARIABLE_],I$EXP[VARIABLE]],
    Rule[Power[VARIABLE_,Rational[+1,2]],I$SQRT[VARIABLE]],
    Rule[Power[VARIABLE_,Rational[-1,2]],I$DIVIDE[1,I$SQRT[VARIABLE]]],
    Rule[Power[VARIABLE_,-1],I$DIVIDE[1,VARIABLE]],
    Rule[Power[VARIABLE_,POWER_],I$POWER[VARIABLE,POWER]],
    Rule[Exp,I$EXP],
    Rule[Sin,I$SIN],
    Rule[Cos,I$COS],
    Rule[Tan,I$TAN],
    Rule[Cot,I$COT],
    Rule[ArcSin,I$ARCSIN],
    Rule[ArcCos,I$ARCCOS],
    Rule[ArcTan,I$ARCTAN],
    Rule[ArcCot,I$ARCCOT],
    Rule[Sinh,I$SINH],
    Rule[Cosh,I$COSH],
    Rule[E,I$E],
    Rule[Pi,I$PI],
    Rule[If,I$IF],
    Rule[Do,I$DO],
    Rule[Greater,I$GREATER],
    Rule[Less,I$LESS],
    Rule[GreaterEqual,I$GREATER$EQUAL],
    Rule[LessEqual,I$LESS$EQUAL],
    Rule[Equal,I$EQUAL],
    Rule[Sec,I$SEC],
    Rule[Csc,I$CSC],
    Rule[Sqrt,I$SQRT],
    Rule[Log,I$LOG],
    Rule[Log10,I$LOG10],
    Rule[I$TIMES[+N[1],VARIABLE_],+VARIABLE],
    Rule[I$TIMES[-N[1],VARIABLE_],-VARIABLE]
  ]
] ;
(* ################################################################################################################################################################ *)
(* CODE (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$CODE$OPTIMIZE$EXPRESSION] ;
I$CODE$OPTIMIZE$EXPRESSION::usage = "
I$CODE$OPTIMIZE$EXPRESSION[EXPRESSION,LEVEL] -- optimize expression <EXPRESSION> using optimization level <LEVEL> (integer)
" ;
I$CODE$OPTIMIZE$EXPRESSION[       (* -- OPTIMIZE COMPUTATION OF COMMON EXPRESSIONS *)
  EXPRESSION_,                    (* -- EXPRESSION TO OPTIMIZE *)
  LEVEL_Integer                   (* -- OPTIMIZATION LEVEL *)
] := Catch[
  Block[
    {RESULT,COMMON,LENGTH,OPTIMIZATION,INDEX},
    RESULT = EXPRESSION ;
    RESULT = Experimental`OptimizeExpression[RESULT,Rule["OptimizationLevel",LEVEL]] ;
    RESULT = Fold[ReplaceAll,RESULT,List[Rule[Set,I$SET],Rule[Block,I$BLOCK],Rule[CompoundExpression,I$COMPOUND$EXPRESSION],Rule[Experimental`OptimizedExpression,Identity]]] ;
    If[
      SameQ[Head[RESULT],I$BLOCK],
      List[
        COMMON = First[RESULT] ;
        LENGTH = Length[COMMON] ;
        OPTIMIZATION = ToExpression[Map[StringTemplate["I$OPT$``"],Range[LENGTH]]] ;
        COMMON = Dispatch[Thread[Rule[COMMON,OPTIMIZATION]]] ;
        RESULT = ReplaceAll[RESULT,COMMON] ;
      ],
      LENGTH = 0 ;
      OPTIMIZATION = List[] ;
      RESULT = I$BLOCK[List[],I$COMPOUND$EXPRESSION[RESULT]] ;
    ] ;
    INDEX = Last[RESULT] ;
    INDEX = Last[INDEX] ;
    INDEX = Flatten[Position[Map[Apply[SameQ],Transpose[List[I$VARIABLES,INDEX]]],False]] ;
    Part[RESULT,Subtract[0,1],Subtract[0,1]] = Part[RESULT,Subtract[0,1],Subtract[0,1],INDEX] ;
    RESULT = ReplaceRepeated[RESULT,I$RULE$CODE] ;
    RESULT = ReplaceAll[RESULT,I$RULE$STATE] ;
    RESULT = I$SET[Apply[I$STATE,List[INDEX]],RESULT] ;
    RESULT
  ]
] ;
ClearAll[I$CODE$OPTIMIZE$MAP] ;
I$CODE$OPTIMIZE$MAP::usage = "
I$CODE$OPTIMIZE$MAP[NAME,LEVEL] -- optimize map (common expressions computation and ) with name <NAME> (string) using optimization level <LEVEL> (integer)
" ;
I$CODE$OPTIMIZE$MAP[              (* -- MAP OPTIMIZATION *)
  NAME_String,                    (* -- MAP NAME (STRING) *)
  LEVEL_Integer:Plus[1]           (* -- OPTIMIZATION LEVEL *)
] := Catch[
  Block[
    {DATA,RULE,VARIABLE,NUMBER},
    DATA = I$TABLE["MAP"][NAME,"MAP"] ;
    DATA = Through[DATA[Apply[Sequence,Flatten[List[I$PARAMETER,I$TABLE["MAP"][NAME,"LOCAL"]]]]]] ;
    DATA = Map[Function[I$CODE$OPTIMIZE$EXPRESSION[Slot[1],LEVEL]],DATA] ;
    RULE = I$TABLE["MAP"][NAME,"LOCAL"] ;
    RULE = Dispatch[Thread[Rule[RULE,Array[I$LOCAL,Length[RULE]]]]] ;
    DATA = ReplaceAll[DATA,RULE] ;
    VARIABLE = DeleteDuplicates[Flatten[Cases[DATA,Rule[I$BLOCK[List[VARIABLE___],___],List[VARIABLE]],Infinity]]] ;
    NUMBER = Length[VARIABLE] ;
    DATA = Association[
      Rule["SET",DATA],
      Rule["LENGTH",NUMBER],
      Rule["DATA",VARIABLE]
    ] ;
    ReplaceAll[I$TABLE["MAP"][NAME],Rule[Null,DATA]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* CODE (SEQUENCE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$CODE$OPTIMIZE$SEQUENCE] ;
I$CODE$OPTIMIZE$SEQUENCE::usage = "
I$CODE$OPTIMIZE$SEQUENCE[NAME] -- optimize sequence (common weights) with name <NAME> (string)
" ;
I$CODE$OPTIMIZE$SEQUENCE[       (* -- SEQUENCE OPTIMIZATION *)
  NAME_String                   (* -- SEQUENCE NAME (STRING) *)
] := Catch[
  Block[
    {DATA,WEIGHT,NUMBER,VARIABLE},
    (* SET SEQUENCE ORDERING *)
    DATA = I$TABLE["SEQUENCE"][NAME,"ORDERING"] ;
    (* SET WEIGHTS *)
    WEIGHT = Map[Last,DATA] ;
    (* REMOVE IDENTCAL WEIGHTS *)
    WEIGHT = DeleteDuplicates[WEIGHT] ;
    (* SET NUMBER OF UNIQUE WEIGHTS *)
    NUMBER = Length[WEIGHT] ;
    (* SET WEIGHTS REPLACEMENT RULE *)
    VARIABLE = ToExpression[Map[StringTemplate["I$OPT$WEI$`1`"],Range[Length[WEIGHT]]]] ;
    WEIGHT = Thread[Rule[WEIGHT,VARIABLE]] ;
    (* MODIFY SEQUENCE *)
    DATA = ReplaceAll[DATA,WEIGHT] ;
    Part[DATA,All,-1] = Thread[Rule[I$PARAMETER,Part[DATA,All,-1]]] ;
    (* MODIFY WEIGHTS *)
    WEIGHT = Transpose[Map[Composition[Apply[List],Reverse],WEIGHT]] ;
    (* SET CODE *)
    DATA = List[List[First[WEIGHT],Map[Apply[I$SET],Transpose[WEIGHT]]],DATA] ;
    DATA = Association[
      Rule["SET",DATA],
      Rule["LENGTH",NUMBER],
      Rule["DATA",VARIABLE]
    ] ;
    ReplaceAll[I$TABLE["SEQUENCE"][NAME],Rule[Null,DATA]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* CODE *)
(* ################################################################################################################################################################ *)
ClearAll[I$CODE] ;
I$CODE::usage = "
I$CODE[] -- generate (optimized) code for current I$TABLE[\"MAP\"] and I$TABLE[\"SEQUENCE\"]
" ;
Options[I$CODE] = List[
  Rule["LEVEL",Plus[1]],          (* -- OPTIMIZATION LEVEL (INTEGER) *)
  Rule["MAP",Map]                 (* -- MAP FUNCTION *)
] ;
I$CODE[                           (* -- CODE GENERATION *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {FUNCTION,LEVEL,MAP,SEQUENCE},
  FUNCTION = OptionValue["MAP"] ;
  LEVEL = OptionValue["LEVEL"] ;
  MAP = FUNCTION[Function[I$CODE$OPTIMIZE$MAP[Slot[1],LEVEL]],Keys[I$TABLE["MAP"]]] ;
  MAP = Map[Apply[Rule],Transpose[List[Keys[I$TABLE["MAP"]],MAP]]] ;
  MAP = DeleteCases[MAP,Rule[_,Null]] ;
  SEQUENCE = FUNCTION[I$CODE$OPTIMIZE$SEQUENCE,Keys[I$TABLE["SEQUENCE"]]] ;
  SEQUENCE = Map[Apply[Rule],Transpose[List[Keys[I$TABLE["SEQUENCE"]],SEQUENCE]]] ;
  SEQUENCE = DeleteCases[SEQUENCE,Rule[_,Null]] ;
  Map[Function[I$TABLE$APPEND["MAP",Slot[1]]],MAP] ;
  Map[Function[I$TABLE$APPEND["SEQUENCE",Slot[1]]],SEQUENCE] ;
] ;
(* ################################################################################################################################################################ *)
(* CODE TO MATHEMATICA REPLACEMENT RULES *)
(* ################################################################################################################################################################ *)
ClearAll[I$RULE$MATHEMATICA] ;
I$RULE$MATHEMATICA::usage = "
I$RULE$MATHEMATICA -- CODE to WM replacement rules
" ;
I$RULE$MATHEMATICA = Dispatch[
  List[
    Rule[I$BLOCK,Block],
    Rule[I$COMPOUND$EXPRESSION,CompoundExpression],
    Rule[I$SEQUENCE,Sequence],
    Rule[I$SET,Set],
    Rule[I$PLUS,Plus],
    Rule[I$TIMES,Times],
    Rule[I$DIVIDE,Divide],
    Rule[I$POWER,Power],
    Rule[I$EXP,Exp],
    Rule[I$SIN,Sin],
    Rule[I$COS,Cos],
    Rule[I$TAN,Tan],
    Rule[I$COT,Cot],
    Rule[I$ARCSIN,ArcSin],
    Rule[I$ARCCOS,ArcCos],
    Rule[I$ARCTAN,ArcTan],
    Rule[I$ARCCOT,ArcCot],
    Rule[I$SINH,Sinh],
    Rule[I$COSH,Cosh],
    Rule[I$MATH$E,E],
    Rule[I$MATH$PI,Pi],
    Rule[I$IF,If],
    Rule[I$DO,Do],
    Rule[I$GREATER,Greater],
    Rule[I$LESS,Less],
    Rule[I$GREATER$EQUAL,GreaterEqual],
    Rule[I$LESS$EQUAL,LessEqual],
    Rule[I$EQUAL,Equal],
    Rule[I$LIST,List],
    Rule[I$THROW,Throw],
    Rule[I$CATCH,Catch],
    Rule[I$COMPILE,Compile],
    Rule[I$SQRT,Sqrt],
    Rule[I$PART,Part],
    Rule[I$SEC,Sec],
    Rule[I$CSC,Csc],
    Rule[I$LOG,Log],
    Rule[I$LOG10,Log10]
  ]
] ;
(* ################################################################################################################################################################ *)
(* COMPILE ELEMENT (COMPILED FUNCTION) *)
(* ################################################################################################################################################################ *)
(* NOTE: COMPILED FUNCTION TAKES TWO ARGUMENTS <STATE> AND <GLOBAL> (LIST OF VALUES FOR GLOBAL CONSTANTS IF ANY OR EMPTY LIST) *)
(* ################################################################################################################################################################ *)
ClearAll[I$COMPILE$ELEMENT] ;
I$COMPILE$ELEMENT::usage = "
I$COMPILE$ELEMENT[NAME,MAX,LENGTH,FLAG] -- compile element <NAME> (string) for given maximum integration step length <MAX> (list of reals), total length for each sequence <LENGTH> (list of reals) and flag values <FLAG> (list of reals or empty list) (use ReleaseHold[] to compile, returns function of two arguments, first arguments is state, seconds argument is list of global constants or empty list)
" ;
Options[I$COMPILE$ELEMENT] = List[
  Rule[CompilationTarget,"C"],
  Rule[RuntimeOptions,List["Speed",Rule["EvaluateSymbolically",False]]],
  Rule[CompilationOptions,List[Rule["ExpressionOptimization",True]]],
  Rule[RuntimeAttributes,List[Listable]],
  Rule[Parallelization,True]
] ;
I$COMPILE$ELEMENT[                (* -- CONVERT ELEMENT OJBECT TO MATHEMATICA (READY TO COMPILE FUNCTION) (USE ReleaseHold[] TO COMPILE) *)
  NAME_String,                    (* -- ELEMENT TO CONVERT (I$ELEMENT[] OTPUT)*)
  MAX_List,                       (* -- MAXIMUM INTEGRATION STEP FOR EACH SEQUENCE (LIST OF REALS) *)
  LENGTH_List,                    (* -- TOTAL INTEGRATION LENGTH FOR EACH SEQUENCE (LIST OF REALS) *)
  FLAG_List,                      (* -- FLAG VALUES *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) (COMPILE) *)
] := Catch[
  Block[
    {ITERATION,STEP,ORDERING,WEIGHT,SEQUENCE,RULE,MAP,LEFT,RIGHT},
    ITERATION = Ceiling[N[Divide[Abs[LENGTH],MAX]]] ;
    STEP = N[Divide[LENGTH,ITERATION]] ;
    ORDERING = I$TABLE["ELEMENT"][NAME,"ORDERING"] ;
    ORDERING = Map[I$TABLE["SEQUENCE"],ORDERING]  ;
    List[WEIGHT,SEQUENCE] = Transpose[Through[ORDERING["CODE","SET"]]] ;
    WEIGHT = Last[Transpose[WEIGHT]] ;
    Part[WEIGHT,All,All,-1] = Times[Part[WEIGHT,All,All,-1],STEP] ;
    WEIGHT = Map[Dispatch,ReplaceAll[WEIGHT,Rule[I$SET,Rule]]] ;
    MAP = DeleteDuplicates[Flatten[Map[Map[First],SEQUENCE]]] ;
    MAP = Dispatch[Thread[Rule[MAP,N[ReplaceAll[Map[Apply[I$COMPOUND$EXPRESSION],Through[Map[I$TABLE["MAP"],MAP]["CODE","SET"]]],Dispatch[Thread[Rule[I$FLAG$PARAMETRIC,FLAG]]]]]]]] ;
    SEQUENCE = MapThread[ReplaceAll,List[SEQUENCE,WEIGHT]] ;
    RULE = I$TABLE["ELEMENT"][NAME,"GLOBAL"] ;
    RULE = Dispatch[Thread[Rule[RULE,Array[I$GLOBAL,Length[RULE]]]]] ;
    RULE = ReplaceAll[I$TABLE["ELEMENT"][NAME,"RELATION"],RULE] ;
    RULE = List[Through[Map[I$TABLE["SEQUENCE"],I$TABLE["ELEMENT"][NAME,"ORDERING"]]["UNIQUE"]],RULE] ;
    RULE = Map[Transpose,Transpose[RULE]] ;
    RULE = MapAt[Function[I$SET[I$LOCAL,Slot[1]]],RULE,List[All,All,Plus[1,1]]] ;
    RULE = MapAt[Apply[Rule],RULE,List[All,All]] ;
    RULE = ReplaceAll[RULE,Rule[Rule[LEFT_,RIGHT_],Rule[LEFT,List[RIGHT,LEFT]]]] ;
    RULE = Map[Dispatch,RULE] ;
    SEQUENCE = MapThread[ReplaceAll,List[SEQUENCE,RULE]] ;
    SEQUENCE = ReplaceAll[SEQUENCE,MAP] ;
    SEQUENCE = Map[Map[Apply[ReplaceAll]],SEQUENCE] ;
    SEQUENCE = Map[Flatten,SEQUENCE] ;
    SEQUENCE = DeleteCases[SEQUENCE,I$SET[I$LOCAL,List[]],Infinity] ;
    SEQUENCE = Map[Apply[I$COMPOUND$EXPRESSION],SEQUENCE] ;
    SEQUENCE = MapThread[I$DO,List[SEQUENCE,ITERATION]] ;
    SEQUENCE = Apply[I$COMPOUND$EXPRESSION,SEQUENCE] ;
    SEQUENCE = SEQUENCE //. List[
      I$STATE[INDEX_]          :> I$PART[I$STATE,INDEX],
      I$STATE[INDEX__]         :> I$PART[I$STATE,List[INDEX]],
      I$STATE[List[INDEX_]]    :> I$PART[I$STATE,List[INDEX]],
      I$GLOBAL[INDEX_]         :> I$PART[I$GLOBAL,INDEX],
      I$LOCAL[INDEX_]          :> I$PART[I$LOCAL,INDEX]
    ] ;
    SEQUENCE = I$CATCH[
      I$BLOCK[
        List[I$STATE,I$GLOBAL,I$LOCAL],
        I$COMPOUND$EXPRESSION[
          I$SET[I$STATE,C$VAR],
          I$SET[I$GLOBAL,C$PAR],
          I$IF[I$LESS[I$PART[I$STATE,1],N[1/2]],I$THROW[I$STATE]],
          SEQUENCE,
          I$STATE
        ]
      ]
    ] ;
    SEQUENCE = SEQUENCE //. List[
      I$COMPOUND$EXPRESSION[EXPRESSION_] :> EXPRESSION,
      I$BLOCK[List[],List[EXPRESSION_]]  :> List[EXPRESSION]
    ] ;
    SEQUENCE = I$COMPILE[
      List[List[C$VAR,_Real,1],List[C$PAR,_Real,1]],
      SEQUENCE,
      Rule[CompilationTarget,OptionValue[CompilationTarget]],
      Rule[RuntimeOptions,OptionValue[RuntimeOptions]],
      Rule[CompilationOptions,OptionValue[CompilationOptions]]
    ] ;
    SEQUENCE = Apply[Hold,List[SEQUENCE]] ;
    SEQUENCE = ReplaceRepeated[SEQUENCE,I$RULE$MATHEMATICA] ;
    ReplaceAll[SEQUENCE,Rule[Times[1.0,X_],X]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* SAVE COMPILED FUNCTION *)
(* ################################################################################################################################################################ *)
ClearAll[I$COMPILE$SAVE] ;
I$COMPILE$SAVE::usage = "
I$COMPILE$SAVE[FUNCTION,NAME] -- save compiled function <FUNCTION> into file <NAME> (name without extension)
" ;
I$COMPILE$SAVE[                   (* -- SAVE COMPILED FUNCTION *)
  FUNCTION_CompiledFunction,      (* -- FUNCTION TO SAVE *)
  NAME_String                     (* -- FUNCTION NAME (STRING) *)
] := Block[
  {DATA,DIRECTORY},
  DATA = Last[FUNCTION] ;
  DIRECTORY = FileNameJoin[List[I$DIRECTORY,StringJoin[ToLowerCase[NAME],".so"]]] ;
  If[FileExistsQ[DIRECTORY],DeleteFile[DIRECTORY]] ;
  CopyFile[First[DATA],DIRECTORY] ;
  DATA = Flatten[List[NAME,Rest[Apply[List,DATA]]],1] ;
  DATA = Flatten[List[DIRECTORY,Rest[DATA]],1] ;
  DATA = With[List[Set[DATA,DATA]],Hold[Apply[LibraryFunctionLoad,DATA]]] ;
  DIRECTORY = FileNameJoin[List[I$DIRECTORY,StringJoin[ToLowerCase[NAME],".mx"]]] ;
  Export[DIRECTORY,DATA] ;
] ;
(* ################################################################################################################################################################ *)
(* LOAD COMPILED FUNCTION *)
(* ################################################################################################################################################################ *)
ClearAll[I$COMPILE$LOAD] ;
I$COMPILE$LOAD::usage = "
I$COMPILE$LOAD[NAME] -- load compiled function <NAME> (string)
" ;
I$COMPILE$LOAD[                   (* -- LOAD COMPILED FUNCTION *)
  NAME_String                     (* -- FUNCTION NAME *)
] := Block[
  {DIRECTORY},
  DIRECTORY = FileNameJoin[List[I$DIRECTORY,StringJoin[ToLowerCase[NAME],".mx"]]] ;
  ReleaseHold[Import[DIRECTORY]]
] ;
(* ################################################################################################################################################################ *)
(* CHECK COMPILED FUNCTION (MAIN EVALUATE CALLS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$COMPILE$CHECK] ;
I$COMPILE$CHECK::usage ="
I$COMPILE$CHECK[FUNCTION] -- check calls to MainEvaluate[] for given compiled function <FUNCTION> (compiled function), returns 'True' if no calls present
" ;
I$COMPILE$CHECK = Function[VerificationTest[StringFreeQ[CompilePrint[Slot[1]],"MainEvaluate"]]] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (DIRECTORY) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$DIRECTORY] ;
I$FORTRAN$DIRECTORY::usage = "
I$FORTRAN$DIRECTORY -- FORTRAN working directory (string), default value \"I$DIRECTORY\"
" ;
I$FORTRAN$DIRECTORY := I$DIRECTORY ;
(* ################################################################################################################################################################ *)
(* FORTRAN (COMPILER) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$COMPILER] ;
I$FORTRAN$COMPILER::usage = "
I$FORTRAN$COMPILER -- FORTRAN compiler (string), default value \"gfortran-10\"
" ;
I$FORTRAN$COMPILER := "gfortran-10" ;
(* ################################################################################################################################################################ *)
(* FORTRAN (FLAGS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$FLAGS] ;
I$FORTRAN$FLAGS::usage = "
I$FORTRAN$FLAGS -- FORTRAN compilation flags (string), default value \"-std=f2018 -Wall -pedantic -fPIC -O3 -ffast-math -frecursive -march=native -Wno-unused-function -Wno-unused-value\"
" ;
I$FORTRAN$FLAGS := "-std=f2018 -Wall -pedantic -fPIC -O3 -ffast-math -frecursive -march=native -Wno-unused-dummy-argument -Wno-unused-variable -Wno-unused-function -Wno-unused-value" ;
(* ################################################################################################################################################################ *)
(* C (FLAGS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$C$FLAGS] ;
I$C$FLAGS::usage = "
I$C$FLAGS -- C compilation flags (string), default value \"-O3 -ffast-math -march=native\"
" ;
I$C$FLAGS := " -O3 -ffast-math -march=native " ;
(* ################################################################################################################################################################ *)
(* FORTRAN (REAL KIND) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$REAL$KIND] ;
I$FORTRAN$REAL$KIND::usage = "
I$FORTRAN$REAL$KIND -- FORTRAN real kind (string), default value \"RK\"
" ;
I$FORTRAN$REAL$KIND := "RK" ;
(* ################################################################################################################################################################ *)
(* FORTRAN (REAL KIND VALUE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$REAL$KIND$VALUE] ;
I$FORTRAN$REAL$KIND$VALUE::usage = "
I$FORTRAN$REAL$KIND$VALUE -- FORTRAN real kind value (string), default value \"C_DOUBLE\"
" ;
I$FORTRAN$REAL$KIND$VALUE := "C_DOUBLE" ;
(* ################################################################################################################################################################ *)
(* FORTRAN (PRECISION) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$REAL$PRECISION] ;
I$FORTRAN$REAL$PRECISION::usage = "
I$FORTRAN$REAL$PRECISION -- FORTRAN real precision (integer)
" ;
I$FORTRAN$REAL$PRECISION := Ceiling[$MachinePrecision] + 1 ;
(* ################################################################################################################################################################ *)
(* FORTRAN (REAL) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$REAL] ;
I$FORTRAN$REAL::usage = "
I$FORTRAN$REAL[REAL] -- convert real number <REAL> (real) to FORTRAN form (string) using given kind I$FORTRAN$REAL$KIND and precision I$FORTRAN$REAL$PRECISION
" ;
I$FORTRAN$REAL[                   (* -- CONVERT REAL NUMBER TO FORTRAN FORM (RETURNS STRING), ACTS AS IDENTITY ON STRING ARGUMENTS *)
  REAL_Real                       (* -- NUMBER (REAL) *)
] := Block[
  {MANTISSA,EXPONENT,SIGN},
  {MANTISSA,EXPONENT} = If[SameQ[REAL,N[0,Precision[REAL]]],List[N[0,Precision[REAL]],1],MantissaExponent[REAL]] ;
  MANTISSA = N[Times[10,MANTISSA],I$FORTRAN$REAL$PRECISION] ;
  MANTISSA = NumberForm[MANTISSA,Rule[DefaultPrintPrecision,I$FORTRAN$REAL$PRECISION]] ;
  EXPONENT = Subtract[EXPONENT,1] ;
  SIGN = Sign[EXPONENT] ;
  SIGN = Which[GreaterEqual[SIGN,0],"+",Less[SIGN,0],"-"] ;
  EXPONENT = Abs[EXPONENT] ;
  MANTISSA = ToString[MANTISSA] ;
  MANTISSA =  StringTrim[MANTISSA,"0"..] ;
  MANTISSA = If[SameQ[MANTISSA,"."],"0.",MANTISSA] ;
  If[UnsameQ[EXPONENT,0],StringTemplate["`1`E`2``3`_`4`"][MANTISSA,SIGN,EXPONENT,I$FORTRAN$REAL$KIND],StringTemplate["`1`_`2`"][MANTISSA,I$FORTRAN$REAL$KIND]]
] ;
I$FORTRAN$REAL[REAL_String] := REAL ;
(* ################################################################################################################################################################ *)
(* FORTRAN (LENGTH) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$LENGTH] ;
I$FORTRAN$LENGTH::usage = "
I$FORTRAN$LENGTH -- FORTRAN line length (integer), default value \"120\"
" ;
I$FORTRAN$LENGTH := 130 ;
(* ################################################################################################################################################################ *)
(* FORTRAN (SPLIT) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$SPLIT] ;
I$FORTRAN$SPLIT::usage = "
I$FORTRAN$SPLIT[STRING] -- split string <STR> using FORTRAN line split
" ;
I$FORTRAN$SPLIT[                  (* -- SPLIT LINE *)
  STRING_String                   (* -- STRING TO SPLIT *)
] := Block[
  {LINE,LENGTH},
  LINE = StringDelete[STRING," "] ;
  LINE = StringPartition[LINE,UpTo[Subtract[I$FORTRAN$LENGTH,Plus[1,1]]]] ;
  LENGTH = Length[LINE] ;
  LINE = Flatten[Transpose[List[ConstantArray[" &",LENGTH],LINE,ConstantArray["&\n",LENGTH]]]] ;
  LINE = Rest[LINE] ;
  LINE = Most[LINE] ;
  StringJoin[" ",LINE]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (RULES) (ACT ON STRINGS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$RULE$FORTRAN] ;
I$RULE$FORTRAN::usage = "
I$RULE$FORTRAN -- WM to FORTRAN replacement rules (acts on strings)
" ;
I$RULE$FORTRAN = List[
  Rule[" ",""],
  Rule["+-","-"],
  Rule["-+","-"],
  Rule["\"",""],
  Rule["{","["],
  Rule["}","]"],
  Rule["[","(["],
  Rule["]","])"],
  Rule[",",", "],
  Rule["Sqrt","SQRT"],
  Rule["Exp","EXP"],
  Rule["Log","LOG"],
  Rule["Log10","LOG10"],
  Rule["Sin","SIN"],
  Rule["Cos","COS"],
  Rule["Tan","TAN"],
  Rule["Cot","COT"],
  Rule["Sinh","SINH"],
  Rule["Cosh","COSH"],
  Rule["Tanh","TANH"],
  Rule["Coth","COTH"],
  Rule["ArcSin","ASIN"],
  Rule["ArcCos","ACOS"],
  Rule["ArcTan","ATAN"],
  Rule["ArcCot","ACOT"],
  Rule["Sec","SEC"],
  Rule["Csc","CSC"],
  Rule["Sech","SECH"],
  Rule["Csch","CSCH"]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (LINE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$LINE] ;
I$FORTRAN$LINE::usage = "
I$FORTRAN$LINE[EXPRESSION] -- generate FORTRAN line for given WM expression <EXPRESSION>
"  ;
I$FORTRAN$LINE[                   (* -- MAKE FORTRAN LINE (STRING) *)
  EXPRESSION_                     (* -- INPUT (WM EXPRESSION) *)
] := Block[
  {REAL,RULE,LINE},
  (* EXTRACT REAL NUMBERS *)
  REAL = DeleteDuplicates[Cases[EXPRESSION,_Real,Infinity]] ;
  (* GENERATE REPLACEMENT RULES FOR REAL NUMBERS *)
  RULE = Dispatch[Thread[Rule[REAL,Map[I$FORTRAN$REAL,REAL]]]] ;
  (* CONVERT TO FORTRAN *)
  LINE = StringReplace[ToString[ReplaceAll[FortranForm[EXPRESSION],RULE]],I$RULE$FORTRAN] ;
  (* SPLIT AND RETURN *)
  ToUpperCase[I$FORTRAN$SPLIT[StringReplace[LINE,I$RULE$FORTRAN]]]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (ARGUMENT) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$ARGUMENT] ;
I$FORTRAN$ARGUMENT::usage = "
I$FORTRAN$ARGUMENT[ARGUMENT] -- format FORTRAN argument, e.g. convert X([1,2,3,4]) to array section X(1:4:1)
" ;
I$FORTRAN$ARGUMENT[               (* -- FORTRAN ARGUMENT OPTIMIZATION *)
  ARGUMENT_String                 (* -- ARGUMENT (STRING), X([...]) *)
] := Catch[
  Block[
    {HEAD,LIST,DIFFERENCE,LENGTH},
    HEAD = StringCases[ARGUMENT,Rule[StringExpression[Pattern[HEAD,BlankSequence[]],"(",BlankNullSequence[]],HEAD]] ;
    LIST = StringCases[ARGUMENT,Rule[StringExpression[BlankNullSequence[],"[",Pattern[LIST,BlankSequence[]],"]",BlankNullSequence[]],LIST]] ;
    LIST = ToExpression[StringJoin["{",LIST,"}"]] ;
    If[SameQ[Length[LIST],1],Throw[ARGUMENT]] ;
    DIFFERENCE = Split[Differences[LIST]] ;
    LENGTH = Map[Length,DIFFERENCE] ;
    LENGTH = Plus[LENGTH,PadLeft[List[1],Length[LENGTH]]] ;
    LIST = TakeList[LIST,LENGTH] ;
    DIFFERENCE = Map[First,DIFFERENCE] ;
    LIST = Transpose[List[LIST,DIFFERENCE]] ;
    LIST = Map[Function[If[SameQ[Length[First[Slot[1]]],1],First[Slot[1]],Flatten[List[Part[First[Slot[1]],List[1,-1]],Last[Slot[1]]]]]],LIST] ;
    LIST = Map[Function[If[SameQ[Length[Slot[1]],1],ToString[First[Slot[1]]],Apply[StringTemplate["[(I,I=`1`,`2`,`3`)]"]][Slot[1]]]],LIST] ;
    If[
      SameQ[Length[LIST],1],
      StringReplace[StringDelete[StringJoin[HEAD,"(",StringTake[ToString[LIST],List[8,-4]],")"]," "],Rule[",",":"]],
      StringDelete[StringJoin[HEAD,"([",StringTake[ToString[LIST],List[2,-2]],"])"]," "]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (REPLACE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$REPLACE] ;
I$FORTRAN$REPLACE::usage = "
I$FORTRAN$REPLACE[TABLE,NAME] -- generate replacement rules for given name <NAME> (string) in table <TABLE> (string)
" ;
I$FORTRAN$REPLACE[                (* -- STRING REPLACEMEN RULES *)
  TABLE_String,                   (* -- TYPE (STRING) *)
  NAME_String                     (* -- NAME (STRING) *)
] := Catch[
  Which[
    SameQ[TABLE,"MAP"],
    Block[
      {LENGTH,VARIABLE},
      LENGTH = I$TABLE[TABLE][NAME,"CODE","LENGTH"] ;
      VARIABLE = I$TABLE[TABLE][NAME,"CODE","DATA"] ;
      VARIABLE = Join[
        Thread[Rule[I$FLAG$PARAMETRIC,Map[StringTemplate["F(`1`)"],Range[I$DIMENSION$PARAMETRIC]]]],
        Thread[Rule[VARIABLE,Map[StringTemplate["O(`1`)"],Range[LENGTH]]]]
      ] ;
      Dispatch[Flatten[List[VARIABLE,Rule[I$STATE,"X"],Rule[I$LOCAL,"L"],Rule[I$GLOBAL,"G"],Rule[I$PARAMETER,"S"]]]]
    ],
    SameQ[TABLE,"SEQUENCE"],
    Block[
      {LENGTH,VARIABLE},
      LENGTH = I$TABLE[TABLE][NAME,"CODE","LENGTH"] ;
      VARIABLE = I$TABLE[TABLE][NAME,"CODE","DATA"] ;
      VARIABLE = Thread[Rule[VARIABLE,Map[StringTemplate["S(`1`)"],Range[LENGTH]]]] ;
      Dispatch[Flatten[List[VARIABLE,Rule[I$STATE,"X"],Rule[I$LOCAL,"L"],Rule[I$GLOBAL,"G"],Rule[I$PARAMETER,"S"]]]]
    ],
    SameQ[TABLE,"ELEMENT"],
    Dispatch[Flatten[List[Thread[Rule[I$TABLE["ELEMENT"][NAME,"GLOBAL"],Array[StringTemplate["G(`1`)"],Length[I$TABLE["ELEMENT"][NAME,"GLOBAL"]]]]],Rule[I$STATE,"X"],Rule[I$LOCAL,"L"],Rule[I$GLOBAL,"G"],Rule[I$PARAMETER,"S"]]]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (SET) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$SET] ;
I$FORTRAN$SET::usage = "
I$FORTRAN$SET[RULE][I$SET[LEFT,RIGHT]] -- convert set statement to FORTRAN
" ;
I$FORTRAN$SET[RULE_:List[]][I$SET[LEFT_,RIGHT_]] := Catch[
  Block[
    {LHS,RHS,VARIABLE},
    {LHS,RHS} = {LEFT,RIGHT} ;
    LHS = ReplaceRepeated[LHS,I$RULE$MATHEMATICA] ;
    LHS = ReplaceAll[LHS,RULE] ;
    LHS = ReplaceAll[LHS,Rule[List,Sequence]] ;
    LHS = I$FORTRAN$LINE[LHS] ;
    LHS = StringReplace[LHS,List[Rule["(","(["], Rule[")","])"]]] ;
    LHS = StringDelete[LHS,List["\n","&"," "]] ;
    LHS = If[
      StringContainsQ[LHS,StringExpression["([",BlankNullSequence[],"])"]],
      I$FORTRAN$ARGUMENT[StringDelete[LHS,List["&","\n"]]],
      StringReplace[LHS,List[Rule["[",""],Rule["]",""]]]
    ] ;
    If[SameQ[LHS,"X([])"],Throw["X=X\n"]] ;
    RHS = ReplaceRepeated[RHS,I$RULE$MATHEMATICA] ;
    RHS = N[RHS,I$FORTRAN$REAL$PRECISION] ;
    RHS = ReplaceAll[RHS,Rule[Power[VARIABLE_,-1],Divide[N[1,I$FORTRAN$REAL$PRECISION],VARIABLE]]] ;
    RHS = ReplaceAll[RHS,RULE] ;
    RHS = If[
      SameQ[Head[RHS],List],
      StringJoin["[",StringTake[I$FORTRAN$LINE[RHS],List[7,-2]],"]"],
      I$FORTRAN$LINE[RHS]
    ] ;
    RHS = StringDelete[RHS,List["\n","&"," "]] ;
    RHS = StringReplace[
      RHS,
      List[
        Rule["SEC(",StringTemplate["1._``/COS("][I$FORTRAN$REAL$KIND]],
        Rule["CSC(",StringTemplate["1._``/SIN("][I$FORTRAN$REAL$KIND]],
        Rule["SECH(",StringTemplate["1._``/COSH("][I$FORTRAN$REAL$KIND]],
        Rule["CSCH(",StringTemplate["1._``/SINH("][I$FORTRAN$REAL$KIND]]
      ]
    ] ;
    RHS = StringReplace[RHS,Rule[StringExpression[".",Repeated["0"],"_"],"._"]] ;
    RHS = StringReplace[RHS,Rule[StringExpression[".",Repeated["0"],"E"],".E"]] ;
    RHS = StringDelete[RHS,List[StringTemplate["1._`1`*"][I$FORTRAN$REAL$KIND]]] ;
    LHS = If[UnsameQ[StringTake[RHS,1],"["],StringDelete[LHS,List["[","]"]],LHS] ;
    If[
      StringMatchQ[LHS,StringExpression["X([",Blank[],"])"]],
      I$FORTRAN$SPLIT[StringDelete[StringJoin[LHS,"=",RHS],List["[","]"]]],
      I$FORTRAN$SPLIT[StringJoin[LHS,"=",RHS]]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (DEFINITIONS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$DEFINITIONS] ;
I$FORTRAN$DEFINITIONS::usage = "
I$FORTRAN$DEFINITIONS[TABLE,NAME] -- generate FORTRAN definitions for given table <TABLE> (string) and name <NAME> (string)
" ;
Options[I$FORTRAN$DEFINITIONS] = List[
  Rule["EXTEND",0],               (* -- EXTEND STATE DIMENSION (INTEGER) *)
  Rule["USE",List[]],             (* -- USE (LIST OF STRINGS) *)
  Rule["PARAMETERS",List[]],      (* -- PARAMETERS (LIST OF STRINGS) *)
  Rule["VARIABLES",List[]]        (* -- VARIABLES (LIST OF STRINGS) *)
] ;
I$FORTRAN$DEFINITIONS[            (* -- GENERATE FORTRAN MAP DEFINITIONS *)
  "MAP",                          (* -- TABLE (STRING) *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {},
  Association[
    "TYPE" -> "MAP",
    "USE" -> StringJoin[
      " USE ISO_C_BINDING\n",
      Map[Composition[StringTemplate[" USE `1`\n"],ToUpperCase,StringTrim],OptionValue["USE"]]
    ],
    "MODULE" -> StringJoin[
      StringTemplate[" INTEGER,PARAMETER :: `1`=`2`\n"][I$FORTRAN$REAL$KIND,I$FORTRAN$REAL$KIND$VALUE],
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["PARAMETERS"]]
    ],
    "DATA" -> StringJoin[
      StringTemplate[" REAL(`1`),INTENT(IN) :: S\n"][I$FORTRAN$REAL$KIND],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: L\n"][I$FORTRAN$REAL$KIND,Max[List[1,Length[I$TABLE["MAP"][NAME,"LOCAL"]]]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(INOUT) :: X\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`) :: O\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$TABLE["MAP"][NAME,"CODE","LENGTH"]]]],
      " INTEGER :: I,J,K,N\n",
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["VARIABLES"]]
    ]
  ]
] ;
I$FORTRAN$DEFINITIONS[            (* -- GENERATE FORTRAN SEQUENCE DEFINITIONS *)
  "SEQUENCE",                     (* -- TABLE (STRING) *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {},
  Association[
    "TYPE" -> "SEQUENCE",
    "USE" -> StringJoin[
      " USE ISO_C_BINDING\n",
      Map[StringTemplate[" USE `1`_MAP, ONLY: `1`_MAP_SUB\n"],DeleteDuplicates[Map[First,I$TABLE["SEQUENCE"][NAME,"ORDERING"]]]],
      Map[Composition[StringTemplate[" USE `1`\n"],ToUpperCase,StringTrim],OptionValue["USE"]]
    ],
    "MODULE" -> StringJoin[
      StringTemplate[" INTEGER,PARAMETER :: `1`=`2`\n"][I$FORTRAN$REAL$KIND,I$FORTRAN$REAL$KIND$VALUE],
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["PARAMETERS"]]
    ],
    "DATA" -> StringJoin[
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: S\n"][I$FORTRAN$REAL$KIND,I$TABLE["SEQUENCE"][NAME,"CODE","LENGTH"]],
      MapThread[
        Function[StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: L_`3`\n"][I$FORTRAN$REAL$KIND,SlotSequence[]]],
        List[
          ReplaceAll[I$TABLE["SEQUENCE"][NAME,"LENGTH_LOCAL"],Rule[0,1]],
          Range[I$TABLE["SEQUENCE"][NAME,"LENGTH_UNIQUE"]]
        ]
      ],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(INOUT) :: X\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
      " INTEGER :: I,J,K,N\n",
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["VARIABLES"]]
    ]
  ]
] ;
I$FORTRAN$DEFINITIONS[            (* -- GENERATE FORTRAN SEQUENCE DEFINITIONS *)
  "ELEMENT",                      (* -- TABLE (STRING) *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {},
  Association[
    "TYPE" -> "ELEMENT",
    "USE" -> StringJoin[
      " USE ISO_C_BINDING\n",
      Map[StringTemplate[" USE `1`_SEQ, ONLY: `1`_SEQ_SUB\n"],DeleteDuplicates[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      Map[Composition[StringTemplate[" USE `1`\n"],ToUpperCase,StringTrim],OptionValue["USE"]]
    ],
    "MODULE" -> StringJoin[
      StringTemplate[" INTEGER,PARAMETER :: `1`=`2`\n"][I$FORTRAN$REAL$KIND,I$FORTRAN$REAL$KIND$VALUE],
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["PARAMETERS"]]
    ],
    "DATA" -> StringJoin[
      StringTemplate[" LOGICAL,DIMENSION(`1`),INTENT(IN) :: T\n"][Length[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: MAX_STEP_LENGTH\n"][I$FORTRAN$REAL$KIND,Length[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: LENGTH\n"][I$FORTRAN$REAL$KIND,Length[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: G\n"][I$FORTRAN$REAL$KIND,Max[List[1,Length[I$TABLE["ELEMENT"][NAME,"GLOBAL"]]]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(INOUT) :: X\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
      StringTemplate[" INTEGER,DIMENSION(`1`) :: NUMBER_OF_STEPS\n"][Length[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`) :: STEP_LENGTH\n"][I$FORTRAN$REAL$KIND,Length[I$TABLE["ELEMENT"][NAME,"ORDERING"]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`) :: S\n"][I$FORTRAN$REAL$KIND,Max[Through[Map[I$TABLE["SEQUENCE"],I$TABLE["ELEMENT"][NAME,"ORDERING"]]["CODE","LENGTH"]]]],
      StringTemplate[" REAL(`1`),DIMENSION(`2`) :: THIS, NEXT\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
      " INTEGER :: I,J,K,N\n",
      MapIndexed[
        Function[
          Block[
            {LIST,INDEX},
            LIST = Slot[1] ;
            INDEX = Slot[2] ;
            INDEX = First[INDEX] ;
            LIST = Map[Length,Through[Map[I$TABLE["MAP"],LIST]["LOCAL"]]] ;
            LIST = ReplaceAll[LIST,Rule[0,1]] ;
            INDEX = Transpose[List[LIST,ConstantArray[INDEX,Length[LIST]],Range[Length[LIST]]]] ;
            Map[
              Function[StringTemplate[" REAL(`1`), DIMENSION(`2`) :: L_`3`_`4`\n"][I$FORTRAN$REAL$KIND,Apply[Sequence,Slot[1]]]],
              INDEX
            ]
          ]
        ],
        Through[Map[I$TABLE["SEQUENCE"],I$TABLE["ELEMENT"][NAME,"ORDERING"]]["UNIQUE"]]
      ],
      Map[Composition[StringTemplate[" `1`\n"],ToUpperCase,StringTrim],OptionValue["VARIABLES"]]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$MAP] ;
I$FORTRAN$MAP::usage = "
I$FORTRAN$MAP[NAME] -- generates FORTRAN code for map <NAME> (string)
" ;
Options[I$FORTRAN$MAP] = List[
  Rule["EXTEND",0],               (* -- EXTEND STATE DIMENSION (INTEGER) *)
  Rule["USE",List[]],             (* -- USE (LIST OF STRINGS) *)
  Rule["PARAMETERS",List[]],      (* -- PARAMETERS (LIST OF STRINGS) *)
  Rule["VARIABLES",List[]],       (* -- VARIABLES (LIST OF STRINGS) *)
  Rule["PROLOG",List[""]],        (* -- PROLOG FORTRAN CODE TO EXECUTE *)
  Rule["EPILOG",List[""]],        (* -- EPILOG FORTRAN CODE TO EXECUTE *)
  Rule["IO_PROLOG",List[""]],     (* -- IO PROLOG *)
  Rule["IO_EPILOG",List[""]],     (* -- IO EPILOG *)
  Rule["IO_UNIT","*"],            (* -- IO UNIT *)
  Rule["IO_FORMAT","*"]           (* -- IO FORMAT *)
] ;
I$FORTRAN$MAP[                    (* -- GENERATE FORTRAN CODE FOR MAP *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {DEFINITION,RULE,INDEX,CODE,ARGUMENT,FIRST,LAST,PROLOG,EPILOG,ONE,TWO,SUBROUTINE,FILE},
    DEFINITION = I$FORTRAN$DEFINITIONS["MAP",NAME,Apply[Sequence,FilterRules[Flatten[List[OPTIONS,Options[I$FORTRAN$MAP]]],Options[I$FORTRAN$DEFINITIONS]]]] ;
    RULE = I$FORTRAN$REPLACE["MAP",NAME] ;
    CODE = I$TABLE["MAP"][NAME,"CODE","SET"] ;
    CODE = ReplaceAll[CODE,Rule[I$SET[ARGUMENT_,I$BLOCK[List[___],I$COMPOUND$EXPRESSION[FIRST___,List[LAST__]]]],List[FIRST,I$SET[ARGUMENT,List[LAST]]]]] ;
    If[
      UnsameQ[I$VARIABLES$DERIVATIVE,List[]],
      INDEX = Plus[1,I$DIMENSION$PARAMETRIC,I$DIMENSION$CANONICAL] ;
      CODE = ReplaceAll[
	      CODE,
        RuleDelayed[
	        I$SET[I$STATE[LIST__],EXPRESSION_],
	        List[
	          I$SET[I$STATE[Select[LIST,Curry[Greater][INDEX]]],Take[EXPRESSION,-Length[Select[LIST,Curry[Greater][INDEX]]]]],
	          I$SET[I$STATE[Select[LIST,Curry[LessEqual][INDEX]]],Take[EXPRESSION,+Length[Select[LIST,Curry[LessEqual][INDEX]]]]]
	        ]
	      ]
	    ] ;
    ] ;
    CODE = Flatten[CODE] ;
    CODE = Map[I$FORTRAN$SET[RULE],CODE] ;
    CODE = Map[StringJoin,Transpose[List[CODE,ConstantArray["\n",Length[CODE]]]]] ;
    CODE = StringJoin[CODE] ;
    PROLOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["PROLOG"]] ;
    EPILOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["EPILOG"]] ;
    ONE = Map[Composition[Function[StringTemplate[" !IO_PROLOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_PROLOG"]] ;
    TWO = Map[Composition[Function[StringTemplate[" !IO_EPILOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_EPILOG"]] ;
    SUBROUTINE = StringJoin[
      StringTemplate["SUBROUTINE `1`_MAP_SUB(S,L,F,X)\n"][NAME],
      DEFINITION["DATA"],
      StringDelete[PROLOG," \n"],
      ONE,
      CODE,
      StringDelete[EPILOG," \n"],
      TWO,
      StringTemplate["END SUBROUTINE `1`_MAP_SUB\n"][NAME]
    ] ;
    CODE = StringJoin[
      StringTemplate["MODULE `1`_MAP\n"][NAME],
      DEFINITION["USE"],
      " IMPLICIT NONE\n",
      StringTemplate[" PUBLIC :: `1`_MAP_SUB\n"][NAME],
      " PRIVATE\n",
      DEFINITION["MODULE"],
      "CONTAINS\n",
      SUBROUTINE,
      StringTemplate["END MODULE `1`_MAP\n"][NAME]
    ] ;
    CODE = ToUpperCase[CODE] ;
    FILE = StringJoin[ToLowerCase[NAME],"_map.f90"] ;
    CODE = StringJoin[
      StringTemplate["!`1` -c `2` `3`\n"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE],
      CODE
    ] ;
    Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,FILE]],CODE,"String"] ;
    Association[
      Rule["NAME",NAME],
      Rule["TYPE","MAP"],
      Rule["SRC",StringJoin[ToLowerCase[NAME],"_map"]],
      Rule["OBJ",StringTemplate["`1` -c `2` `3`"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE]],
      Rule["USE",List[]]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (SEQUENCE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$SEQUENCE] ;
I$FORTRAN$SEQUENCE::usage = "
I$FORTRAN$SEQUENCE[NAME] -- generates FORTRAN code for sequence <NAME> (string)
" ;
Options[I$FORTRAN$SEQUENCE] = List[
  Rule["EXTEND",0],               (* -- EXTEND STATE DIMENSION (INTEGER) *)
  Rule["USE",List[]],             (* -- USE (LIST OF STRINGS) *)
  Rule["PARAMETERS",List[]],      (* -- PARAMETERS (LIST OF STRINGS) *)
  Rule["VARIABLES",List[]],       (* -- VARIABLES (LIST OF STRINGS) *)
  Rule["PROLOG",List[""]],        (* -- PROLOG FORTRAN CODE TO EXECUTE *)
  Rule["EPILOG",List[""]],        (* -- EPILOG FORTRAN CODE TO EXECUTE *)
  Rule["IO_PROLOG",List[""]],     (* -- IO PROLOG *)
  Rule["IO_EPILOG",List[""]],     (* -- IO EPILOG *)
  Rule["IO_UNIT","*"],            (* -- IO UNIT *)
  Rule["IO_FORMAT","*"]           (* -- IO FORMAT *)
] ;
I$FORTRAN$SEQUENCE[               (* -- FORTRAN CODE FOR SEQUENCE *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {DEFINITION,RULE,PROLOG,EPILOG,ONE,TWO,CODE,SUBROUTINE,FILE},
    DEFINITION = I$FORTRAN$DEFINITIONS["SEQUENCE",NAME,Apply[Sequence,FilterRules[Flatten[List[OPTIONS,Options[I$FORTRAN$SEQUENCE]]],Options[I$FORTRAN$DEFINITIONS]]]] ;
    RULE = I$FORTRAN$REPLACE["SEQUENCE",NAME] ;
    PROLOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["PROLOG"]] ;
    EPILOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["EPILOG"]] ;
    ONE = Map[Composition[Function[StringTemplate[" !IO_PROLOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_PROLOG"]] ;
    TWO = Map[Composition[Function[StringTemplate[" !IO_EPILOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_EPILOG"]] ;
    CODE = I$TABLE["SEQUENCE"][NAME,"CODE","SET"] ;
    CODE = ReplaceAll[Last[CODE],RULE] ;
    CODE = Map[
      Function[
        Block[
          {MAP,FACTOR,LOCAL},
          {MAP,FACTOR} = Slot[1] ;
          FACTOR = Last[FACTOR] ;
          LOCAL = Position[I$TABLE["SEQUENCE"][NAME,"UNIQUE"],MAP] ;
          LOCAL = Flatten[LOCAL] ;
          LOCAL = First[LOCAL] ;
          StringJoin[
            StringTemplate[" CALL `1`_MAP_SUB(&\n"][MAP],
            StringTemplate["  S=`1`,&\n"][FACTOR],
            StringTemplate["  L=L_`1`,&\n"][LOCAL],
            "  F=F,&\n",
            "  X=X&\n",
            " )\n"
          ]
        ]
      ],
      CODE
    ] ;
    SUBROUTINE = StringJoin[
      StringTemplate["SUBROUTINE `1`_SEQ_SUB(S,`2`F,X)\n"][NAME,StringJoin[Map[StringTemplate["L_`1`,"],Range[Length[I$TABLE["SEQUENCE"][NAME,"UNIQUE"]]]]]],
      DEFINITION["DATA"],
      StringTemplate[" IF(X(1).LT.5.E-1_`1`) RETURN\n"][I$FORTRAN$REAL$KIND],
      StringDelete[PROLOG," \n"],
      ONE,
      CODE,
      StringDelete[EPILOG," \n"],
      TWO,
      StringTemplate["END SUBROUTINE `1`_SEQ_SUB\n"][NAME]
    ] ;
    CODE = StringJoin[
      StringTemplate["MODULE `1`_SEQ\n"][NAME],
      DEFINITION["USE"],
      " IMPLICIT NONE\n",
      StringTemplate[" PUBLIC :: `1`_SEQ_SUB\n"][NAME],
      " PRIVATE\n",
      DEFINITION["MODULE"],
      "CONTAINS\n",
      SUBROUTINE,
      StringTemplate["END MODULE `1`_SEQ\n"][NAME]
    ] ;
    CODE = ToUpperCase[CODE] ;
    FILE = StringJoin[ToLowerCase[NAME],"_seq.f90"] ;
    CODE = StringJoin[
      StringTemplate["!`1` -c `2` `3`\n"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE],
      CODE
    ] ;
    Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,FILE]],CODE,"String"] ;
    Association[
      Rule["NAME",NAME],
      Rule["TYPE","SEQUENCE"],
      Rule["SRC",StringJoin[ToLowerCase[NAME],"_seq"]],
      Rule["OBJ",StringTemplate["`1` -c `2` `3`"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE]],
      Rule[
        "USE",
        Map[StringTemplate["`1`_map"],ToLowerCase[DeleteDuplicates[Map[First,I$TABLE["SEQUENCE"][NAME,"ORDERING"]]]]]
      ]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (TEST) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$TEST] ;
I$FORTRAN$TEST::usage = "
I$FORTRAN$TEST[TEST,GLOBAL] -- convert given test WM <TEST> (single comparison) with element global constants <GLOBAL> (list of symbols) to FORTRAN
" ;
I$FORTRAN$TEST[                   (* -- CONVERT TEST *)
  TEST_,                          (* -- TEST (WM) *)
  GLOBAL_List                     (* -- GLOBAL LIST *)
] := Block[
  {HEAD,LEFT,RIGHT,RULE,VARIABLE},
  HEAD = Head[TEST] ;
  List[LEFT,RIGHT] = N[Fold[ReplaceAll,Apply[List,TEST],List[I$RULE$STATE,Thread[Rule[GLOBAL,Array[I$GLOBAL,Length[GLOBAL]]]]]],I$FORTRAN$REAL$PRECISION] ;
  RULE = Dispatch[Flatten[List[Thread[Rule[I$FLAG$PARAMETRIC,Map[StringTemplate["F(`1`)"],Range[I$DIMENSION$PARAMETRIC]]]],Rule[I$STATE,"X"],Rule[I$GLOBAL,"G"],Rule[I$PARAMETER,"S"]]]] ;
  LEFT = I$FORTRAN$SET[RULE][I$SET[1,ReplaceAll[LEFT,RuleDelayed[VARIABLE_Real,I$FORTRAN$REAL[VARIABLE]]]]] ;
  RIGHT = I$FORTRAN$SET[RULE][I$SET[1,ReplaceAll[RIGHT,RuleDelayed[VARIABLE_Real,I$FORTRAN$REAL[VARIABLE]]]]] ;
  HEAD = Which[
    SameQ[HEAD,Less],".LT.",
    SameQ[HEAD,LessEqual],".LE.",
    SameQ[HEAD,Greater],".GT.",
    SameQ[HEAD,GreaterEqual],".GE.",
    SameQ[HEAD,Equal],""
  ] ;
  ToUpperCase[StringJoin["(","(",StringDrop[LEFT,3],")",HEAD,"(",StringDrop[RIGHT,3],")",")"]]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (CHECK) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$CHECK] ;
I$FORTRAN$CHECK::usage = "
I$FORTRAN$CHECK[INDEX,TEST,GLOBAL] -- make aperture test function (FORTRAN) for given test index <INDEX> (integer), WM <TEST> list (list of single comparisons) with element global constants <GLOBAL> (list of symbols), exits if TRUE and changes flag
" ;
I$FORTRAN$CHECK[                  (* -- GENERATE APERTURE BLOCK *)
  INDEX_Integer,                  (* -- CHECK INDEX (INTEGER) *)
  TEST_List,                      (* -- LIST OF TESTS (WM) *)
  GLOBAL_List,                    (* -- GLOBAL LIST *)
  MERGE_List:List[".OR."]         (* -- MERGE LIST *)
] := Block[
  {CODE,LIST},
  CODE = Map[Curry[I$FORTRAN$TEST][GLOBAL],TEST] ;
  LIST = MERGE ;
  LIST = If[SameQ[Length[LIST],1],ConstantArray[LIST,Subtract[Length[CODE],1]],LIST] ;
  LIST = Flatten[List[LIST,""]] ;
  CODE = StringJoin[Transpose[List[CODE,LIST]]] ;
  CODE = StringJoin[
    StringTemplate["CHECK_`1`="][INDEX],
    CODE
  ] ;
  CODE = I$FORTRAN$SPLIT[CODE] ;
  StringJoin[
    StringTemplate["LOGICAL FUNCTION CHECK_`1`(G,F,X)\n"][INDEX],
    StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: G\n"][I$FORTRAN$REAL$KIND,Max[List[1,Length[GLOBAL]]]],
    StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
    StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: X\n"][I$FORTRAN$REAL$KIND,I$DIMENSION$VARIABLES],
    CODE,
    "\n",
    StringTemplate["END FUNCTION CHECK_`1`\n"][INDEX]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (ELEMENT) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$ELEMENT] ;
I$FORTRAN$ELEMENT::usage = "
I$FORTRAN$ELEMENT[NAME] -- generates FORTRAN code for element <NAME> (string)
" ;
Options[I$FORTRAN$ELEMENT] = List[
  Rule["EXTEND",0],               (* -- EXTEND STATE DIMENSION (INTEGER) *)
  Rule["USE",List[]],             (* -- USE (LIST OF STRINGS) *)
  Rule["PARAMETERS",List[]],      (* -- PARAMETERS (LIST OF STRINGS) *)
  Rule["VARIABLES",List[]],       (* -- VARIABLES (LIST OF STRINGS) *)
  Rule["PROLOG",List[""]],        (* -- PROLOG FORTRAN CODE TO EXECUTE *)
  Rule["EPILOG",List[""]],        (* -- EPILOG FORTRAN CODE TO EXECUTE *)
  Rule["IO_PROLOG",List[""]],     (* -- IO PROLOG *)
  Rule["IO_EPILOG",List[""]],     (* -- IO EPILOG *)
  Rule["IO_UNIT","*"],            (* -- IO UNIT *)
  Rule["IO_FORMAT","*"],          (* -- IO FORMAT *)
  Rule["APERTURE",Null],          (* -- APERTURE (LIST OF WM TESTS FOR EACH SEQUENCE) *)
  Rule["MODE","NORMAL"],          (* -- EVALUATION MODE (STRING), ("NORMAL" OR "ADAPTIVE") *)
  Rule["TOLERANCE",10.^-12],      (* -- ADAPTIVE TEST TOLERANCE (REAL) *)
  Rule["EXPRESSION",Null]         (* -- ADAPTIVE TEST EXPRESSION ('Null' OR LIST OF EXPRESSIONS TO EVALUATE FOR EACH SEQUENCE, E.G. LIST OF HAMILTONIANS, 'Null' VALUE CORRESPONDS TO NORM TEST) *)
] ;
I$FORTRAN$ELEMENT[                (* -- FORTRAN CODE FOR ELEMENT *)
  NAME_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {DEFINITION,RULE,PROLOG,EPILOG,ONE,TWO,WEIGHT,CODE,APERTURE,CONVERGENCE,FILE},
    DEFINITION = I$FORTRAN$DEFINITIONS["ELEMENT",NAME,Apply[Sequence,FilterRules[Flatten[List[OPTIONS,Options[I$FORTRAN$ELEMENT]]],Options[I$FORTRAN$DEFINITIONS]]]] ;
    RULE = I$FORTRAN$REPLACE["ELEMENT",NAME] ;
    PROLOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["PROLOG"]] ;
    EPILOG = Map[Composition[StringTemplate[" `1`\n"],StringTrim],OptionValue["EPILOG"]] ;
    ONE = Map[Composition[Function[StringTemplate[" !IO_PROLOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_PROLOG"]] ;
    TWO = Map[Composition[Function[StringTemplate[" !IO_EPILOG WRITE(`1`,`2`) `3`\n"][OptionValue["IO_UNIT"],OptionValue["IO_FORMAT"],Slot[1]]],StringTrim],OptionValue["IO_EPILOG"]] ;
    (* CODE *)
    WEIGHT = I$TABLE["ELEMENT"][NAME,"ORDERING"] ;
    WEIGHT = Through[Map[I$TABLE["SEQUENCE"],WEIGHT]["CODE","SET"]] ;
    WEIGHT = N[Map[Composition[Last,Transpose,Map[Apply[List]],Last,First],WEIGHT],I$FORTRAN$REAL$PRECISION] ;
    WEIGHT = Map[Function[I$FORTRAN$SET[RULE][I$SET[I$STATE[Range[Length[Slot[1]]]],Slot[1]]]],WEIGHT] ;
    WEIGHT = StringReplace[WEIGHT,Rule["X","S"]] ;
    CODE = Subtract[Map[First,Flatten[StringPosition[WEIGHT,"="],1]],1] ;
    CODE = StringTrim[MapThread[StringTake,List[WEIGHT,CODE]]] ;
    CODE = Transpose[List[WEIGHT,I$TABLE["ELEMENT"][NAME,"ORDERING"],CODE,Range[Length[WEIGHT]]]] ;
    CODE = Which[
      SameQ[OptionValue["MODE"],"NORMAL"],
      Map[
        Function[
          Block[
            {FACTOR,SEQUENCE,SUBROUTINE,INDEX,LOCAL,SET},
            {FACTOR,SEQUENCE,SUBROUTINE,INDEX} = Slot[1] ;
            LOCAL = I$TABLE["ELEMENT"][NAME,"GLOBAL"] ;
            LOCAL = Dispatch[Thread[Rule[LOCAL,Array[I$GLOBAL,Length[LOCAL]]]]] ;
            LOCAL = ReplaceAll[I$TABLE["ELEMENT"][[NAME,"RELATION",INDEX]],LOCAL] ;
            SET = Map[Function[StringTemplate["L_`1`_`2`"][INDEX,Slot[1]]],Range[Length[LOCAL]]] ;
            LOCAL = Map[Composition[I$FORTRAN$SET[RULE],Apply[I$SET]],Transpose[List[SET,LOCAL]]] ;
            LOCAL = StringReplace[LOCAL,Rule["[]",StringTemplate["[0._`1`]"][I$FORTRAN$REAL$KIND]]] ;
            LOCAL = StringRiffle[LOCAL,"\n"] ;
            SET = MapIndexed[Function[StringTemplate["   L_`1`=`2`,&\n"][First[Slot[2]],Slot[1]]],SET] ;
            StringJoin[
              LOCAL,
              "\n",
              FACTOR,
              "\n",
              StringTemplate[" `1`=`1`*STEP_LENGTH(`2`)\n"][SUBROUTINE,INDEX],
              StringTemplate[" DO I=1,NUMBER_OF_STEPS(`1`),1\n"][INDEX],
              StringTemplate["  CALL `1`_SEQ_SUB(&\n"][SEQUENCE],
              StringTemplate["   S=`1`,&\n"][SUBROUTINE],
              SET,
              "   F=F,&\n",
              "   X=X&\n",
              "  )\n",
              "  !IO_STEP WRITE(*,*) X\n",
              StringTemplate["  IF(T(`1`).AND.CHECK_`1`(G,F,X))THEN\n   X(1)=0.0_`2`\n   RETURN\n  END IF\n"][INDEX,I$FORTRAN$REAL$KIND],
              " END DO\n"
            ]
          ]
        ],
        CODE
      ],
      SameQ[OptionValue["MODE"],"ADAPTIVE"],
      Map[
        Function[
          Block[
            {FACTOR,SEQUENCE,SUBROUTINE,INDEX,LOCAL,SET},
            {FACTOR,SEQUENCE,SUBROUTINE,INDEX} = Slot[1] ;
            LOCAL = I$TABLE["ELEMENT"][NAME,"GLOBAL"] ;
            LOCAL = Dispatch[Thread[Rule[LOCAL,Array[I$GLOBAL,Length[LOCAL]]]]] ;
            LOCAL = ReplaceAll[I$TABLE["ELEMENT"][[NAME,"RELATION",INDEX]],LOCAL] ;
            SET = Map[Function[StringTemplate["L_`1`_`2`"][INDEX,Slot[1]]],Range[Length[LOCAL]]] ;
            LOCAL = Map[Composition[I$FORTRAN$SET[RULE],Apply[I$SET]],Transpose[List[SET,LOCAL]]] ;
            LOCAL = StringReplace[LOCAL,Rule["[]",StringTemplate["[0._`1`]"][I$FORTRAN$REAL$KIND]]] ;
            LOCAL = StringRiffle[LOCAL,"\n"] ;
            SET = MapIndexed[Function[StringTemplate["   L_`1`=`2`,&\n"][First[Slot[2]],Slot[1]]],SET] ;
            StringJoin[
              LOCAL,
              "\n",
              StringTemplate[" DO I=1,NUMBER_OF_STEPS(`1`),1\n"][INDEX],
              " ",
              FACTOR,
              "\n",
              StringTemplate["  `1`=`1`*STEP_LENGTH(`2`)\n"][SUBROUTINE,INDEX],
              "  J=1\n",
              "  K=0\n",
              "  THIS=X\n",
              StringTemplate["  CALL `1`_SEQ_SUB(&\n"][SEQUENCE],
              StringTemplate["   S=`1`,&\n"][SUBROUTINE],
              SET,
              "   F=F,&\n",
              "   X=THIS&\n",
              "  )\n",
              StringTemplate["  IF(T(`1`).AND.CHECK_`1`(G,F,THIS))THEN\n   THIS(1)=0.0_`2`\n   X=THIS\n   RETURN\n  END IF\n"][INDEX,I$FORTRAN$REAL$KIND],
              "  DO\n",
              "   J=2*J\n",
              "   K=K+1\n",
              "   NEXT=X\n",
              StringTemplate["   `1`=`1`/REAL(2,`2`)\n"][SUBROUTINE,I$FORTRAN$REAL$KIND],
              "   DO N=1,J,1\n",
              StringTemplate["    CALL `1`_SEQ_SUB(&\n"][SEQUENCE],
              StringTemplate["     S=`1`,&\n"][SUBROUTINE],
              Map[StringTemplate["  ``"],SET],
              "     F=F,&\n",
              "     X=NEXT&\n",
              "    )\n",
              "   END DO\n",
              StringTemplate["   !ADAPTIVE WRITE(*,*) I,J,K,CONVERGENCE_`1`(G,F,THIS,NEXT)\n"][INDEX],
              StringTemplate["   IF((CONVERGENCE_`1`(G,F,THIS,NEXT)).OR.(K.GT.64))THEN\n"][INDEX],
              "    X=NEXT\n",
              "    EXIT\n",
              "   ELSE\n",
              "    THIS=NEXT\n",
              "   END IF\n",
              "  END DO\n",
              " END DO\n"
            ]
          ]
        ],
        CODE
      ]
    ] ;
    (* APERTURE *)
    If[
      SameQ[OptionValue["APERTURE"],Null],
      APERTURE = Thread[Greater[Take[I$VARIABLES$CANONICAL,Min[List[I$DIMENSION$CANONICAL,Plus[1,1,1,1]]]],1.0]] ;
      APERTURE = Map[
        Function[I$FORTRAN$CHECK[Slot[1],APERTURE,I$TABLE["ELEMENT"][NAME,"GLOBAL"]]],
        Range[I$TABLE["ELEMENT"][NAME,"LENGTH"]]
      ],
      APERTURE = OptionValue["APERTURE"] ;
      APERTURE = If[SameQ[Head[First[APERTURE]],List],APERTURE,ConstantArray[APERTURE,I$TABLE["ELEMENT"][NAME,"LENGTH"]]] ;
      APERTURE = MapThread[
        Function[I$FORTRAN$CHECK[Slot[1],Slot[2],I$TABLE["ELEMENT"][NAME,"GLOBAL"]]],
        List[Range[I$TABLE["ELEMENT"][NAME,"LENGTH"]],APERTURE]
      ]
    ] ;
    (* CONVERGENCE *)
    CONVERGENCE = "" ;
    If[
      SameQ[OptionValue["MODE"],"ADAPTIVE"],
      If[
        SameQ[OptionValue["EXPRESSION"],Null],
        CONVERGENCE = StringTemplate[" CONVERGENCE=(MINVAL(ABS(NEXT-THIS))).LE.(`1`)\n"][I$FORTRAN$REAL[N[OptionValue["TOLERANCE"]]]] ;
        CONVERGENCE = StringJoin[
          "LOGICAL FUNCTION CONVERGENCE(G,F,THIS,NEXT)\n",
          StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: G\n"][I$FORTRAN$REAL$KIND,Max[List[1,Length[I$TABLE["ELEMENT"][NAME,"GLOBAL"]]]]],
          StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
          StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: THIS\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
          StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: NEXT\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],                    
          StringTemplate[" REAL(`1`) :: VALUE_THIS, VALUE_NEXT\n"][I$FORTRAN$REAL$KIND],
          CONVERGENCE,
          "END FUNCTION CONVERGENCE\n"
        ] ;
        CONVERGENCE = Map[
          Function[StringReplace[CONVERGENCE,Slot[1]]],
          Thread[Rule["CONVERGENCE",Map[StringTemplate["CONVERGENCE_`1`"],Range[I$TABLE["ELEMENT"][NAME,"LENGTH"]]]]]
        ],
        CONVERGENCE = OptionValue["EXPRESSION"] ;
        CONVERGENCE = If[UnsameQ[Head[CONVERGENCE],List],ConstantArray[CONVERGENCE,I$TABLE["ELEMENT"][NAME,"LENGTH"]],CONVERGENCE] ;
        CONVERGENCE = Transpose[List[Range[Length[CONVERGENCE]],CONVERGENCE]] ;
        CONVERGENCE = Map[
          Function[
            Block[
              {INDEX,EXPRESSION},
              {INDEX,EXPRESSION} = Slot[1] ;
              EXPRESSION = I$FORTRAN$TEST[Greater[EXPRESSION,0],I$TABLE["ELEMENT"][NAME,"GLOBAL"]] ;
              EXPRESSION = StringDelete[EXPRESSION,List[".GT.(0)","&"," ","\n"]] ;
              EXPRESSION = StringJoin[
                I$FORTRAN$SPLIT[StringJoin[" VALUE_THIS=",StringReplace[EXPRESSION,Rule["X","THIS"]],"\n"]],
                I$FORTRAN$SPLIT[StringJoin[" VALUE_NEXT=",StringReplace[EXPRESSION,Rule["X","NEXT"]],"\n"]],
                StringTemplate[" CONVERGENCE_`1`=(ABS(VALUE_NEXT-VALUE_THIS).LE.(`2`))\n"][INDEX,I$FORTRAN$REAL[OptionValue["TOLERANCE"]]]
              ] ;
              StringJoin[
                StringTemplate["LOGICAL FUNCTION CONVERGENCE_`1`(G,F,THIS,NEXT)\n"][INDEX],
                StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: G\n"][I$FORTRAN$REAL$KIND,Max[List[1,Length[I$TABLE["ELEMENT"][NAME,"GLOBAL"]]]]],
                StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: F\n"][I$FORTRAN$REAL$KIND,Max[List[1,I$DIMENSION$PARAMETRIC]]],
                StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: THIS\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],
                StringTemplate[" REAL(`1`),DIMENSION(`2`),INTENT(IN) :: NEXT\n"][I$FORTRAN$REAL$KIND,Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]]],                    
                StringTemplate[" REAL(`1`) :: VALUE_THIS, VALUE_NEXT\n"][I$FORTRAN$REAL$KIND],
                EXPRESSION,
                StringTemplate["END FUNCTION CONVERGENCE_`1`\n"][INDEX]
              ]
            ]
          ],
          CONVERGENCE
        ] ;
      ] ;
    ] ;
    CODE = StringJoin[
      StringTemplate["MODULE `1`_ELE\n"][NAME],
      DEFINITION["USE"],
      " IMPLICIT NONE\n",
      StringTemplate[" PUBLIC :: `1`_ELE_SUB\n"][NAME],
      " PRIVATE\n",
      DEFINITION["MODULE"],
      "CONTAINS\n",
      StringTemplate["SUBROUTINE `1`_ELE_SUB(T,MAX_STEP_LENGTH,LENGTH,G,F,X)\n"][NAME],
      DEFINITION["DATA"],
      StringTemplate[" IF(X(1).LT.5.E-1_`1`) RETURN\n"][I$FORTRAN$REAL$KIND],
      " NUMBER_OF_STEPS=CEILING(ABS(LENGTH)/MAX_STEP_LENGTH)\n",
      StringTemplate[" STEP_LENGTH=LENGTH/REAL(NUMBER_OF_STEPS,`1`)\n"][I$FORTRAN$REAL$KIND],
      StringDelete[PROLOG," \n"],
      ONE,
      CODE,
      StringDelete[EPILOG," \n"],
      TWO,
      StringTemplate["END SUBROUTINE `1`_ELE_SUB\n"][NAME],
      APERTURE,
      CONVERGENCE,
      StringTemplate["END MODULE `1`_ELE\n"][NAME]
    ] ;
    FILE = StringJoin[ToLowerCase[NAME],"_ele.f90"] ;
    CODE = StringJoin[
      StringTemplate["!`1` -c `2` `3`\n"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE],
      CODE
    ] ;
    Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,FILE]],CODE,"String"] ;
    Association[
      Rule["NAME",NAME],
      Rule["TYPE","ELEMENT"],
      Rule["SRC",StringJoin[ToLowerCase[NAME],"_ele"]],
      Rule["OBJ",StringTemplate["`1` -c `2` `3`"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,FILE]],
      Rule[
        "USE",
        Flatten[
          List[
            Map[StringTemplate["`1`_map"],ToLowerCase[DeleteDuplicates[Flatten[Map[Map[First],Through[Map[I$TABLE["SEQUENCE"],DeleteDuplicates[I$TABLE["ELEMENT"][NAME,"ORDERING"]]]["ORDERING"]]]]]]],
            Map[StringTemplate["`1`_seq"],ToLowerCase[DeleteDuplicates[I$TABLE["ELEMENT"][NAME,"ORDERING"]]]]
          ]
        ]
      ]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (NORMALIZE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$NORMALIZE] ;
I$FORTRAN$NORMALIZE::usage = "
I$FORTRAN$NORMALIZE[] -- generate FORTRAN code for normalization of 1st order derivatives (used for chaos indicators, system should be 1st order in phase space and zero order in parameters)
" ;
I$FORTRAN$NORMALIZE[              (* -- NORMALIZATION OF 1ST ORDER DERIVATIVES *)
] := Catch[
  Block[
    {LIST,VARIABLE,NORMAL},
    LIST = TakeList[I$ORDER$LIST,List[I$DIMENSION$CANONICAL,I$DIMENSION$PARAMETRIC]] ;
    LIST = Map[DeleteDuplicates,LIST] ;
    VARIABLE = Partition[ReplaceAll[I$VARIABLES$DERIVATIVE,I$RULE$STATE],I$DIMENSION$CANONICAL] ;
    VARIABLE = ReplaceAll[VARIABLE,Rule[I$STATE,Identity]] ;
    VARIABLE = StringDelete[Map[StringTemplate["X([`1`])"],VARIABLE],List["{","}"]] ;
    VARIABLE = Map[I$FORTRAN$ARGUMENT,VARIABLE] ;
    NORMAL = Map[StringTemplate["NORM2(`1`)"],VARIABLE] ;
    VARIABLE = Map[Function[StringJoin[Slot[1],"/"]],VARIABLE] ;
    NORMAL = Map[StringJoin,Transpose[List[VARIABLE,NORMAL]]] ;
    VARIABLE = StringReplace[VARIABLE,Rule["/","="]] ;
    NORMAL = Map[StringJoin,Transpose[List[VARIABLE,NORMAL]]] ;
    NORMAL = StringDelete[NORMAL,List[" ","&","\n"]] ;
    NORMAL = Map[I$FORTRAN$LINE,NORMAL] ;
    NORMAL = StringJoin[Flatten[Transpose[List[NORMAL,ConstantArray["\n",Length[NORMAL]]]]]] ;
    List["FORTRAN",NORMAL]
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (LATTICE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$LATTICE] ;
I$FORTRAN$LATTICE::usage = "
I$FORTRAN$LATTICE[NAME,LATTICE] -- generate FORTRAN lattice for given integrator signature lattice data <LATTICE> =  {...,{<NAME_I>,<TEST_I>,<MAX_I>,<LENGTH_I>,<GLOBAL_I>,<FLAG_I>},...} 
" ;
Options[I$FORTRAN$LATTICE] = List[
  Rule["DEFINITIONS",""],         (* -- ADDITIONAL DEFINITIONS AND/OR CODE (INSERTED AFTER DEFINITIONS) *)
  Rule["EXTEND",0],               (* -- ADD EXTRA DIMENSIONS TO STATE *)
  Rule["MAP",I$PARALLEL$MAP],     (* -- MAP FUNCTION *)
  Rule["VERBOSE",False]           (* -- VERBOSE FLAG *)
] ;
I$FORTRAN$LATTICE[                (* -- GENERATE FORTRAN LATTICE FILE *)
  NAME_String,                    (* -- FILE/LATTICE NAME (WITHOUT EXTENSION) *)
  LATTICE_List,                   (* -- LATTICE DATA, {...,{<NAME_I>,<TEST_I>,<MAX_I>,<LENGTH_I>,<GLOBAL_I>,<FLAG_I>},...} *)
                                  (* NAME_I   -- ELEMENT/FILE NAME (STRING) *)
                                  (* TEST_I   -- {...,TEST_I_J,...},   TEST_I_J   = 0/1,  TURN OF/ON AREPTURE TEST IN SEQUENCE J IN ELEMENT I*)
                                  (* STEP_I   -- {...,STEP_I_J,...},   STEP_I_J   = REAL, MAXIMUM INTEGRATION STEP LENGTH FOR SEQUENCE J IN ELEMENT I *)
                                  (* LENGTH_I -- {...,LENGTH_I_J,...}, LENGTH_I_J = REAL, INTEGRATION LENGTH FOR SEQUENCE J IN ELEMENT I *)
                                  (* GLOBAL_I -- {...,GLOBAL_I_J,...}, GLOBAL_I_J = REAL, GLOBAL PARAMETERS FOR ELEMENT I *)
                                  (* FLAG_I   -- {...,FLAG_I_J},...},  FLAG_I_J   = REAL, FLAG VALUES FOR KNOBS *)
                                  (* NUMBER OF INTEGRATIONS FOR A SEQUENCE IS ABS(CEILING(LENGTH_I/MAX_I)) *)
                                  (* INTEGRATION STEP LENGTH IS LENGTH_I/ABS(CEILING(LENGTH_I/MAX_I)) *)
                                  (* TO GET ONE INTEGRATION STEP USE MAX_I_J >= LENGTH_I_J *)
                                  (* GLOBAL_I -- LIST OF GENERIC PARAMETERS (ALL GLOBAL PARAMETERS SHOULD BE SPECIFIED, USE ZEROS FOR UNUSED PARAMETERS) *)
                                  (* IF NAME IS "FORTRAN", THEN THE ENTRY SHOULD BE OF THE FORM List["FORTRAN","CODE"] *)
                                  (* WHERE "CODE" IS A STRING WITH VALID FORTRAN CODE *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {DIMENSION,CODE,POSITION,DATA,LIST,USE,CALL,SUBROUTINE},
    If[
      OptionValue["VERBOSE"],
      Print[StringTemplate["SIGNATURE          : `1`"][Flatten[List[Last[StringSplit[First[I$SIGNATURE],"/"]],Rest[I$SIGNATURE]]]]] ;
      Print[StringTemplate["USER OPTIONS       : `1`"][List[OPTIONS]]] ;
    ] ;
    (* SET STATE DIMENSION (ADD EXTRA TO DIMENSIONS) *)
    DIMENSION = Plus[I$DIMENSION$VARIABLES,OptionValue["EXTEND"]] ;
    If[
      OptionValue["VERBOSE"],
      Print[StringTemplate["DIMENSION          : `1`"][DIMENSION]] ;
    ] ;
    (* EXTRACT FORTRAN CODE AND SET CODE POSITIONS *)
    CODE = MapAt[ToUpperCase,LATTICE,List[All,1]] ;
    CODE = Cases[Transpose[List[Range[Length[CODE]],CODE]],List[_,List["FORTRAN",___]]] ;
    List[POSITION,CODE] = If[UnsameQ[CODE,List[]],Transpose[CODE],List[List[],List[]]] ;
    If[
      OptionValue["VERBOSE"],
      Print[StringTemplate["FORTRAN            : `1`"][Length[CODE]]] ;
    ] ;
    (* REMOVE FORTRAN CODE *)
    DATA = DeleteCases[LATTICE,Alternatives[List["FORTRAN",___],List["fortran",___]]] ;
    If[
      OptionValue["VERBOSE"],
      Print[StringTemplate["ELEMENTS           : `1`"][Length[DATA]]] ;
    ] ;
    (* SET LIST OF UNIQUE ELEMENTS *)
    LIST = Sort[DeleteDuplicates[Map[First,DATA]]] ;
    If[
      OptionValue["VERBOSE"],
      Print[StringTemplate["UNIQUE             : `1`"][Length[LIST]]] ;
    ] ;
    (* GENERATE USE STATEMENTS *)
    If[
      OptionValue["VERBOSE"],
      Print["GENERATE USE STATEMENTS..."] ;
    ] ;
    USE = Map[StringTemplate[" USE `1`_ELE, ONLY: `1`_ELE_SUB\n"],LIST] ;
    (* GENRATE CALLS *)
    If[
      OptionValue["VERBOSE"],
      Print["GENERATE ELEMENT CALLS..."] ;
    ] ;
    CALL = OptionValue["MAP"][
      Function[
        Block[
          {ELEMENT,FLAG,LENGTH,RUN,PARAMETER,RULE,RESULT},
          {ELEMENT,FLAG,LENGTH,RUN,PARAMETER,RULE} = Slot[1] ;
          RULE = If[SameQ[RULE,List[]],ConstantArray[1.,I$DIMENSION$PARAMETRIC],RULE] ;
          FLAG = Map[Function[If[SameQ[Slot[1],1],".TRUE.",".FALSE."]],FLAG] ;
          FLAG = StringDelete[StringReplace[ToString[FLAG],I$RULE$FORTRAN]," "] ;
          LENGTH = StringDelete[StringReplace[ToString[Map[I$FORTRAN$REAL,N[LENGTH,I$FORTRAN$REAL$PRECISION]]],I$RULE$FORTRAN]," "] ;
          RUN = StringDelete[StringReplace[ToString[Map[I$FORTRAN$REAL,N[RUN,I$FORTRAN$REAL$PRECISION]]],I$RULE$FORTRAN]," "] ;
          PARAMETER = StringDelete[StringReplace[ToString[Map[I$FORTRAN$REAL,N[PARAMETER,I$FORTRAN$REAL$PRECISION]]],I$RULE$FORTRAN]," "] ;
          RULE = StringDelete[StringReplace[ToString[Map[I$FORTRAN$REAL,N[RULE,I$FORTRAN$REAL$PRECISION]]],I$RULE$FORTRAN]," "] ;
          (*
          FLAG = StringJoin[StringJoin["T=",FLAG,","],"&\n"] ;
          FLAG = I$FORTRAN$SPLIT[FLAG] ;
          LENGTH = StringJoin[StringJoin["MAX_STEP_LENGTH=",LENGTH,","],"&\n"] ;
          LENGTH = I$FORTRAN$SPLIT[LENGTH] ;
          RUN = StringJoin[StringJoin["LENGTH=",RUN,","],"&\n"] ;
          RUN = I$FORTRAN$SPLIT[RUN] ;
          PARAMETER = StringJoin[StringJoin["G=",PARAMETER,","],"&\n"] ;
          PARAMETER = I$FORTRAN$SPLIT[PARAMETER] ;
          RULE = StringJoin[StringJoin["F=",RULE,","],"&\n"] ;
          RULE = I$FORTRAN$SPLIT[RULE] ;
          *)
          FLAG = StringJoin["T=",FLAG,","] ;
          FLAG = I$FORTRAN$SPLIT[FLAG] ;
          FLAG = StringJoin[FLAG,"&\n"] ;
          LENGTH = StringJoin["MAX_STEP_LENGTH=",LENGTH,","] ;
          LENGTH = I$FORTRAN$SPLIT[LENGTH] ;
          LENGTH = StringJoin[LENGTH,"&\n"] ;
          RUN = StringJoin["LENGTH=",RUN,","] ;
          RUN = I$FORTRAN$SPLIT[RUN] ;
          RUN = StringJoin[RUN,"&\n"] ;
          PARAMETER = StringJoin["G=",PARAMETER,","] ;
          PARAMETER = I$FORTRAN$SPLIT[PARAMETER] ;
          PARAMETER = StringJoin[PARAMETER,"&\n"] ;
          RULE = StringJoin["F=",RULE,","] ;
          RULE = I$FORTRAN$SPLIT[RULE] ;
          RULE = StringJoin[RULE,"&\n"] ;
          RESULT = StringJoin[
            " ",
            StringDelete[StringTemplate[" !`1`\n"][Slot[1]]," "],
            StringTemplate[" CALL `1`_ELE_SUB(&\n"][ELEMENT],
            " ",FLAG,
            " ",LENGTH,
            " ",RUN,
            " ",PARAMETER,
            " ",RULE,
            "  X=X&\n",
            " )\n"
          ] ;
          StringReplace[RESULT,Rule["[]",StringTemplate["[0._`1`]"][I$FORTRAN$REAL$KIND]]]
        ]
      ],
      DATA
    ] ;
    (* INSERT FORTRAN CODE *)
    CALL = I$INSERT[CALL,Map[Composition[Last,ToUpperCase],CODE],POSITION] ;
    If[
      OptionValue["VERBOSE"],
      Print["GENERATE LATTICE SUBROUTINE..."] ;
    ] ;
    (* DEFINE SUBROUTINE *)
    SUBROUTINE = StringJoin[
      StringTemplate["SUBROUTINE `1`_LAT_SUB(X) &\n"][NAME],
      StringTemplate[" BIND(C, NAME = \"`1`_lat_sub\")\n"][ToLowerCase[NAME]],
      " USE ISO_C_BINDING\n",
      USE,
      " IMPLICIT NONE\n",
      StringTemplate[" INTEGER,PARAMETER :: `1`=`2`\n"][I$FORTRAN$REAL$KIND,I$FORTRAN$REAL$KIND$VALUE],
      StringTemplate[" REAL(`1`),DIMENSION(`2`), INTENT(INOUT) :: X\n"][I$FORTRAN$REAL$KIND,DIMENSION],
      ToUpperCase[OptionValue["DEFINITIONS"]],
      "",
      StringTemplate[" IF(X(1).LT.5.E-1_`1`)THEN\n  RETURN\n ENDIF\n"][I$FORTRAN$REAL$KIND],
      CALL,
      StringTemplate["END SUBROUTINE `1`_LAT_SUB\n"][NAME]
    ] ;
    (* EXPORT FILE *)
    If[
      OptionValue["VERBOSE"],
      Print["EXPORT LATTICE FILE..."] ;
    ] ;
    Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,StringTemplate["`1`_lat.f90"][ToLowerCase[NAME]]]],SUBROUTINE,"String"] ;
    (* RETURN *)
    If[
      OptionValue["VERBOSE"],
      Print["ALL DONE..."] ;
    ] ;
    Association[
      Rule["NAME",ToUpperCase[NAME]],
      Rule["TYPE","LATTICE"],
      Rule["LENGTH",Length[LATTICE]],
      Rule["SRC",StringJoin[ToLowerCase[NAME],"_lat"]],
      Rule["OBJ",StringTemplate["`1` -c `2` `3`_lat.f90"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,ToLowerCase[NAME]]],
      Rule[
        "USE",
        Flatten[
          List[
            Map[StringTemplate["`1`_map"],ToLowerCase[DeleteDuplicates[Flatten[Map[Map[First],Through[Map[I$TABLE["SEQUENCE"],DeleteDuplicates[Flatten[Through[Map[I$TABLE["ELEMENT"],DeleteCases[DeleteDuplicates[Map[First,LATTICE]],"FORTRAN"]]["ORDERING"]]]]]["ORDERING"]]]]]]],
            Map[StringTemplate["`1`_seq"],ToLowerCase[DeleteDuplicates[Flatten[Through[Map[I$TABLE["ELEMENT"],DeleteCases[DeleteDuplicates[Map[First,LATTICE]],"FORTRAN"]]["ORDERING"]]]]]],
            Map[StringTemplate["`1`_ele"],ToLowerCase[DeleteCases[DeleteDuplicates[Map[First,LATTICE]],"FORTRAN"]]]
          ]
        ]
      ]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(*  FORTRAN (LIBRARY)  *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$LIBRARY] ;
I$FORTRAN$LIBRARY::usage = "
I$FORTRAN$LIBRARY[LATTICE] -- create library from lattice data <LATTICE> (output of I$FORTRAN$LATTICE[])
" ;
I$FORTRAN$LIBRARY[                (* -- CREATE LIBRARY *)
  LATTICE_Association             (* -- LATTICE DATA (I$FORTRAN$LATTICE OUTPUT) *)
] := Catch[
  If[Not[Apply[And,Map[Composition[FileExistsQ,StringTemplate["`1`.o"]],LATTICE["USE"]]]],Throw[$Failed]] ;
  Block[
    {WRAPPER,OBJECT,LIBRARY},
    (* WRAPPER *)
    WRAPPER = StringTemplate[
    "
      //wrapper.cc
      #include \"WolframLibrary.h\"
      #include \"WolframCompileLibrary.h\"
      DLLEXPORT mint WolframLibrary_getVersion(){ return WolframLibraryVersion ; }
      DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData){ return 0 ; }
      extern \"C\" {void `1`(double*) ;}
      EXTERN_C DLLEXPORT int TRACK_`1`(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res){
        MTensor input = MArgument_getMTensor(Args[0]) ;
        MTensor state = 0 ;
        libData->MTensor_clone(input,&state);
        `1`(MTensor_getRealDataMacro(state)) ;
        MArgument_setMTensor(Res,state) ;
        return LIBRARY_NO_ERROR ;
      }
    "
    ][StringJoin[LATTICE["SRC"],"_sub"]] ;
    Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,"wrapper.cc"]],WRAPPER,"List"] ;
    (* OBJECTS *)
    OBJECT = Map[StringTemplate["`1`.o"],Flatten[List[LATTICE["USE"],LATTICE["SRC"]]]] ;
    (* LIBRARY *)
    LIBRARY = Flatten[List["wrapper.cc",OBJECT]] ;
    LIBRARY = CreateLibrary[
      LIBRARY,
      LATTICE["SRC"],
      Rule["CleanIntermediate",True],
      Rule["CompileOptions",I$C$FLAGS],
      Rule["Compiler",Automatic],
      Rule["TargetDirectory",I$FORTRAN$DIRECTORY],
      Rule["Debug",False]
    ] ;
  ]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (LOAD) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$LOAD] ;
I$FORTRAN$LOAD::usage = "
I$FORTRAN$LOAD[FILE,NAME] -- load FORTRAN library <FILE> to name <NAME>
" ;
I$FORTRAN$LOAD[                   (* -- LOAD LIBRARY *)
  FILE_,                          (* -- LIBRARY FILE NAME (WITHOUT EXTENCION) *)
  NAME_                           (* -- LOAD TO NAME *)
] := Block[
  List[],
  If[SameQ[Head[NAME],LibraryFunction],LibraryFunctionUnload[NAME]] ;
  LibraryFunctionLoad[FileNameJoin[List[I$FORTRAN$DIRECTORY,FILE]],StringJoin["TRACK_",FILE,"_sub"],List[List[Real,1,Automatic]],List[Real,1]]
] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (INITIAL) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$INITIAL] ;
I$FORTRAN$INITIAL::usage = "
I$FORTRAN$INITIAL[INITIAL] -- convert given state initial <INITIAL> to FORTRAN form
" ;
I$FORTRAN$INITIAL[
  INITIAL_
] := I$FORTRAN$SPLIT[StringDelete[ToUpperCase[StringJoin[" X=[",StringTake[I$FORTRAN$LINE[INITIAL],List[7,-2]],"]"]],List[" ","&","\n"]]] ;
(* ################################################################################################################################################################ *)
(* FORTRAN (PROGRAM) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FORTRAN$PROGRAM] ;
I$FORTRAN$PROGRAM::usage = "
I$FORTRAN$PROGRAM[NAME,LATTICE,INITIAL] -- generate standalone FORTRAN program <NAME> (string) from lattice data <LATTICE> with initial condition <INITIAL> (use 'Null' for command line input)
" ;
Options[I$FORTRAN$PROGRAM] = List[
  Rule["DEFINITIONS",""],         (* -- ADD EXTRA DEFINITIONS *)
  Rule["PROLOG",""],              (* -- CODE TO EXECUTE BEFORE MAIN CALL *)
  Rule["EPILOG",""]               (* -- CODE TO EXECUTE AFTER MAIN CALL *)
] ;
I$FORTRAN$PROGRAM[                (* -- GENERATE FORTRAN PROGRAM *)
  NAME_String,                    (* -- PROGRAM FILE NAME (WITHOUT EXTENSION) *)
  LATTICE_Association,            (* -- LATTICE DATA (I$FORTRAN$LATTICE OUTPUT) *)
  INITIAL_,                       (* -- INITIAL CONDITION (USE 'Null' FOR COMMAND LINE INPUT) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Block[
  {SUBROUTINE,POSITION,DEFINITION,FLAG,DIMENSION,STATE,DATA},
  (* IMPORT MAIN SUBROUTINE *)
  SUBROUTINE = LATTICE["SRC"] ;
  SUBROUTINE = Import[FileNameJoin[List[I$FORTRAN$DIRECTORY,StringJoin[SUBROUTINE,".f90"]]],"List"] ;
  (* EXTRACT DEFINITIONS, APERTURE AND DIMENSION *)
  POSITION = First[Flatten[Position[SUBROUTINE,ToUpperCase[" IMPLICIT NONE"]]]] ;
  DEFINITION = Take[SUBROUTINE,List[POSITION,-1]] ;
  POSITION = First[Flatten[Position[DEFINITION,StringTemplate[" IF(X(1).LT.5.E-1_``)THEN"][I$FORTRAN$REAL$KIND]]]] ;
  FLAG = Take[DEFINITION,List[POSITION,Plus[1,1,POSITION]]] ;
  FLAG = Map[Function[StringJoin[Slot[1],"\n"]],FLAG] ;
  DEFINITION = Take[DEFINITION,List[1,Subtract[POSITION,1]]] ;
  DEFINITION = StringDelete[DEFINITION,",INTENT(INOUT)"] ;
  DIMENSION = First[StringCases[First[Drop[DEFINITION,2]],DigitCharacter..]] ;
  DEFINITION = Map[Function[StringJoin[Slot[1],"\n"]],DEFINITION] ;
  DEFINITION = StringJoin[Take[DEFINITION,Plus[1,1,1]]] ;
  DEFINITION = StringJoin[
    StringTemplate[" USE LATTICE, ONLY: `1`_SUB\n"][ToUpperCase[LATTICE["SRC"]]],
    DEFINITION
  ] ;
  DEFINITION = StringDelete[DEFINITION,", INTENT(INOUT)"] ;
  (* ADD EXTRA DEFINITIONS *)
  DEFINITION = Flatten[List[DEFINITION,OptionValue["DEFINITIONS"]]] ;
  (* GENERATE INITIAL CONDITION *)
  STATE = If[
    SameQ[INITIAL,Null],
    StringJoin[
      " INTEGER::ARG_IND,ARG_NUM\n",
      StringTemplate[" CHARACTER(LEN=100),DIMENSION(`1`)::ARG_VAL\n"][DIMENSION],
      " INTEGER::I\n",
      " ARG_NUM = COMMAND_ARGUMENT_COUNT()\n",
      StringTemplate[" IF(ARG_NUM.NE.`1`) STOP\n"][DIMENSION],
      " DO ARG_IND=1,ARG_NUM,1\n",
      "  CALL GET_COMMAND_ARGUMENT(ARG_IND,ARG_VAL(ARG_IND))\n",
      "  READ(ARG_VAL(ARG_IND),*) X(ARG_IND)\n",
      " END DO"
    ],
    I$FORTRAN$INITIAL[N[INITIAL,I$FORTRAN$REAL$PRECISION]]
  ] ;
  STATE = StringJoin[
    STATE,
    "\n !IO_DATA OPEN(UNIT=10,FILE='OUTPUT',STATUS='UNKNOWN',FORM='FORMATTED')\n"
  ] ;
  (* BUILD PROGRAM *)
  DATA = StringJoin[
    "MODULE LATTICE\n",
    " IMPLICIT NONE\n",
    " PRIVATE\n",
    StringTemplate[" PUBLIC :: ``_SUB\n"][ToUpperCase[LATTICE["SRC"]]],
    "CONTAINS\n",
    StringTemplate[" INCLUDE \"`1`.f90\"\n"][ToLowerCase[LATTICE["SRC"]]],
    "END MODULE LATTICE\n",
    StringTemplate["PROGRAM `1`\n"][ToUpperCase[NAME]],
    " USE ISO_C_BINDING\n",
    DEFINITION,
    STATE,
    OptionValue["PROLOG"],
    StringTemplate["  CALL `1`_SUB(X)\n"][ToUpperCase[LATTICE["SRC"]]],
    OptionValue["EPILOG"],
    " !IO_DATA CLOSE(10)\n",
    StringTemplate["END PROGRAM `1` \n"][ToUpperCase[NAME]]
  ] ;
  Export[FileNameJoin[List[I$FORTRAN$DIRECTORY,StringTemplate["`1`.f90"][NAME]]],DATA,"String"] ;
  StringJoin[
    StringTemplate["`1` -c `2` `3`"][I$FORTRAN$COMPILER,I$FORTRAN$FLAGS,StringTemplate["`1`.f90"][NAME]],
    " && ",
    StringTemplate["`1` -o `2` `3` `4`"][I$FORTRAN$COMPILER,NAME,I$FORTRAN$FLAGS,StringJoin[Map[StringTemplate["`1`.o "],Flatten[List[LATTICE["USE"],NAME]]]]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* STRAIGHT MULTIPOLE VECTOR POTENTIAL (2D) *)
(* ################################################################################################################################################################ *)
(* NOTE: STRAIGHT MULTIPOLE VECTOR POTENTIAL IS MAXWELLIAN *)
(*       ONLY 3RD COMPONENT IS NOT EQUAL TO ZERO *)
(*       APPEARS IN HAMILTONIAN WITH MINUS SIGN *)
(*       MULTIPOLE COEFFICIENTS <KSL> = {KSL[0],KSL[1],KSL[2],KSL[3],...} (S-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B1[Q1,0,Q3] = KSL[0] + KSL[1] Q1 + KSL[2] Q1^2 + KSL[3] Q1^3 + ... (NO FACTORIAL) *)
(*       MULTIPOLE COEFFICIENTS <KNL> = {KNL[0],KNL[1],KNL[2],KNL[3],...} (N-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B2[Q1,0,Q3] = KNL[0] + KNL[1] Q1 + KNL[2] Q1^2 + KNL[3] Q1^3 + ... (NO FACTORIAL) *)
(* ################################################################################################################################################################ *)
ClearAll[I$POTENTIAL$PLANE] ;
I$POTENTIAL$PLANE::usage="
I$POTENTIAL$PLANE[ORDER,KNL,KSL] -- straight multipole vector potential (single multipole) for given multipole order <ORDER> (integer), n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol)
I$POTENTIAL$PLANE[KNL,KSL] -- straight multipole vector potential (composition of multipoles) for given list of n-amplitudes <KNL> (list) and list of s-amplitudes <KSL> (list)
I$POTENTIAL$PLANE[ORDER,MAX,KNL,KSL] -- (Forest) s-dependent straight multipole vector potential of order <ORDER> (integer) summed up to <MAX> (integer) number of terms, n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol)
" ;
I$POTENTIAL$PLANE[                (* -- STRAIGHT MULTIPOLE POTENTIAL (SINGLE MULTUPOLE) *)
  ORDER_Integer,                  (* -- MULTIPOLE ORDER (INTEGER) *)
  KNL_,                           (* -- N-AMPLITUDE (SYMBOL OR NUMBER) *)
  KSL_                            (* -- S-AMPLITUDE (SYMBOL OR NUMBER) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,NORMAL,SKEW,POTENTIAL},
  List[Q1,P1,Q2,P2,Q3,P3] = CANONICAL ;
  POTENTIAL = ReplaceAll[ComplexExpand[Times[Power[ORDER,-1],Power[Plus[Q1,Times[Complex[0,1],Q2]],ORDER],Plus[NORMAL,Times[Complex[0,1],SKEW]]]],Rule[I,0]] ;
  List[
    0,
    0,
    Times[Subtract[0,1],ReplaceAll[Total[MonomialList[POTENTIAL,List[NORMAL,SKEW]]],Thread[Rule[List[NORMAL,SKEW],List[KNL,KSL]]]]]
  ]
] /; And[
  Greater[ORDER,0],
  SameQ[Length[CANONICAL],Times[Plus[1,1,1],Plus[1,1]]]
] ;
I$POTENTIAL$PLANE[                (* -- STRAIGHT MULTIPOLE POTENTIAL (COMPOSITION OF SEVERAL MULTUPOLES) *)
  KNL_List,                       (* -- N-AMPLITUDES (LIST OR EMPTY LIST) *)
  KSL_List                        (* -- S-AMPLITUDES (LIST OR EMPTY LIST) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {MAX,NORMAL,SKEW,POTENTIAL},
  MAX = Max[Map[Length,List[KNL,KSL]]] ;
  POTENTIAL = Total[Map[Function[I$POTENTIAL$PLANE[Slot[1],NORMAL[Slot[1]],SKEW[Slot[1]]][CANONICAL]],Range[MAX]]] ;
  Fold[
    ReplaceAll,
    POTENTIAL,
    List[
      Thread[Rule[Map[NORMAL,Range[MAX]],PadRight[KNL,MAX]]],
      Thread[Rule[Map[SKEW,Range[MAX]],PadRight[KSL,MAX]]]
    ]
  ]
] ;
I$POTENTIAL$PLANE[                (* -- S-DEPENDENT STRAIGHT MULTIPOLE POTENTIAN *)
  ORDER_Integer,                  (* -- MULTIPOLE ORDER (INTEGER) *)
  MAX_Integer,                    (* -- MAXIMUM NUMBER OF TERMS (INTEGER) *)
  KNL_,                           (* -- N-AMPLITUDE *)
  KSL_                            (* -- S-AMPLITUDE *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,ANGLE,RADIUS,INDEX,PSI,AR,AX,AY,AZ,RULE},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  PSI = -I/ORDER*Sum[D[I*KSL[I$PARAMETER]+KNL[I$PARAMETER],List[I$PARAMETER,2*INDEX]]*(-1)^INDEX*Factorial[ORDER]*RADIUS^(2*INDEX+ORDER)/(2^(2*INDEX)*Factorial[INDEX]*Factorial[INDEX+ORDER]),List[INDEX,0,MAX]] ;
  AR = I*RADIUS/ORDER*D[PSI*Exp[I*ORDER*ANGLE],I$PARAMETER] ;
  AX = AR*Cos[ANGLE] ;
  AY = AR*Sin[ANGLE] ;
  AZ = -I*RADIUS/ORDER*D[PSI*Exp[I*ORDER*ANGLE],RADIUS] ;
  RULE = List[
    Rule[Cos[ANGLE],Q1/RADIUS],
    Rule[Sin[ANGLE],Q2/RADIUS],
    Rule[RADIUS,Sqrt[Q1^2+Q2^2]]
  ] ;
  AX = Simplify[ReplaceRepeated[TrigExpand[ComplexExpand[Re[ExpToTrig[AX]]]],RULE]] ;
  AY = Simplify[ReplaceRepeated[TrigExpand[ComplexExpand[Re[ExpToTrig[AY]]]],RULE]] ;
  AZ = Simplify[ReplaceRepeated[TrigExpand[ComplexExpand[Re[ExpToTrig[AZ]]]],RULE]] ;
  Collect[List[AX,AY,AZ],List[KNL[_],KSL[_],Derivative[_][KNL][_],Derivative[_][KSL][_]],Expand]
] /; And[
  GreaterEqual[ORDER,Plus[1,1]],
  GreaterEqual[MAX,0]
] ;
(* ################################################################################################################################################################ *)
(* STRAIGHT MULTIPOLE VECTOR POTENTIAL (3D) *)
(* ################################################################################################################################################################ *)
ClearAll[I$POTENTIAL$PLANE$3D] ;
I$POTENTIAL$PLANE$3D::usage = "
I$POTENTIAL$PLANE$3D[MIN,MAX,KNL,KSL] -- generate s-dependent vector potential for multipole with leading degree <MIN> (integer) up to degree <MAX> (integer) with n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol)
" ;
I$POTENTIAL$PLANE$3D[             (* -- MULTIPOLE VECTOR POTENTIAL *)
  MIN_,                           (* -- LEADING ORDER DEGREE (INTEGER) *)
  MAX_,                           (* -- TRUNCATION DEGREE (INTEGER) *)
  KNL_,                           (* -- N-AMPLITUDE (SYMBOL) *)
  KSL_                            (* -- S-AMPLITUDE (SYMBOL) *)
][
  CANONICAL_                      (* -- LIST OF CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,ORDER,RADIUS,ANGLE,FUNCTION,PSI,A$R,A$X,A$Y,A$Z},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  ORDER = Subtract[MIN,Plus[1,1]] ;
  FUNCTION = Function[
    Times[
      D[Plus[KNL[I$PARAMETER],Times[I,KSL[I$PARAMETER]]],List[I$PARAMETER,Slot[1]]],
	  Divide[Subtract[0,I],MIN],
	  Power[Subtract[0,1],Slot[1]],
	  Factorial[MIN],
	  Divide[1,Power[Plus[1,1],Times[Plus[1,1],Slot[1]]]],
	  Divide[1,Factorial[Slot[1]]],
	  Divide[1,Factorial[Plus[Slot[1],MIN]]],
	  Power[RADIUS,Plus[Times[Plus[1,1],Slot[1]],MIN]]
    ]
  ] ;
  PSI = Total[Map[FUNCTION,Range[0,MAX]]] ;
  A$R = ReplaceAll[ComplexExpand[I*RADIUS/MIN*D[PSI,I$PARAMETER]*Exp[I*MIN*ANGLE]],Rule[I,0]] ;
  A$X = TrigExpand[A$R*Cos[ANGLE]] /. Cos[ANGLE] -> Q1/RADIUS /. Sin[ANGLE] -> Q2/RADIUS /. RADIUS -> Sqrt[Q1^2+Q2^2] ;
  A$X = Collect[A$X,Derivative[_][__][__],Expand] ;
  A$Y = TrigExpand[A$R*Sin[ANGLE]] /. Cos[ANGLE] -> Q1/RADIUS /. Sin[ANGLE] -> Q2/RADIUS /. RADIUS -> Sqrt[Q1^2+Q2^2] ;
  A$Y = Collect[A$Y,Derivative[_][__][__],Expand] ;
  A$Z = ReplaceAll[ComplexExpand[-I*RADIUS/MIN*D[PSI,RADIUS]*Exp[I*MIN*ANGLE]],Rule[I,0]] ;
  A$Z = TrigExpand[A$Z] /. Cos[ANGLE] -> Q1/RADIUS /. Sin[ANGLE] -> Q2/RADIUS /. RADIUS -> Sqrt[Q1^2+Q2^2] ;
  A$Z = Collect[A$Z,Derivative[_][__][__],Expand] ;
  A$Z = Collect[A$Z,List[KNL[_],KSL[_]],Expand] ;
  List[A$X,A$Y,A$Z]
] ;
(* ################################################################################################################################################################ *)
(* GRADIENT PROFILE (HYPERTAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$GRADIENT$PROFILE] ;
I$GRADIENT$PROFILE::usage = "
I$GRADIENT$PROFILE[LENGTH,CENTER,STIFFNESS] -- generate hypertan gradient profile for given effective length <LENGHT> (real), element center <CENTER> (real) and profile stiffness <STIFFNESS> (real)
" ;
I$GRADIENT$PROFILE[               (* -- FIELD GRAIENT PROFILE (HYPERTAN) *)
  LENGTH_,                        (* -- MAGNETIC LENGTH *)
  CENTER_,                        (* -- CENTER POSITION *)
  STIFFNESS_                      (* -- STIFFNESS *)
] := Divide[
  Subtract[
    Tanh[Times[STIFFNESS,Subtract[I$PARAMETER,Subtract[CENTER,Divide[LENGTH,Plus[1,1]]]]]],
    Tanh[Times[STIFFNESS,Subtract[I$PARAMETER,Plus[CENTER,Divide[LENGTH,Plus[1,1]]]]]]
  ],
  Plus[1,1]
] ;
(* ################################################################################################################################################################ *)
(* FIELD (STRAIGHT POTENTIAL) *)
(* ################################################################################################################################################################ *)
ClearAll[I$FIELD$PLANE] ;
I$FIELD$PLANE::usage = "
I$FIELD$PLANE[POTENTIAL] -- magnetic field for given straight multupole potential <POTENTIAL>
" ;
I$FIELD$PLANE[                    (* -- MAGNETIC FIELD FOR STRAIGHT MULTIPOLE POTENTIAL *)
  POTENTIAL_                      (* -- POTENTIAL *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Curl[POTENTIAL,List[Q1,Q2,I$PARAMETER]]
] ;
(* ################################################################################################################################################################ *)
(* CURVED MULTIPOLE VECTOR POTENTIAL (CYLINDRICAL MULTIPOLE, FOREST) *)
(* ################################################################################################################################################################ *)
(* NOTE: CURVED MULTIPOLE VECTOR POTENTIAL IS NOT MAXWELLIAN (IT IS FOR HOMOGENEOUS VETRICLE FIELD AND ONLY APPROXIMATE IN OTHER CASES) *)
(*       ONLY 3RD COMPONENT IS NOT EQUAL TO ZERO *)
(*       APPEARS IN HAMILTONIAN WITH MINUS SIGN AND MULTIPLIED BY CURVATURE FACTOR *)
(*       LAPLACIAN RESIDUAL SCALES WITH CONPUTATION ORDER *)
(*       MULTIPOLE COEFFICIENTS <KSL> = {KSL[0],KSL[1],KSL[2],KSL[3],...} (S-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B1[Q1,0,Q3] = KSL[0] + KSL[1] Q1 + KSL[2] Q1^2 + KSL[3] Q1^3 + ... (NO FACTORIAL) *)
(*       MULTIPOLE COEFFICIENTS <KNL> = {KNL[0],KNL[1],KNL[2],KNL[3],...} (N-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B2[Q1,0,Q3] = KNL[0] + KNL[1] Q1 + KNL[2] Q1^2 + KNL[3] Q1^3 + ... (NO FACTORIAL) *)
(* ################################################################################################################################################################ *)
ClearAll[I$POTENTIAL$CURVE] ;
I$POTENTIAL$CURVE::usage="
I$POTENTIAL$CURVE[ORDER,KNL,KSL,RADIUS,DEGREE] -- (Forest) curved multipole vector potential (single multipole) for given multipole order <ORDER> (integer), n-amplitude <KNL> (symbol), s-amplitude <KSL> (symbol), curvature radius <RADIUS> (symbol) and computation degree <DEGREE> (integer greater then given order)
I$POTENTIAL$CURVE[KNL,KSL,RADIUS,DEGREE] -- (Forest) curved multipole vector potential (composition of multipoles) for given list of n-amplitudes <KNL> (list of symbols or numbers), list of s-amplitudes <KSL> (list of symbols or numbers), curvature radius <RADIUS> (symbol or number) and maximum computation degree <DEGREE> (integer) (minimum computation degree is equal to number of multipoles plus one)
I$POTENTIAL$CURVE[ORDER,KNL,KSL,RADIUS] -- (McMillan) curved multipole vector potentian (and field) (single McMillan multipole) for given multipole order <ORDER>, n-amplitude <KNL> (symbol or number), s-amplitude <KSL> (symbol or number), curvature radius <RADIUS> (symbol or number)
I$POTENTIAL$CURVE[KNL,KSL,RADIUS,NUMBER,CASE] -- (McMillan) curved multipole vector potential (composition of multipoles) for given list of n-amplitudes <KNL> (list of symbols or numbers), list of s-amplitudes <KSL> (list of symbols or numbers), curvature radius <RADIUS> (symbol or number), number of McMillan multipoles <NUMBER> (integer) and match case <CASE> (<CASE> = 1 -- pure horizontal, <CASE> = 2 -- pure verticle, <CASE> = 3 -- McMillan) " ;
I$POTENTIAL$CURVE[                (* -- (FOREST) CURVED MULTIPOLE POTENTIAL (SINGLE MULTIPOLE) *)
  ORDER_Integer,                  (* -- (LEADING) ORDERER/DEGREEREE (INTEGER) *)
  KNL_,                           (* -- N-AMPLITUDE (SYMBOL OR NUMBER) *)
  KSL_,                           (* -- S-AMPLITUDE (SYMBOL OR NUMBER) *)
  RADIUS_,                        (* -- CURVATURE RADIUS (SYMBOL OR NUMBER) *)
  DEGREE_Integer                  (* -- COMPUTATION DEGREEREE (INTEGER) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Condition[
  Block[
    {Q1,P1,Q2,P2,Q3,P3,KN,KS,RR,SUB,POT,EPS,COE,IND,GEN,LHS,RHS,SYS,PAR,RUL,FIL,ZER,RES},
    {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
    SUB = Thread[Rule[List[KN,KS,RR],List[KNL,KSL,RADIUS]]] ;
    POT = Times[Subtract[0,1],Last[I$POTENTIAL$PLANE[ORDER,KN,KS][CANONICAL]]] ;
    GEN = Range[ORDER,DEGREE] ;
    GEN = Map[
      Composition[
        Total,
        Map[Function[Times[Apply[COE,Slot[1]],Apply[Times,Power[List[Q1,Q2],Slot[1]]]]]],
        Function[FrobeniusSolve[List[1,1],Slot[1]]]
      ],
      GEN
    ] ;
    GEN = Plus[Times[POT,Plus[1,Divide[Q1,RR]]],Total[GEN]] ;
    GEN = ReplaceAll[GEN,Thread[Rule[List[Q1,Q2],Times[EPS,List[Q1,Q2]]]]] ;
    GEN = ReplaceAll[Apply[List,Collect[GEN,EPS]],Rule[EPS,1]] ;
    LHS = List[
      Times[-1,D[POT,Q2]],
      Times[+1,D[POT,Q1]]
    ] ;
    LHS = ReplaceAll[LHS,Rule[Q2,0]] ;
    RHS = List[
      Divide[D[Times[-RR,Total[GEN]],Q2],Plus[RR,Q1]],
      Divide[D[Times[+RR,Total[GEN]],Q1],Plus[RR,Q1]]
    ] ;
    RHS = ReplaceAll[RHS,Rule[Q2,0]] ;
    SYS = Subtract[RHS,LHS] ;
    SYS = Simplify[Times[Plus[RR,Q1],SYS]] ;
    SYS = Map[Function[MonomialList[Slot[1],List[Q1,Q2]]],SYS] ;
    SYS = ReplaceAll[Flatten[SYS],Thread[Rule[CANONICAL,1]]] ;
    PAR = Cases[Variables[SYS],COE[__]] ;
    RUL = First[Solve[Thread[Equal[SYS,0]],PAR]] ;
    GEN = Map[Simplify,ReplaceAll[GEN,RUL]] ;
    Do[
      List[
        RHS = Part[GEN,IND] ;
        RHS = 1/RR*(D[RHS,Q1]-Q1*Laplacian[RHS,List[Q1,Q2]]) ;
        RHS = Total[MonomialList[RHS,List[Q1,Q2]]] ;
        RUL = Thread[Rule[Cases[Variables[RHS],COE[__]],0]] ;
        RHS = ReplaceAll[RHS,RUL] ;
        LHS = Part[GEN,Plus[IND,1]] ;
        LHS = Laplacian[LHS,List[Q1,Q2]] ;
        LHS = Total[MonomialList[LHS,List[Q1,Q2]]] ;
        PAR = Cases[Variables[LHS],COE[__]] ;
        SYS = Subtract[RHS,LHS] ;
        SYS = MonomialList[SYS,List[Q1,Q2]] ;
        SYS = ReplaceAll[SYS,List[Rule[Q1,1],Rule[Q2,1]]] ;
        RUL = Flatten[List[RUL,First[Solve[Thread[Equal[SYS,0]],PAR]]]] ;
        GEN = Map[Simplify,ReplaceAll[GEN,RUL]] ;
      ],
      List[IND,1,Subtract[Length[GEN],1]]
    ] ;
    POT = Times[Divide[GEN,Plus[1,Divide[Q1,RR]]],Subtract[0,1]] ;
    POT = Collect[Total[POT],List[KN,KS],Simplify] ;
    POT = Collect[POT,List[Divide[1,Plus[Q1,RR]],KN,KS],Expand] ;
    ReplaceAll[List[0,0,POT],SUB]
  ],
  Greater[DEGREE,ORDER]
] ;
I$POTENTIAL$CURVE[                (* -- (FOREST) CURVED MULTIPOLE POTENTIAL (COMPOSITION OF SEVERAL MULTUPOLES) *)
  KNL_List,                       (* -- N-AMPLITUDES (LIST) *)
  KSL_List,                       (* -- S-AMPLITUDES (LIST) *)
  RADIUS_,                        (* -- CURVATURE RADIUS (SYMBOL OR NUMBER) *)
  MAX_Integer                     (* -- MAXIMUM COMPUTATION DEGREE (INTEGER) (MINIMUM COMPUTATION DEGREE IS EQUAL TO THE NUMBER OF MULTIPOLES PLUS ONE) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,NUM,STR,LIS,RUL,DEG,POT},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  NUM = Max[Map[Length,List[KNL,KSL]]] ;
  LIS = Array[STR,List[Plus[1,1],NUM]] ;
  RUL = Map[Function[PadRight[Slot[1],NUM]],List[KNL,KSL]] ;
  RUL = Dispatch[Thread[Rule[Flatten[LIS],Flatten[RUL]]]] ;
  LIS = Transpose[LIS] ;
  DEG = Max[Plus[1,Length[LIS]],MAX] ;
  LIS = Map[Flatten,Transpose[List[Range[Length[LIS]],LIS]]] ;
  POT = Map[
    Function[
      Block[
        {ORD,ONE,TWO,OUT},
        ORD = First[Slot[1]] ;
        OUT = Last[I$POTENTIAL$CURVE[ORD,ONE,TWO,RADIUS,DEG][CANONICAL]] ;
        OUT = Simplify[Times[OUT,Plus[1,Divide[Q1,RADIUS]]]] ;
        OUT = Collect[OUT,List[ONE,TWO],Function[Collect[Slot[1],CUR,Expand]]] ;
        OUT = ReplaceAll[OUT,Thread[Rule[List[ONE,TWO],Divide[List[ONE,TWO],Plus[1,Divide[Q1,RADIUS]]]]]] ;
        ReplaceAll[OUT,Thread[Rule[List[ONE,TWO],Rest[Slot[1]]]]]
      ]
    ],
    LIS
  ] ;
  ReplaceAll[List[0,0,Total[POT]],RUL]
] ;
(* ################################################################################################################################################################ *)
(* CURVED MULTIPOLE VECTOR POTENTIAL (MCMILLAN) *)
(* ################################################################################################################################################################ *)
(* NOTE: CURVED MULTIPOLE VECTOR POTENTIAL IS MAXWELLIAN *)
(*       ONLY 3D COMPONENT IS NOT EQUAL TO ZERO *)
(*       APPEARS IN HAMILTONIAN WITH MINUS SIGN AND MULTIPLIED BY CURVATURE FACTOR *)
(*       (CASE-1) MULTIPOLE COEFFICIENTS <KSL> = {KSL[0],KSL[1],KSL[2],KSL[3],...} (S-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B1[Q1,0,Q3] = KSL[0] + KSL[1] Q1 + KSL[2] Q1^2 + KSL[3] Q1^3 + ... (NO FACTORIAL) *)
(*       (CASE-1) MULTIPOLE COEFFICIENTS <KNL> = {KNL[0],KNL[1],KNL[2],KNL[3],...} (N-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B2[Q1,0,Q3] = KNL[0] + KNL[1] Q1 + KNL[2] Q1^2 + KNL[3] Q1^3 + ... (NO FACTORIAL) *)
(*       (CASE-2) MULTIPOLE COEFFICIENTS <KSL> = {KSL[0],KSL[1],KSL[2],KSL[3],...} (S-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B1[0,Q2,Q3] = KSL[0] + KSL[1] Q2 + KSL[2] Q2^2 + KSL[3] Q2^3 + ... (NO FACTORIAL) *)
(*       (CASE-2) MULTIPOLE COEFFICIENTS <KNL> = {KNL[0],KNL[1],KNL[2],KNL[3],...} (N-AMPLITUDES) CORRESPONT TO FIELD EXPANSION B2[0,Q2,Q3] = KNL[0] + KNL[1] Q2 + KNL[2] Q2^2 + KNL[3] Q2^3 + ... (NO FACTORIAL) *)
(*       (CASE-3) MULTIPOLE COEFFICIENTS CORRESPOND TO MCMILLAN COEFFICIENTS *)
(* ################################################################################################################################################################ *)
ClearAll[I$MCMILLAN] ;
I$MCMILLAN::usage = "
I$MCMILLAN[ORD,RAD] -- McMillan radial function of order <ORD> (integer) and radius <RAD> (symbol) (this funtion uses memorization, clean it when done)
" ;
I$MCMILLAN[0,R_]    := I$MCMILLAN[0,R]   = 1 ;
I$MCMILLAN[1,R_]    := I$MCMILLAN[1,R]   = Log[R] ;
I$MCMILLAN[N_,R_]   := I$MCMILLAN[N,R]   = Times[N,Subtract[N,1],Integrate[Divide[Integrate[Times[R,I$MCMILLAN[Subtract[N,Plus[1,1]],R]],List[R,1,R],Rule[GenerateConditions,False]],R],List[R,1,R],Rule[GenerateConditions,False]]] ;
I$POTENTIAL$CURVE[                (* -- (I$MCMILLAN) MULTUPOLE (RETURNS VECTOR POTENTIAL AND MAGNETIC FIELD) *)
  ORD_Integer,                    (* -- MULTIPOLE ORDER (INTEGER) *)
  KNL_,                           (* -- N-AMPLITUDE (I$MCMILLAN) (SYMBOL OR NUMBER) *)
  KSL_,                           (* -- S-AMPLITUDE (I$MCMILLAN) (SYMBOL OR NUMBER) *)
  RAD_                            (* -- CURVARURE RADIUS (SYMBOL OR NUMBER) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,XR,YR,RR,FUN,FAC,RUL,POW,MNL,MSL,VEC,FIL},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  FUN = Range[Plus[1,ORD]] ;
  FUN = Map[Simplify,D[Divide[Map[Curry[I$MCMILLAN][RR],FUN],FUN],RR]] ;
  RUL = Dispatch[Thread[Rule[Array[FAC,Length[FUN]],FUN]]] ;
  VEC = -(I*MSL+MNL)*(XR+I*YR)^ORD ;
  VEC = ReplaceAll[ComplexExpand[VEC],Rule[I,0]] ;
  VEC = Expand[Times[VEC,XR^POW]] ;
  VEC = Simplify[Fold[ReplaceAll,VEC,List[Rule[XR^POW_,FAC[Plus[POW,1]]],Rule[POW,0],RUL,Rule[XR,Subtract[RR,1]]]]] ;
  FIL = List[Subtract[0,D[VEC,YR]],Divide[D[Times[RR,VEC],RR],RR]] ;
  FIL = Map[Simplify,FIL] ;
  VEC = Collect[Times[RAD,VEC],List[MNL,MSL],Expand] ;
  FIL = Collect[Times[Subtract[0,1],FIL],List[MNL,MSL],Expand] ;
  List[VEC,FIL] = Fold[ReplaceAll,List[VEC,FIL],List[Rule[RR,Plus[1,XR]],Rule[XR,Divide[Q1,RAD]],Rule[YR,Divide[Q2,RAD]],Rule[MNL,KNL],Rule[MSL,KSL]]] ;
  VEC = List[0,0,VEC] ;
  FIL = Flatten[List[FIL,0]] ;
  List[VEC,FIL]
] ;
I$POTENTIAL$CURVE[                (* -- (I$MCMILLAN) CURVED MULTIPOLE POTENTIAL (COMPOSITION OF SEVERAL MULTUPOLES) *)
  KNL_List,                       (* -- LIST OF N-AMPLITUDES *)
  KSL_List,                       (* -- LIST OF S-AMPLITUDES *)
  RAD_,                           (* -- CURVARURE RADIUS  *)
  NUM_Integer,                    (* -- NUMBER OF I$MCMILLAN MULTIPOLES (INTEGER) *)
  CAS_Integer                     (* -- MATCH CASE *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,PAR,MNL,MSL,VEC,FIL,RUL,KNO,KSO},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  List[VEC,FIL] = Transpose[Map[Function[I$POTENTIAL$CURVE[Slot[1],MNL[Slot[1]],MSL[Slot[1]],PAR][CANONICAL]],Range[NUM]]] ;
  VEC = Total[Last[Transpose[VEC]]] ;
  Which[
    SameQ[CAS,1],
    List[
      FIL = ReplaceAll[Map[Total,Most[Transpose[FIL]]],Rule[Q2,0]] ;
      FIL = Normal[Series[FIL,List[Q1,0,Subtract[NUM,1]]]] ;
      RUL = List[
        Dot[Map[KSO,Range[0,Subtract[NUM,1],1]],Power[Q1,Range[0,Subtract[NUM,1],1]]],
        Dot[Map[KNO,Range[0,Subtract[NUM,1],1]],Power[Q1,Range[0,Subtract[NUM,1],1]]]
      ] ;
      RUL = ReplaceAll[Flatten[MonomialList[Subtract[RUL,FIL],Q1],1],Rule[Q1,1]] ;
      RUL = First[Solve[Thread[Equal[RUL,0]],Flatten[List[Array[MNL,NUM],Array[MSL,NUM]]]]] ;
    ],
    SameQ[CAS,2],
    List[
      FIL = ReplaceAll[Map[Total,Most[Transpose[FIL]]],Rule[Q1,0]] ;
      FIL = Normal[Series[FIL,List[Q2,0,Subtract[NUM,1]]]] ;
      RUL = List[
        Dot[Map[KSO,Range[0,Subtract[NUM,1],1]],Power[Q2,Range[0,Subtract[NUM,1],1]]],
        Dot[Map[KNO,Range[0,Subtract[NUM,1],1]],Power[Q2,Range[0,Subtract[NUM,1],1]]]
      ] ;
      RUL = ReplaceAll[Flatten[MonomialList[Subtract[RUL,FIL],Q2],1],Rule[Q2,1]] ;
      RUL = First[Solve[Thread[Equal[RUL,0]],Flatten[List[Array[MNL,NUM],Array[MSL,NUM]]]]] ;
    ],
    SameQ[CAS,3],
    List[
      RUL = List[
        Rule[MNL[PAR_],KNO[Subtract[PAR,1]]],
        Rule[MSL[PAR_],KSO[Subtract[PAR,1]]]
      ] ;
    ]
  ] ;
  VEC = ReplaceAll[VEC,RUL] ;
  VEC = Collect[VEC,List[KNO[_],KSO[_]],Composition[Expand,Simplify]] ;
  RUL = Join[
    Thread[Rule[Map[KNO,Range[0,Subtract[NUM,1],1]],PadRight[KNL,NUM]]],
    Thread[Rule[Map[KSO,Range[0,Subtract[NUM,1],1]],PadRight[KSL,NUM]]]
  ] ;
  RUL = Dispatch[RUL] ;
  VEC = ReplaceAll[VEC,RUL] ;
  List[0,0,ReplaceAll[VEC,Rule[PAR,RAD]]]
] ;
(* ################################################################################################################################################################ *)
(* FIELD AND LAPLACIAN FOR CURVED POTENTIAL *)
(* ################################################################################################################################################################ *)
ClearAll[I$FIELD$CURVE] ;
I$FIELD$CURVE::usage = "
I$FIELD$CURVE[RADIUS,POTENTIAL] -- magnetic field for given curvature radius <RADIUS> (symbol or number) and curved potential <POTENTIAL> 
" ;
I$FIELD$CURVE[                    (* -- MAGNETIC FIELD (CURVED MULTIPOLE POTENTIAL) *)
  RADIUS_,                        (* -- CURVATURE RADIUS (SYMBOL OR NUMBER) *)
  POTENTIAL_                      (* -- POTENTIAL *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,GEN},
  List[Q1,P1,Q2,P2,Q3,P3] = CANONICAL ;
  GEN = (1+Q1/RADIUS)*Last[POTENTIAL] ;
  List[
    Divide[D[Times[+RADIUS,GEN],Q2],Plus[RADIUS,Q1]],
    Divide[D[Times[-RADIUS,GEN],Q1],Plus[RADIUS,Q1]],
    0
  ]
] ;
(* ################################################################################################################################################################ *)
(* PARTICLE DATA (RELATIVISTIC FACTORS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$PARTICLE] ;
I$PARTICLE::usage = "
I$PARTICLE[CHARGE,MASS,ENERGY] -- define reference particle with charge <CHARGE> (integer), mass <MASS> and total energy <ENERGY> (same units as mass)
" ;
I$PARTICLE[                       (* -- PARTICLE DATA *)
  CHARGE_,                        (* -- CHARGE (INTEGER) *)
  MASS_,                          (* -- PARTICLE MASS *)
  ENERGY_                         (* -- PARTICLE ENERGY (SAME UNITS AS MASS) *)
] := Block[
  {GAMMA,BETA,MOMENTUM,KINETIC},
  GAMMA = Divide[ENERGY,MASS] ;
  BETA = Sqrt[Subtract[1,Divide[1,Power[GAMMA,2]]]] ;
  MOMENTUM = Times[BETA,GAMMA,MASS] ;
  KINETIC = Subtract[ENERGY,MASS] ;
  Association[
    Rule["CHARGE",CHARGE],
    Rule["MASS",MASS],
    Rule["ENERGY",ENERGY],
    Rule["GAMMA",GAMMA],
    Rule["BETA",BETA],
    Rule["MOMENTUM",MOMENTUM],
    Rule["KINETIC",KINETIC]
  ]
] ;
(* ################################################################################################################################################################ *)
(* GENERIC HAMILTONIAN *)
(* ################################################################################################################################################################ *)
ClearAll[I$HAMILTONIAN] ;
I$HAMILTONIAN::usage = "
I$HAMILTONIAN[RADIUS,SCALAR,VECTOR,List[RFG,RFB]] -- define generic Hamiltonian for curvature radius <RADIUS> (symbol), electric scalar potential <SCALAR> (symbol), magnetic vector potential <VECTOR> (list of symbols), relativistic gamma factor <RFG> (symbol) and relativistic beta factor <RFB> (symbol)
" ;
I$HAMILTONIAN[                    (* -- GENERIC HAMILTONIAN FUNCTION *)
  RADIUS_,                        (* -- CURVATURE RADIUS (SYMBOL) *)
  SCALAR_,                        (* -- (ELECTRIC) SALAR POTENTIAL (SYMBOL) *)
  VECTOR_List,                    (* -- (MAGNETIC) VECTOR POTENTIAL (LIST OF SYMBOLS) *)
  List[RFG_,RFB_]                 (* -- RELATIVISTIC GAMMA NAD BETA FACTORS (LIST OF SYMBOL) *)
][
  CANONICAL_                      (* -- CANONICAL VARIABLES (LIST OF SYMBOLS) *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,A1,A2,A3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {A1,A2,A3} = VECTOR ;
  -(1+1/RADIUS*Q1)*(A3+Sqrt[-(1/(RFB^2*RFG^2))-(P1-A1)^2-(P2-A2)^2+(1/RFB+Q3-SCALAR)^2])
] ;
(* ################################################################################################################################################################ *)
(* SPECIAL (HAMILTONIAN) *)
(* ################################################################################################################################################################ *)
ClearAll[I$DRIF$HAMILTONIAN] ;
I$DRIF$HAMILTONIAN::usage = "
I$DRIF$HAMILTONIAN[List[RFG,RFB]] -- DRIF hamiltonian for given relativistic factors <RFG> and <RFB>
" ;
I$DRIF$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := I$HAMILTONIAN[Infinity,0,List[0,0,0],FACTOR][CANONICAL] ;
ClearAll[I$QUAD$HAMILTONIAN] ;
I$QUAD$HAMILTONIAN::usage = "
I$QUAD$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- QUAD hamiltonian for given n-amplitude <KNL>, s-amplitude <KSL> and relativistic factors <RFG> and <RFB>
" ;
I$QUAD$HAMILTONIAN[KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$HAMILTONIAN[Infinity,0,I$POTENTIAL$PLANE[List[0,KNL],List[0,KSL]][CANONICAL],FACTOR][CANONICAL] ;
ClearAll[I$SEXT$HAMILTONIAN] ;
I$SEXT$HAMILTONIAN::usage = "
I$SEXT$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- SEXT hamiltonian for given n-amplitude <KNL>, s-amplitude <KSL> and relativistic factors <RFG> and <RFB>
" ;
I$SEXT$HAMILTONIAN[KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$HAMILTONIAN[Infinity,0,I$POTENTIAL$PLANE[List[0,0,KNL],List[0,0,KSL]][CANONICAL],FACTOR][CANONICAL] ;
ClearAll[I$OCTU$HAMILTONIAN] ;
I$OCTU$HAMILTONIAN::usage = "
I$OCTU$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- OCTU hamiltonian for given n-amplitude <KNL>, s-amplitude <KSL> and relativistic factors <RFG> and <RFB>
" ;
I$OCTU$HAMILTONIAN[KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$HAMILTONIAN[Infinity,0,I$POTENTIAL$PLANE[List[0,0,0,KNL],List[0,0,0,KSL]][CANONICAL],FACTOR][CANONICAL] ;
ClearAll[I$KICK$HAMILTONIAN] ;
I$KICK$HAMILTONIAN::usage = "
I$KICK$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- straight multipole KICK hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL> and relativistic factors <RFG> and <RFB>
I$KICK$HAMILTONIAN[KNL,KSL,RADIUS,ORDER,List[RFG,RFB]] -- (Forest) cylindrical multipole KICK hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, computation order <ORDER> (integer) and relativistic factors <RFG> and <RFB>
I$KICK$HAMILTONIAN[KNL,KSL,RADIUS,NUMBER,CASE,List[RFG,RFB]] -- (McMillan) cylindrical multipole KICK hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, number of multipoles <NUMBER> (integer), match case <CASE> (integer) and relativistic factors <RFG> and <RFB>
" ;
I$KICK$HAMILTONIAN[KNL_List,KSL_List,FACTOR_:List[Infinity,1]][CANONICAL_] := Times[Subtract[0,1],Last[I$POTENTIAL$PLANE[KNL,KSL][CANONICAL]]] ;
I$KICK$HAMILTONIAN[KNL_List,KSL_List,RADIUS_,ORDER_Integer,FACTOR_List:List[Infinity,1]][CANONICAL_] := Times[Subtract[0,1],Plus[1,Divide[First[CANONICAL],RADIUS]],Last[I$POTENTIAL$CURVE[KNL,KSL,RADIUS,ORDER][CANONICAL]]] ;
I$KICK$HAMILTONIAN[KNL_List,KSL_List,RADIUS_,NUMBER_Integer,CASE_Integer,FACTOR_:List[Infinity,1]][CANONICAL_] := Times[Subtract[0,1],Plus[1,Divide[First[CANONICAL],RADIUS]],Last[I$POTENTIAL$CURVE[KNL,KSL,RADIUS,NUMBER,CASE][CANONICAL]]] ;
ClearAll[I$MULT$HAMILTONIAN] ;
I$MULT$HAMILTONIAN::usage = "
I$MULT$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- straight multipole MULT hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL> and relativistic factors <RFG> and <RFB>
I$MULT$HAMILTONIAN[KNL,KSL,RADIUS,ORDER,List[RFG,RFB]] -- (Forest) cylindrical multipole MULT hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, computation order <ORDER> and relativistic factors <RFG> and <RFB>
I$MULT$HAMILTONIAN[KNL,KSL,RADIUS,NUMBER,CASE,List[RFG,RFB]] -- (McMillan) cylindrical multipole MULT hamiltonian for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, number of multipoles <NUMBER>, match case <CASE> and relativistic factors <RFG> and <RFB>
" ;
I$MULT$HAMILTONIAN[KNL_List,KSL_List,FACTOR_:List[Infinity,1]][CANONICAL_] := Plus[I$DRIF$HAMILTONIAN[FACTOR][CANONICAL],Times[Subtract[0,1],Last[I$POTENTIAL$PLANE[KNL,KSL][CANONICAL]]]] ;
I$MULT$HAMILTONIAN[KNL_List,KSL_List,RADIUS_,ORDER_Integer,FACTOR_List:List[Infinity,1]][CANONICAL_] := Plus[Times[Plus[1,Divide[First[CANONICAL],RADIUS]],I$DRIF$HAMILTONIAN[FACTOR][CANONICAL]],Times[Subtract[0,1],Plus[1,Divide[First[CANONICAL],RADIUS]],Last[I$POTENTIAL$CURVE[KNL,KSL,RADIUS,ORDER][CANONICAL]]]] ;
I$MULT$HAMILTONIAN[KNL_List,KSL_List,RADIUS_,NUMBER_Integer,CASE_Integer,FACTOR_:List[Infinity,1]][CANONICAL_] := Plus[Times[Plus[1,Divide[First[CANONICAL],RADIUS]],I$DRIF$HAMILTONIAN[FACTOR][CANONICAL]],Times[Subtract[0,1],Plus[1,Divide[First[CANONICAL],RADIUS]],Last[I$POTENTIAL$CURVE[KNL,KSL,RADIUS,NUMBER,CASE][CANONICAL]]]] ;
ClearAll[I$RBEN$HAMILTONIAN] ;
I$RBEN$HAMILTONIAN::usage = "
I$RBEN$HAMILTONIAN[STRENGTH,List[RFG,RFB]] -- RBEN hamiltonian for given strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$RBEN$HAMILTONIAN[STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$MULT$HAMILTONIAN[List[STRENGTH],List[],FACTOR][CANONICAL] ;
ClearAll[I$SBEN$HAMILTONIAN] ;
I$SBEN$HAMILTONIAN::usage = "
I$SBEN$HAMILTONIAN[RADIUS,STRENGTH,List[RFG,RFB]] -- SBEN hamiltonian for given curvature radius <RADIUS>, strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SBEN$HAMILTONIAN[RADIUS_,STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$MULT$HAMILTONIAN[List[STRENGTH],List[],RADIUS,0,FACTOR][CANONICAL] ;
ClearAll[I$SHIM$HAMILTONIAN] ;
I$SHIM$HAMILTONIAN::usage = "
I$SHIM$HAMILTONIAN[STRENGTH,List[RFG,RFB]] -- SHIM hamiltonian for given strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SHIM$HAMILTONIAN[STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[{RADIUS},Limit[Times[RADIUS,I$SBEN$HAMILTONIAN[RADIUS,STRENGTH,FACTOR][CANONICAL]],Rule[RADIUS,0]]] ;
ClearAll[I$SOLE$HAMILTONIAN] ;
I$SOLE$HAMILTONIAN::usage = "
I$SOLE$HAMILTONIAN[STRENGTH,List[RFG,RFB]] -- SOLE hamiltonian for given strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SOLE$HAMILTONIAN[STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := I$HAMILTONIAN[Infinity,0,Times[Subtract[0,1],List[Last[I$POTENTIAL$PLANE[List[0],List[Divide[STRENGTH,Plus[1,1]]]][CANONICAL]],Last[I$POTENTIAL$PLANE[List[Divide[STRENGTH,Plus[1,1]]],List[0]][CANONICAL]],0]],FACTOR][CANONICAL] ;
ClearAll[I$DTQ1$HAMILTONIAN] ;
I$DTQ1$HAMILTONIAN::usage = "
I$DTQ1$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q1 translation hamiltonian in free space for given relativistic factors <RFG> and <RFB>
" ;
I$DTQ1$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Times[Subtract[0,1],P1]
] ;
ClearAll[I$DTQ2$HAMILTONIAN] ;
I$DTQ2$HAMILTONIAN::usage = "
I$DTQ2$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q2 translation hamiltonian in free space for given relativistic factors <RFG> and <RFB>)
" ;
I$DTQ2$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Times[Subtract[0,1],P2]
] ;
ClearAll[I$DTQ3$HAMILTONIAN] ;
I$DTQ3$HAMILTONIAN::usage = "
I$DTQ3$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q3 translation hamiltonian in free space for given relativistic factors <RFG> and <RFB>
" ;
I$DTQ3$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  I$DRIF$HAMILTONIAN[FACTOR][List[Q1,P1,Q2,P2,Q3,P3]]
] ;
ClearAll[I$DRQ1$HAMILTONIAN] ;
I$DRQ1$HAMILTONIAN::usage = "
I$DRQ1$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q1 rotation hamiltonian in free space for given relativistic factors <RFG> and <RFB>
" ;
I$DRQ1$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Times[Q2,I$DRIF$HAMILTONIAN[FACTOR][List[Q1,P1,Q2,P2,Q3,P3]]]
] ;
ClearAll[I$DRQ2$HAMILTONIAN] ;
I$DRQ2$HAMILTONIAN::usage = "
I$DRQ2$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q2 rotation hamiltonian in free space for given relativistic factors <RFG> and <RFB>
" ;
I$DRQ2$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Times[Q1,I$DRIF$HAMILTONIAN[FACTOR][List[Q1,P1,Q2,P2,Q3,P3]]]
] ;
ClearAll[I$DRQ3$HAMILTONIAN] ;
I$DRQ3$HAMILTONIAN::usage = "
I$DRQ3$HAMILTONIAN[List[RFG,RFB]] -- dynamic Q3 rotation hamiltonian in free space for given relativistic factors <RFG> and <RFB>
" ;
I$DRQ3$HAMILTONIAN[FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  Plus[Times[Subtract[0,1],P2,Q1],Times[P1,Q2]]
] ;
ClearAll[I$MULT$FRINGE$HAMILTONIAN] ;
I$MULT$FRINGE$HAMILTONIAN::usage = "
I$MULT$FRINGE$HAMILTONIAN[ORDER,KNL,KSL,List[RFG,RFB]] -- (Forest) leading order fringe hamiltonian for straight multipole of order <ORDER> (integer) with n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol)
I$MULT$FRINGE$HAMILTONIAN[KNL,KSL,List[RFG,RFB]] -- (Forest) leading order fringe hamiltonian for combination of straight multipoles with n-amplitudes <KNL> (list) and s-amplitudes <KSL> (list)
" ;
I$MULT$FRINGE$HAMILTONIAN[ORDER_Integer,KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,GENERATOR,F1,F2},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  GENERATOR = -1/(4(ORDER+1))*(KNL+I*KSL)(Q1+I*Q2)^ORDER*(Q1*P1+Q2*P2+I*(ORDER+2)/ORDER*(Q1*P2-Q2*P1)) ;
  GENERATOR = Expand[GENERATOR] ;
  GENERATOR = ComplexExpand[GENERATOR] ;
  GENERATOR = ReplaceAll[GENERATOR,Rule[I,0]] ;
  F1 = Simplify[Coefficient[GENERATOR,P1]] ;
  F2 = Simplify[Coefficient[GENERATOR,P2]] ;
  -(F1*P1+F2*P2)/(1/RB+Q3)
] ;
I$MULT$FRINGE$HAMILTONIAN[KNL_List,KSL_List,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,MN,KN,KS,GENERATOR,F1,F2,RULE},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  MN = Max[Map[Length,List[KNL,KSL]]] ;
  GENERATOR = (1/RB+Q3)*Total[Map[Function[I$MULT$FRINGE$HAMILTONIAN[Slot[1],KN[Slot[1]],KS[Slot[1]],FACTOR][CANONICAL]],Range[MN]]] ;
  F1 = Simplify[Coefficient[GENERATOR,P1]] ;
  F2 = Simplify[Coefficient[GENERATOR,P2]] ;
  F1 = Collect[F1,Flatten[List[Map[KN,Range[MN]],Map[KS,Range[MN]]]],Simplify] ;
  F2 = Collect[F2,Flatten[List[Map[KN,Range[MN]],Map[KS,Range[MN]]]],Simplify] ;
  GENERATOR = (F1*P1+F2*P2)/(1/RB+Q3) ;
  RULE = Join[
    Thread[Rule[Map[KN,Range[MN]],PadRight[KNL,MN]]],
    Thread[Rule[Map[KS,Range[MN]],PadRight[KSL,MN]]]
  ] ;
  RULE = Dispatch[RULE] ;
  ReplaceAll[GENERATOR,RULE]
] ;
ClearAll[I$BEND$FRINGE$HAMILTONIAN] ;
I$BEND$FRINGE$HAMILTONIAN::usage = "
I$BEND$FRINGE$HAMILTONIAN[STRENGTH,List[RFG,RFB]] -- BEND fringe field hamiltonian for given strength <STRENGTH>
" ;
I$BEND$FRINGE$HAMILTONIAN[STRENGTH_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,HAMILTONIAN},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  HAMILTONIAN = -I$DRIF$HAMILTONIAN[FACTOR][CANONICAL] ;
  STRENGTH*(P1/HAMILTONIAN)/(1+(P2/HAMILTONIAN)^2)*Q2^2/2
] ;
(* ################################################################################################################################################################ *)
(* SPECIAL (MAP) *)
(* ################################################################################################################################################################ *)
ClearAll[I$DRIF$MAP] ;
I$DRIF$MAP::usage = "
I$DRIF$MAP[List[RFG,RFB]] -- DRIF transformation for given relativistic factors <RFG> and <RFB>
" ;
I$DRIF$MAP[FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Q1+P1*I$PARAMETER/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2],
      P1,
      Q2+P2*I$PARAMETER/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2],
      P2,
      Q3,
      P3+(1/RB+Q3)*I$PARAMETER/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]
    ]
  ]
] ;
ClearAll[I$DTQ1$MAP] ;
I$DTQ1$MAP::usage = "
I$DTQ1$MAP[DELTA,List[RFG,RFB]] -- dynamic translation transformation along Q1 by <DELTA> (length)
" ;
I$DTQ1$MAP[DELTA_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Subtract[Q1,DELTA],
      P1,
      Q2,
      P2,
      Q3,
      P3
    ]
  ]
] ;
ClearAll[I$DTQ2$MAP] ;
I$DTQ2$MAP::usage = "
I$DTQ2$MAP[DELTA,List[RFG,RFB]] -- dynamic transtation transformation along Q2 by <DELTA> (length)
" ;
I$DTQ2$MAP[DELTA_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Q1,
      P1,
      Subtract[Q2,DELTA],
      P2,
      Q3,
      P3
    ]
  ]
] ;
ClearAll[I$DTQ3$MAP] ;
I$DTQ3$MAP::usage = "
I$DTQ3$MAP[DELTA,List[RFG,RFB]] -- dynamic translation transformation along Q3 by <DELTA> (length)
" ;
I$DTQ3$MAP[DELTA_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Q1+P1*DELTA/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2],
      P1,
      Q2+P2*DELTA/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2],
      P2,
      Q3,
      P3+(1/RB+Q3)*DELTA/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]
    ]
  ]
] ;
ClearAll[I$DRQ1$MAP] ;
I$DRQ1$MAP::usage = "
I$DRQ1$MAP[ANGLE,List[RFG,RFB]] -- dynamic rotation transformation around Q1 by <ANGLE>
" ;
I$DRQ1$MAP[ANGLE_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Q1+(P1*Q2*Tan[ANGLE])/(Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]-P2*Tan[ANGLE]),
      P1,
      (Q2*Sec[ANGLE])/(1-(P2*Tan[ANGLE])/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]),
      P2*Cos[ANGLE]+Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]*Sin[ANGLE],
      Q3,
      P3+((1/RB+Q3)*Q2*Tan[ANGLE])/(Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]-P2*Tan[ANGLE])
    ]
  ]
] ;
ClearAll[I$DRQ2$MAP] ;
I$DRQ2$MAP::usage = "
I$DRQ2$MAP[ANGLE,List[RFG,RFB]] -- dynamic rotation transformation around Q2 by <ANGLE>
" ;
I$DRQ2$MAP[ANGLE_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      (Q1*Sec[ANGLE])/(1-(P1*Tan[ANGLE])/Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]),
      P1*Cos[ANGLE]+Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]*Sin[ANGLE],
      Q2+(P2*Q1*Tan[ANGLE])/(Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]-P1*Tan[ANGLE]),
      P2,
      Q3,
      P3+((1/RB+Q3)*Q1*Tan[ANGLE])/(Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]-P1*Tan[ANGLE])
    ]
  ]
] ;
ClearAll[I$DRQ3$MAP] ;
I$DRQ3$MAP::usage = "
I$DRQ3$MAP[ANGLE,List[RFG,RFB]] -- dynamic rotation transformation around Q3 by <ANGLE>
" ;
I$DRQ3$MAP[ANGLE_,FACTOR_:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      Q1*Cos[ANGLE]+Q2*Sin[ANGLE],
      P1*Cos[ANGLE]+P2*Sin[ANGLE],
      Q2*Cos[ANGLE]-Q1*Sin[ANGLE],
      P2*Cos[ANGLE]-P1*Sin[ANGLE],
      Q3,
      P3
    ]
  ]
] ;
ClearAll[I$KICK$MAP] ;
I$KICK$MAP::usage = "
I$KICK$MAP[KNL,KSL,List[RFG,RFB]] -- KICK transformation (straight multipole) for given n-amplitudes <KNL>, s-amplitudes <KSL> and relativistic factors <RFG> and <RFB>
I$KICK$MAP[KNL,KSL,RADIUS,ORDER,List[RFG,RFB]] -- (Forest) KICK transformation (curved multipole) for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, computation order <ORDER> and relativistic factors <RFG> and <RFB>
I$KICK$MAP[KNL,KSL,RADIUS,NUMBER,CASE,List[RFG,RFB]] -- (McMillan) KICK transformation (curved multipole) for given n-amplitudes <KNL>, s-amplitudes <KSL>, curvarure radius <RADIUS>, number of multipoles <NUMBER>, match case <CASE> and relativistic factors <RFG> and <RFB>
" ;
I$KICK$MAP[KNL_List,KSL_List,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,KICK},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  KICK = I$KICK$HAMILTONIAN[KNL,KSL,FACTOR][CANONICAL] ;
  KICK = First[I$DSOLVE[List[KICK]]] ;
  List[Collect[KICK,Cases[Flatten[List[I$PARAMETER,KNL,KSL]],_Symbol],Expand]]
] ;
I$KICK$MAP[KNL_List,KSL_List,RADIUS_,ORDER_Integer,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,KICK},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  KICK = I$KICK$HAMILTONIAN[KNL,KSL,RADIUS,ORDER,FACTOR][CANONICAL] ;
  KICK = First[I$DSOLVE[List[KICK]]] ;
  List[Collect[KICK,Cases[Flatten[List[I$PARAMETER,KNL,KSL]],_Symbol],Expand]]
] ;
I$KICK$MAP[KNL_List,KSL_List,RADIUS_,NUMBER_Integer,CASE_Integer,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,KICK},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  KICK = I$KICK$HAMILTONIAN[KNL,KSL,RADIUS,NUMBER,CASE,FACTOR][CANONICAL] ;
  KICK = First[I$DSOLVE[List[KICK]]] ;
  List[Collect[KICK,Cases[Flatten[List[I$PARAMETER,KNL,KSL]],_Symbol],Expand]]
] ;
ClearAll[I$RBEN$MAP] ;
I$RBEN$MAP::usage = "
I$RBEN$MAP[STRENGTH,List[RFG,RFB]] -- RBEN transformation for given strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$RBEN$MAP[STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,OUT},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      (STRENGTH*Q1-Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2]+Sqrt[-(1/(RB^2*RG^2))-P1^2-P2^2+(1/RB+Q3)^2+2*STRENGTH*P1*I$PARAMETER-STRENGTH^2*I$PARAMETER^2])/STRENGTH,
      P1-STRENGTH*I$PARAMETER,
      (STRENGTH*Q2+P2*ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-P2*ArcSin[(P1-STRENGTH*I$PARAMETER)/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]])/STRENGTH,
      P2,
      Q3,
      P3+((Q3+1/RB)*(ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-ArcSin[(P1-STRENGTH*I$PARAMETER)/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]))/STRENGTH
    ]
  ]
] ;
ClearAll[I$SBEN$MAP] ;
I$SBEN$MAP::usage = "
I$SBEN$MAP[RADIUS,STRENGTH,List[RFG,RFB]] -- SBEN transformation for given curvature radius <RADIUS>, strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SBEN$MAP[RADIUS_,STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,OUT},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      -RADIUS+(Q1+RADIUS-Sqrt[-P1^2-P2^2+(1/RB+Q3)^2-1/(RB^2*RG^2)]/STRENGTH)*Cos[I$PARAMETER/RADIUS]+(P1 Sin[I$PARAMETER/RADIUS])/STRENGTH+Sqrt[-P2^2+(1/RB+Q3)^2-1/(RB^2*RG^2)-(P1 Cos[I$PARAMETER/RADIUS]+(Sqrt[-P1^2-P2^2+(1/RB+Q3)^2-1/(RB^2*RG^2)]-(Q1+RADIUS) STRENGTH) Sin[I$PARAMETER/RADIUS])^2]/STRENGTH,
      P1*Cos[I$PARAMETER/RADIUS]+(-STRENGTH*(Q1+RADIUS)+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[I$PARAMETER/RADIUS],
      Q2+(P2*I$PARAMETER)/(STRENGTH*RADIUS)+(P2*(ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-ArcSin[(P1*Cos[I$PARAMETER/RADIUS]+(-STRENGTH*(Q1+RADIUS)+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[I$PARAMETER/RADIUS])/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]))/STRENGTH,
      P2,
      Q3,
      P3+((Q3+1/RB)*I$PARAMETER)/(STRENGTH*RADIUS)+((Q3+1/RB)*(ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-ArcSin[(P1*Cos[I$PARAMETER/RADIUS]+(-STRENGTH*(Q1+RADIUS)+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[I$PARAMETER/RADIUS])/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]))/STRENGTH
    ]
  ]
] ;
ClearAll[I$SHIM$MAP] ;
I$SHIM$MAP::usage = "
I$SHIM$MAP[ANGLE,STRENGTH,List[RFG,RFB]] -- SHIM transformation for given angle <ANGLE>, strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SHIM$MAP[ANGLE_,STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,OUT},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      (1/STRENGTH)(STRENGTH*Q1*Cos[ANGLE]-Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]*Cos[ANGLE]+P1*Sin[ANGLE]+Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)-(P1*Cos[ANGLE]+(-STRENGTH*Q1+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[ANGLE])^2]),
      P1*Cos[ANGLE]+(-STRENGTH*Q1+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[ANGLE],
      (STRENGTH*Q2+P2*ANGLE+P2*ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-P2*ArcSin[(P1*Cos[ANGLE]+(-STRENGTH*Q1+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[ANGLE])/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]])/STRENGTH,
      P2,
      Q3,
      P3+((Q3+1/RB)*ANGLE)/STRENGTH+((Q3+1/RB)*(ArcSin[P1/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2 RG^2)]]-ArcSin[(P1*Cos[ANGLE]+(-STRENGTH*Q1+Sqrt[-P1^2-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)])*Sin[ANGLE])/Sqrt[-P2^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]))/STRENGTH
    ]
  ]
] ;
ClearAll[I$SOLE$MAP] ;
I$SOLE$MAP::usage = "
I$SOLE$MAP[STRENGTH,List[RFG,RFB]] -- SOLE transformation for given strength <STRENGTH> and relativistic factors <RFG> and <RFB>
" ;
I$SOLE$MAP[STRENGTH_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,OUT},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  List[
    List[
      (2*P2+STRENGTH*Q1+(-2*P2+STRENGTH*Q1)*Cos[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]+(2*P1+STRENGTH*Q2)*Sin[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]])/(2*STRENGTH),
      1/4*(2*P1-STRENGTH*Q2+(2*P1+STRENGTH*Q2)*Cos[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]+(2*P2-STRENGTH*Q1)*Sin[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]),
      (-2*P1+STRENGTH*Q2+(2*P1+STRENGTH*Q2)*Cos[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]+(2*P2-STRENGTH*Q1)*Sin[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]])/(2*STRENGTH),
      1/4*(2*P2+STRENGTH*Q1+(2*P2-STRENGTH*Q1)*Cos[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]-(2*P1+STRENGTH*Q2)*Sin[(STRENGTH*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]]),
      Q3,
      P3+((1/RB+Q3)*I$PARAMETER)/Sqrt[-(P2-(STRENGTH*Q1)/2)^2-(P1+(STRENGTH*Q2)/2)^2+(Q3+1/RB)^2-1/(RB^2*RG^2)]
    ]
  ]
] ;
ClearAll[I$MULT$FRINGE$MAP] ;
I$MULT$FRINGE$MAP::usage = "
I$MULT$FRINGE$MAP[ORDER,KNL,KSL,List[RFG,RFB]] -- (Forest) 1st order fringe map for straight multipole of order <ORDER> (integer) with n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol) (change signs of <KNL> and <KSL> to get the corresponding exit fringe map)
" ;
I$MULT$FRINGE$MAP[ORDER_,KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,GENERATOR,F1,F2,F1D1,F1D2,F2D1,F2D2},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  GENERATOR = -1/(4(ORDER+1))*(KNL+I*KSL)(Q1+I*Q2)^ORDER*(Q1*P1+Q2*P2+I*(ORDER+2)/ORDER*(Q1*P2-Q2*P1)) ;
  GENERATOR = Expand[GENERATOR] ;
  GENERATOR = ComplexExpand[GENERATOR] ;
  GENERATOR = ReplaceAll[GENERATOR,Rule[I,0]] ;
  F1 = Coefficient[GENERATOR,P1] ;
  F1D1 = D[F1,Q1] ;
  F1D2 = D[F1,Q2] ;
  F2 = Coefficient[GENERATOR,P2] ;
  F2D1 = D[F2,Q1] ;
  F2D2 = D[F2,Q2] ;
  GENERATOR = List[
    Q1-(F1*RB)/(1+Q3*RB),
    -(((1+Q3*RB)*(P1+F2D1*P2*RB+P1*(-F2D2+Q3)*RB))/(-1+(F1D1+F2D2-2*Q3)*RB+(F1D2*F2D1+(F1D1-Q3)*(-F2D2+Q3))*RB^2)),
    Q2-(F2*RB)/(1+Q3*RB),
    -(((1+Q3*RB)*(P2+F1D2*P1*RB+P2*(-F1D1+Q3)*RB))/(-1+(F1D1+F2D2-2*Q3)*RB+(F1D2*F2D1+(F1D1-Q3)*(-F2D2+Q3))*RB^2)),
    Q3,
    P3-(RB^2*(F2*(P2+F1D2*P1*RB+P2*(-F1D1+Q3)*RB)+F1*(P1+F2D1*P2*RB+P1*(-F2D2+Q3)*RB)))/((1+Q3*RB)*(1-(F1D1+F2D2-2*Q3)*RB+(-F1D2*F2D1+(F1D1-Q3)*(F2D2-Q3))*RB^2))
  ] ;
  List[Map[Simplify,GENERATOR]]
] ;
ClearAll[I$MONOMIAL$MAP] ;
I$MONOMIAL$MAP::usage = "
I$MONOMIAL$MAP[AMPLITUDE,List[NQ,NP]][Q,P] -- monomial transformation map generated by hamiltonian H = -AMPLITUDE*Q^NQ*P^NP
" ;
I$MONOMIAL$MAP[AMPLITUDE_,List[NQ_,NP_]][Q_,P_] := If[
  UnsameQ[NQ,NP],
  List[
    Q*(1+AMPLITUDE(NQ-NP)*Q^(NQ-1)*P^(NP-1))^(NP/(NP-NQ)),
    P*(1+AMPLITUDE(NQ-NP)*Q^(NQ-1)*P^(NP-1))^(NQ/(NQ-NP))
  ],
  List[
    Q*Exp[-AMPLITUDE*NQ*Q^(NQ-1)*P^(NQ-1)],
    P*Exp[+AMPLITUDE*NQ*Q^(NQ-1)*P^(NQ-1)]
  ]
] ;
ClearAll[I$MULT$FRINGE$MAP$LIST] ;
I$MULT$FRINGE$MAP$LIST::usage = "
I$MULT$FRINGE$MAP$LIST[DIRECTION,ORDER,KNL,KSL,List[RFG,RFB]] -- list of monomial maps derived by splitting leading order fringe multipole hamiltonian into sovable monomials for given direction <DIRECTION> (use '+1' for entrance fringe and '-1' for exit fringe), multipole order <ORDER> (integer) with n-amplitude <KNL> (symbol) and s-amplitude <KSL> (symbol), generates list of maps sutable for Yoshida integration
I$MULT$FRINGE$MAP$LIST[DIRECTION,KNL,KSL,List[RFG,RFB]] -- list of monomial maps derived by splitting leading order fringe multipole hamiltonian into sovable monomials for given direction <DIRECTION> (use '+1' for entrance fringe and '-1' for exit fringe), multipole order <ORDER> (integer) with n-amplitudes <KNL> (list) and s-amplitudes <KSL> (list), generates list of maps sutable for Yoshida integration
" ;
I$MULT$FRINGE$MAP$LIST[DIRECTION_Integer,ORDER_Integer,KNL_,KSL_,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,GENERATOR,ONE,TWO,HAMILTONIAN,A,B},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  GENERATOR = I$MULT$FRINGE$HAMILTONIAN[ORDER,KNL,KSL,FACTOR][CANONICAL] ;
  GENERATOR = Times[DIRECTION,I$PARAMETER,GENERATOR] ;
  ONE = MonomialList[Times[Coefficient[GENERATOR,P1],P1],List[Q1,P1,Q2,P2]] ;
  HAMILTONIAN = Divide[ONE,I$PARAMETER] ;
  ONE = Times[Subtract[0,1],ONE] ;
  ONE = Times[ONE,Apply[Times,Power[List[Q1,P1],List[A,B]]]] ;
  ONE = Transpose[List[ONE,Cases[ONE,Rule[Times[___,Q1^A_,P1^B_],List[A,B]]]]] ;
  ONE = ReplaceAll[ONE,Thread[Rule[List[A,B],0]]] ;
  ONE = ReplaceAll[ONE,Thread[Rule[List[Q1,P1],1]]] ;
  ONE = Through[Map[Apply[I$MONOMIAL$MAP],ONE][Q1,P1]] ;
  ONE = Transpose[List[ConstantArray[List[Q1,P1],Length[ONE]],ONE]] ;
  ONE = Map[Composition[Thread,Apply[Rule]],ONE] ;
  ONE = List[
    ONE,
    Thread[Rule[P2,P2+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q2],ONE]],I$PARAMETER]]],
    Thread[Rule[P3,P3+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q3],ONE]],I$PARAMETER]]]
  ] ;
  ONE = Map[Composition[ReplaceAll,Flatten],Transpose[ONE]] ;
  ONE = Through[ONE[CANONICAL]] ;
  TWO = MonomialList[Times[Coefficient[GENERATOR,P2],P2],List[Q1,P1,Q2,P2]] ;
  HAMILTONIAN = Divide[TWO,I$PARAMETER] ;
  TWO = Times[Subtract[0,1],TWO] ;
  TWO = Times[TWO,Apply[Times,Power[List[Q2,P2],List[A,B]]]] ;
  TWO = Transpose[List[TWO,Cases[TWO,Rule[Times[___,Q2^A_,P2^B_],List[A,B]]]]] ;
  TWO = ReplaceAll[TWO,Thread[Rule[List[A,B],0]]] ;
  TWO = ReplaceAll[TWO,Thread[Rule[List[Q2,P2],1]]] ;
  TWO = Through[Map[Apply[I$MONOMIAL$MAP],TWO][Q2,P2]] ;
  TWO = Transpose[List[ConstantArray[List[Q2,P2],Length[TWO]],TWO]] ;
  TWO = Map[Composition[Thread,Apply[Rule]],TWO] ;
  TWO = List[
    TWO,
    Thread[Rule[P1,P1+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q1],TWO]],I$PARAMETER]]],
    Thread[Rule[P3,P3+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q3],TWO]],I$PARAMETER]]]
  ] ;
  TWO = Map[Composition[ReplaceAll,Flatten],Transpose[TWO]] ;
  TWO = Through[TWO[CANONICAL]] ;
  Join[ONE,TWO]
] ;
I$MULT$FRINGE$MAP$LIST[DIRECTION_Integer,KNL_List,KSL_List,FACTOR_List:List[Infinity,1]][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3,RG,RB,GENERATOR,ONE,TWO,HAMILTONIAN,A,B},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  {RG,RB} = FACTOR ;
  GENERATOR = I$MULT$FRINGE$HAMILTONIAN[KNL,KSL,FACTOR][CANONICAL] ;
  GENERATOR = Times[DIRECTION,I$PARAMETER,GENERATOR] ;
  ONE = MonomialList[Times[Coefficient[GENERATOR,P1],P1],List[Q1,P1,Q2,P2]] ;
  HAMILTONIAN = Divide[ONE,I$PARAMETER] ;
  ONE = Times[Subtract[0,1],ONE] ;
  ONE = Times[ONE,Apply[Times,Power[List[Q1,P1],List[A,B]]]] ;
  ONE = Transpose[List[ONE,Cases[ONE,Rule[Times[___,Q1^A_,P1^B_],List[A,B]]]]] ;
  ONE = ReplaceAll[ONE,Thread[Rule[List[A,B],0]]] ;
  ONE = ReplaceAll[ONE,Thread[Rule[List[Q1,P1],1]]] ;
  ONE = Through[Map[Apply[I$MONOMIAL$MAP],ONE][Q1,P1]] ;
  ONE = Transpose[List[ConstantArray[List[Q1,P1],Length[ONE]],ONE]] ;
  ONE = Map[Composition[Thread,Apply[Rule]],ONE] ;
  ONE = List[
    ONE,
    Thread[Rule[P2,P2+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q2],ONE]],I$PARAMETER]]],
    Thread[Rule[P3,P3+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q3],ONE]],I$PARAMETER]]]
  ] ;
  ONE = Map[Composition[ReplaceAll,Flatten],Transpose[ONE]] ;
  ONE = Through[ONE[CANONICAL]] ;
  TWO = MonomialList[Times[Coefficient[GENERATOR,P2],P2],List[Q1,P1,Q2,P2]] ;
  HAMILTONIAN = Divide[TWO,I$PARAMETER] ;
  TWO = Times[Subtract[0,1],TWO] ;
  TWO = Times[TWO,Apply[Times,Power[List[Q2,P2],List[A,B]]]] ;
  TWO = Transpose[List[TWO,Cases[TWO,Rule[Times[___,Q2^A_,P2^B_],List[A,B]]]]] ;
  TWO = ReplaceAll[TWO,Thread[Rule[List[A,B],0]]] ;
  TWO = ReplaceAll[TWO,Thread[Rule[List[Q2,P2],1]]] ;
  TWO = Through[Map[Apply[I$MONOMIAL$MAP],TWO][Q2,P2]] ;
  TWO = Transpose[List[ConstantArray[List[Q2,P2],Length[TWO]],TWO]] ;
  TWO = Map[Composition[Thread,Apply[Rule]],TWO] ;
  TWO = List[
    TWO,
    Thread[Rule[P1,P1+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q1],TWO]],I$PARAMETER]]],
    Thread[Rule[P3,P3+Integrate[-MapThread[ReplaceAll,List[D[HAMILTONIAN,Q3],TWO]],I$PARAMETER]]]
  ] ;
  TWO = Map[Composition[ReplaceAll,Flatten],Transpose[TWO]] ;
  TWO = Through[TWO[CANONICAL]] ;
  Join[ONE,TWO]
] ;
ClearAll[I$BEND$FRINGE$MAP] ;
I$BEND$FRINGE$MAP::usage="
I$BEND$FRINGE$MAP[STRENGTH] -- BEND entrance fringe map for given scaled field <STRENGTH> (use negative value for exit fringe)
I$BEND$FRINGE$MAP[STRENGTH,FINT,HGAP] -- BEND entrance fringe map for given scaled field <STRENGTH> (use negative value for exit fringe), fringe integral <FINT> and half gap <HGAP> (both variants are valid for ultrarelativistic case)
" ;
I$BEND$FRINGE$MAP[STRENGTH_][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  List[
    List[
      Plus[Q1,Times[-2,STRENGTH,Power[Q2,2],Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-2],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]],Plus[Times[Power[P2,2],Power[Plus[1,Q3],2]],Times[-1,Power[Plus[1,Q3],4]],Times[Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P1,P2,Q2,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]]]],Rational[1,2]]],-2]]],
      P1,
      Times[2,Q2,Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P1,P2,Q2,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]]]],Rational[1,2]]],-1]],
      Plus[P2,Times[2,STRENGTH,P1,Q2,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P1,P2,Q2,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]]]],Rational[1,2]]],-1]]],
      Q3,
      Plus[P3,Times[-2,STRENGTH,P1,Power[Q2,2],Plus[1,Q3],Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-2],Plus[Power[P1,2],Times[2,Power[P2,2]],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P1,P2,Q2,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-1,2]]]],Rational[1,2]]],-2]]]
    ]
  ]
] ;
I$BEND$FRINGE$MAP[STRENGTH_,FINT_,HGAP_][CANONICAL_] := Block[
  {Q1,P1,Q2,P2,Q3,P3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  List[
    List[
      Plus[Q1,Times[-2,STRENGTH,Power[Q2,2],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Times[-1,Power[Plus[1,Q3],4]],Times[Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[-1,Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Plus[Times[Power[P2,2],Power[Plus[1,Q3],2]],Times[-1,Power[Plus[1,Q3],4]],Times[Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Times[Power[P1,6],Plus[Power[P2,2],Power[Plus[1,Q3],2]]],Times[Power[P1,4],Plus[Times[5,Power[P2,4]],Times[Power[P2,2],Power[Plus[1,Q3],2]],Times[-5,Power[Plus[1,Q3],4]]]],Times[-1,Power[Plus[1,Q3],4],Plus[Power[P2,4],Times[-4,Power[P2,2],Power[Plus[1,Q3],2]],Times[3,Power[Plus[1,Q3],4]]]],Times[Power[P1,2],Plus[Power[P2,6],Times[-3,Power[P2,4],Power[Plus[1,Q3],2]],Times[-6,Power[P2,2],Power[Plus[1,Q3],4]],Times[7,Power[Plus[1,Q3],6]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-2]]],
      P1,
      Times[2,Q2,Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-1]],
      Plus[P2,Times[Rational[-4,9],Power[STRENGTH,2],Power[HGAP,-1],Power[FINT,-1],Power[Q2,3],Power[Plus[1,Q3],-1],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-3]],Times[-2,STRENGTH,Q2,Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-1],Tan[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]]]],
      Q3,
      Plus[P3,Times[Rational[2,9],Power[STRENGTH,2],Power[HGAP,-1],Power[FINT,-1],Power[Q2,4],Power[Plus[1,Q3],-2],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-4]],Times[2,STRENGTH,Power[Q2,2],Plus[1,Q3],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[-1,P1,Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Plus[Power[P1,2],Times[2,Power[P2,2]],Times[-1,Power[Plus[1,Q3],2]]]],Times[-2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[Plus[1,Q3],4],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[3,Power[P1,6],Plus[Power[P2,2],Power[Plus[1,Q3],2]]],Times[Power[P1,4],Plus[Power[P2,4],Times[-3,Power[P2,2],Power[Plus[1,Q3],2]],Times[-7,Power[Plus[1,Q3],4]]]],Times[Power[P1,2],Plus[Power[P2,6],Times[-1,Power[P2,4],Power[Plus[1,Q3],2]],Times[-2,Power[P2,2],Power[Plus[1,Q3],4]],Times[5,Power[Plus[1,Q3],6]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2],Power[Plus[1,Power[Plus[1,Times[-2,STRENGTH,P2,Q2,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-5,2]],Power[Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]],-1],Plus[Times[2,STRENGTH,HGAP,FINT,Plus[Times[-1,Power[P1,4]],Times[Power[P1,2],Power[P2,2]],Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]],Times[2,P1,Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Plus[Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2],Times[2,STRENGTH,HGAP,FINT,P1,Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]],Times[-1,P1,Plus[Times[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],Power[Plus[Power[P1,2],Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]],Times[4,STRENGTH,HGAP,FINT,P1,Plus[Times[-2,Power[P1,2]],Times[-1,Power[P2,2]],Times[2,Power[Plus[1,Q3],2]]],Plus[Power[Plus[1,Q3],4],Times[-1,Power[P1,2],Plus[Power[P2,2],Power[Plus[1,Q3],2]]]]]]]],Power[Sec[Plus[Times[2,STRENGTH,HGAP,FINT,Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[-3,2]],Plus[Power[P1,4],Times[-1,Power[P1,2],Power[P2,2]],Times[-1,Power[Plus[Power[P2,2],Times[-1,Power[Plus[1,Q3],2]]],2]]]],Times[-1,ArcTan[Times[P1,Power[Plus[Power[P1,2],Times[-1,Power[Plus[1,Q3],2]]],-1],Power[Plus[Times[-1,Power[P1,2]],Times[-1,Power[P2,2]],Power[Plus[1,Q3],2]],Rational[1,2]]]]]]],2]]],Rational[1,2]]],-2]]]
    ]
  ]
] ;
(* ################################################################################################################################################################ *)
(* ALIGNMENT (ACTS ON THIN REPRESENTATION) *)
(* ################################################################################################################################################################ *)
ClearAll[I$ALIGNMENT$ENT$DYNAMIC] ;
I$ALIGNMENT$ENT$DYNAMIC::usage = "
I$ALIGNMENT$ENT$DYNAMIC[List[A1,A2,A3],List[D1,D2,D3],List[FRG,FRB]] -- entrance alignment transformations (acts on thin representation)
" ;
I$ALIGNMENT$ENT$DYNAMIC[          (* -- STRAIGHT BLOCK ENTRANCE ALIGNMENT TRANSFORMATIONS (DYNAMIC) *)
  List[A1_,A2_,A3_],              (* -- LIST OF ANGLES *)
  List[D1_,D2_,D3_],              (* -- LIST OF DISPLACEMENTS *)
  FACTOR_:List[Infinity,1]        (* -- FACTORATIVISTIC FACTORS *)
][
  CANONICAL_                      (* -- LIST OF CANONICAL VARIABLES *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,MAP},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  MAP = Join[
    I$DTQ1$MAP[D1,FACTOR][CANONICAL],
    I$DTQ2$MAP[D2,FACTOR][CANONICAL],
    I$DTQ3$MAP[D3,FACTOR][CANONICAL],
    I$DRQ1$MAP[A1,FACTOR][CANONICAL],
    I$DRQ2$MAP[A2,FACTOR][CANONICAL],
    I$DRQ3$MAP[A3,FACTOR][CANONICAL]
  ] ;
  DeleteCases[MAP,CANONICAL]
] ;
ClearAll[I$ALIGNMENT$EXT$DYNAMIC] ;
I$ALIGNMENT$EXT$DYNAMIC::usage = "
I$ALIGNMENT$EXT$DYNAMIC[List[A1,A2,A3],List[D1,D2,D3],List[FRG,FRB]] -- exit alignment transformations (acts on thin representation)
" ;
I$ALIGNMENT$EXT$DYNAMIC[          (* -- STRAIGHT BLOCK EXIT ALIGNMENT TRANSFORMATIONS (DYNAMIC) *)
  List[A1_,A2_,A3_],              (* -- LIST OF ANGLES *)
  List[D1_,D2_,D3_],              (* -- LIST OF DISPLACEMENTS *)
  FACTOR_:List[Infinity,1]        (* -- FACTORATIVISTIC FACTORS *)
][
  CANONICAL_                      (* -- LIST OF CANONICAL VARIABLES *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,MAP},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  MAP = Join[
    I$DRQ3$MAP[-A3,FACTOR][CANONICAL],
    I$DRQ2$MAP[-A2,FACTOR][CANONICAL],
    I$DRQ1$MAP[-A1,FACTOR][CANONICAL],
    I$DTQ1$MAP[-D1,FACTOR][CANONICAL],
    I$DTQ2$MAP[-D2,FACTOR][CANONICAL],
    I$DTQ3$MAP[-D3,FACTOR][CANONICAL]
  ]  ;
  DeleteCases[MAP,CANONICAL]
] ;
ClearAll[I$ALIGNMENT$ENT$STATIC] ;
I$ALIGNMENT$ENT$STATIC::usage = "
I$ALIGNMENT$ENT$STATIC[List[A1,A2,A3],List[D1,D2,D3],List[FRG,FRB]] -- entrance alignment transformations (acts on thin representation)
" ;
I$ALIGNMENT$ENT$STATIC[           (* -- STRAIGHT BLOCK ENTRANCE ALIGNMENT TRANSFORMATIONS (STATIC) *)
  List[A1_,A2_,A3_],              (* -- LIST OF ANGLES *)
  List[D1_,D2_,D3_],              (* -- LIST OF DISPLACEMENTS *)
  FACTOR_:List[Infinity,1]        (* -- RELATIVISTIC FACTORS *)
][
  CANONICAL_                      (* -- LIST OF CANONICAL VARIABLES *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,Q4,P4,R1,R2,R3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  R1 = {{1,0,0},{0,Cos[A1],Sin[A1]},{0,-Sin[A1],Cos[A1]}} ;
  R2 = {{Cos[A2],0,Sin[A2]},{0,1,0},{-Sin[A2],0,Cos[A2]}} ;
  R3 = {{Cos[A3],Sin[A3],0},{-Sin[A3],Cos[A3],0},{0,0,1}} ;
  Q4 = 0 ;
  P4 = Times[Subtract[0,1],I$DRIF$HAMILTONIAN[FACTOR][CANONICAL]] ;
  {Q1,Q2,Q4} = Subtract[{Q1,Q2,Q4},{D1,D2,D3}] ;
  {Q1,Q2,Q4} = Dot[R3,R2,R1,{Q1,Q2,Q4}] ;
  {P1,P2,P4} = Dot[R3,R2,R1,{P1,P2,P4}] ;
  ReplaceAll[
    I$DRIF$MAP[FACTOR][{Q1,P1,Q2,P2,Q3,P3}],
    Rule[I$PARAMETER,Times[Subtract[0,1],Q4]]
  ]
] ;
ClearAll[I$ALIGNMENT$EXT$STATIC] ;
I$ALIGNMENT$EXT$STATIC::usage = "
I$ALIGNMENT$EXT$STATIC[List[A1,A2,A3],List[D1,D2,D3],List[FRG,FRB]] -- exit alignment transformations (acts on thin representation)
" ;
I$ALIGNMENT$EXT$STATIC[           (* -- STRAIGHT BLOCK EXIT ALIGNMENT TRANSFORMATIONS (STATIC) *)
  List[A1_,A2_,A3_],              (* -- LIST OF ANGLES *)
  List[D1_,D2_,D3_],              (* -- LIST OF DISPLACEMENTS *)
  FACTOR_:List[Infinity,1]        (* -- RELATIVISTIC FACTORS *)
][
  CANONICAL_                      (* -- LIST OF CANONICAL VARIABLES *)
] := Block[
  {Q1,P1,Q2,P2,Q3,P3,Q4,P4,R1,R2,R3},
  {Q1,P1,Q2,P2,Q3,P3} = CANONICAL ;
  R1 = {{1,0,0},{0,Cos[A1],-Sin[A1]},{0,Sin[A1],Cos[A1]}} ;
  R2 = {{Cos[A2],0,-Sin[A2]},{0,1,0},{Sin[A2],0,Cos[A2]}} ;
  R3 = {{Cos[A3],-Sin[A3],0},{Sin[A3],Cos[A3],0},{0,0,1}} ;
  Q4 = 0 ;
  P4 = Times[Subtract[0,1],I$DRIF$HAMILTONIAN[FACTOR][CANONICAL]] ;
  {Q1,Q2,Q4} = Dot[R1,R2,R3,{Q1,Q2,Q4}] ;
  {Q1,Q2,Q4} = Plus[{Q1,Q2,Q4},{D1,D2,D3}] ;
  {P1,P2,P4} = Dot[R1,R2,R3,{P1,P2,P4}] ;
  ReplaceAll[
    I$DRIF$MAP[FACTOR][{Q1,P1,Q2,P2,Q3,P3}],
    Rule[I$PARAMETER,Times[Subtract[0,1],Q4]]
  ]
] ;
(* ################################################################################################################################################################ *)
(* COMPLETE TWISS LIST (MADX) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TWISS$DATA] ;
I$TWISS$DATA::usage = "
I$TWISS$DATA -- complete list of valid twiss columns
" ;
I$TWISS$DATA := List["NAME","KEYWORD","S","BETX","ALFX","MUX","BETY","ALFY","MUY","X","PX","Y","PY","T","PT","DX","DPX","DY","DPY","WX","PHIX","DMUX","WY","PHIY","DMUY","DDX","DDPX","DDY","DDPY","R11","R12","R21","R22","ENERGY","L","ANGLE","K0L","K0SL","K1L","K1SL","K2L","K2SL","K3L","K3SL","K4L","K4SL","K5L","K5SL","K6L","K6SL","K7L","K7SL","K8L","K8SL","K9L","K9SL","K10L","K10SL","K11L","K11SL","K12L","K12SL","K13L","K13SL","K14L","K14SL","K15L","K15SL","K16L","K16SL","K17L","K17SL","K18L","K18SL","K19L","K19SL","K20L","K20SL","KSI","HKICK","VKICK","TILT","E1","E2","H1","H2","HGAP","FINT","FINTX","VOLT","LAG","FREQ","HARMON","SLOT_ID","ASSEMBLY_ID","MECH_SEP","V_POS","BBCHARGE","XMA","YMA","SIGX","SIGY","LRAD","PARENT","COMMENTS","RE11","RE12","RE13","RE14","RE15","RE16","RE21","RE22","RE23","RE24","RE25","RE26","RE31","RE32","RE33","RE34","RE35","RE36","RE41","RE42","RE43","RE44","RE45","RE46","RE51","RE52","RE53","RE54","RE55","RE56","RE61","RE62","RE63","RE64","RE65","RE66","KMAX","KMIN","CALIB","POLARITY","ALFA","BETA11","BETA12","BETA13","BETA21","BETA22","BETA23","BETA31","BETA32","BETA33","ALFA11","ALFA12","ALFA13","ALFA21","ALFA22","ALFA23","ALFA31","ALFA32","ALFA33","GAMA11","GAMA12","GAMA13","GAMA21","GAMA22","GAMA23","GAMA31","GAMA32","GAMA33","BETA11P","BETA12P","BETA13P","BETA21P","BETA22P","BETA23P","BETA31P","BETA32P","BETA33P","ALFA11P","ALFA12P","ALFA13P","ALFA21P","ALFA22P","ALFA23P","ALFA31P","ALFA32P","ALFA33P","GAMA11P","GAMA12P","GAMA13P","GAMA21P","GAMA22P","GAMA23P","GAMA31P","GAMA32P","GAMA33P","DISP1","DISP2","DISP3","DISP4","DISP1P","DISP2P","DISP3P","DISP4P","DISP1P2","DISP2P2","DISP3P2","DISP4P2","DISP1P3","DISP2P3","DISP3P3","DISP4P3","MU1","MU2","MU3","SIG11","SIG12","SIG13","SIG14","SIG15","SIG16","SIG21","SIG22","SIG23","SIG24","SIG25","SIG26","SIG31","SIG32","SIG33","SIG34","SIG35","SIG36","SIG41","SIG42","SIG43","SIG44","SIG45","SIG46","SIG51","SIG52","SIG53","SIG54","SIG55","SIG56","SIG61","SIG62","SIG63","SIG64","SIG65","SIG66","N1"] ;
(* ################################################################################################################################################################ *)
(* CUSTOM TWISS LIST (MADX) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TWISS$LIST] ;
I$TWISS$LIST::usage = "
I$TWISS$LIST -- default custom twiss list (list of selected columns for twiss)
" ;
I$TWISS$LIST := List["NAME","KEYWORD","L","ANGLE","K1L","K2L","K3L","E1","E2","TILT"] ;
(* ################################################################################################################################################################ *)
(* MAKE TFS FILE (MADX) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$TFS] ;
I$MAKE$TFS::usage = "
I$MAKE$TFS[FILE,SEQUENCE,OUTPUT] -- generate MADX tfs file for given lattice file <FILE> (string) with defined sequence <SEQUENCE> (string) and output file name <OUTPUT>
" ;
Options[I$MAKE$TFS] = List[
  Rule["BEAM",""],                (* -- BEAM COMMAND (STRING), DEFAULT "beam,particle=electron,gamma=100000;\n" *)
  Rule["FORMAT",""],              (* -- TFS FORMAT (STRING), DEFAULT "set,format=\"20.20f\",\"-20s\";\n" *)
  Rule["SELECT",True],            (* -- SELECT FLAG (LOGICAL), COLUMNS DEFINED BY I$TWISS$LIST ARE USED FOR 'True' *)
  Rule["START",""],               (* -- SEQUENCE START (STRING) *)
  Rule["TWISS",""],               (* -- TWISS COMMAND (STRING), DEFAULT \"twiss;\n\" *)
  Rule["RUN",True],               (* -- RUN MADX (LOGICAL) *)
  Rule["CLEAN",False],            (* -- CLEAN INTERMEDIATE FILES (LOGICAL) *)
  Rule["VERBOSE",False]           (* -- VERBOSE FLAG (LOGICAL) *)
] ;
I$MAKE$TFS[                       (* -- GENERATE TFS DATA *)
  FILE_String,                    (* -- INPUT FILE NAME (STRING) *)
  SEQUENCE_String,                (* -- SEQUENCE NAME (STRING) *)
  OUTPUT_String,                  (* -- OUTPUT (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {CALL,BEAM,USE,FORMAT,TWISS,SELECT,START,WRITE,MADX},
    CALL = StringTemplate["call,file=\"`1`\";\n"][FILE] ;
    BEAM = OptionValue["BEAM"] ;
    BEAM = If[UnsameQ[BEAM,""],BEAM,"beam,particle=electron,gamma=100000;\n"] ;
    FORMAT = OptionValue["FORMAT"] ;
    FORMAT = If[UnsameQ[FORMAT,""],FORMAT,"set,format=\"20.20f\",\"-20s\";\n"] ;
    TWISS = I$TWISS$LIST ;
    If[Not[MemberQ[I$TWISS$DATA,Apply[Alternatives,TWISS]]],Throw[$Failed]] ;
    SELECT = If[OptionValue["SELECT"],StringJoin["select,flag=twiss,clear;\n",StringTemplate["select,flag=twiss,column=``;\n",Rule[InsertionFunction,Composition[ToLowerCase,Curry[StringRiffle][","]]]][TWISS]],""] ;
    START = OptionValue["START"] ;
    START = If[SameQ[START,""],START,StringJoin[StringTemplate["seqedit,sequence=``;\nflatten;\n"][SEQUENCE],StringTemplate["cycle,start=``;\n"][ToLowerCase[START]],"endedit;\n"]] ;
    USE = StringTemplate["use,sequence=`1`;\n"][SEQUENCE] ;
    TWISS = OptionValue["TWISS"] ;
    TWISS = If[UnsameQ[TWISS,""],TWISS,"twiss;\n"] ;
    WRITE = StringTemplate["write,table=twiss,file=\"`1`\";\n"][OUTPUT] ;
    MADX = StringJoin["!run.madx\n",CALL,BEAM,FORMAT,SELECT,START,USE,TWISS,WRITE] ;
    MADX = ToLowerCase[MADX] ;
    Export["run.madx",MADX,"String"] ;
    If[OptionValue["VERBOSE"],Print[MADX]] ;
    If[
      OptionValue["RUN"],
      MADX = RunProcess[List["madx","run.madx"],"StandardOutput"] ;
      If[OptionValue["VERBOSE"],Print[MADX]] ;
    ] ;
    If[OptionValue["CLEAN"],Run["rm run.madx"]] ;
    OUTPUT
  ]
] ;
(* ################################################################################################################################################################ *)
(* IMPORT TFS FILE (TWISS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$IMPORT$TFS]
I$IMPORT$TFS::usage = "
I$IMPORT$TFS[FILE] -- import tfs <FILE> (string)
" ;
Options[I$IMPORT$TFS] = List[
  Rule["MERGE",DeleteDuplicates]  (* -- MERGE FUNCTION *)
] ;
I$IMPORT$TFS[                     (* -- IMPORT TFS FILE *)
  FILE_String,                    (* -- NAME (STRING) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {DATA,LIST,COLUMN,SEQUENCE,TYPE},
    DATA = Import[FILE,"Table"] ;
    LIST = Cases[DATA,List["@",___]] ;
    LIST = Map[Rest,LIST] ;
    LIST = List[Map[First,LIST],Map[Last,LIST]] ;
    LIST = Association[Thread[Apply[Rule,LIST]]] ;
    LIST = Association[Rule["PARAMETER",LIST]] ;
    DATA = DeleteCases[DATA,List["@",___]] ;
    COLUMN = Rest[First[DATA]] ;
    If[SameQ[Flatten[Position[COLUMN,"KEYWORD"]],List[]],Throw[$Failed]] ;
    COLUMN = Association[Rule["COLUMN",Rest[COLUMN]]] ;
    DATA = Drop[DATA,Plus[1,1]] ;
    SEQUENCE = Map[First,DATA] ;
    TYPE = Sort[DeleteDuplicates[Map[Composition[First,Rest],DATA]]] ;
    TYPE = Association[Rule["TYPE",TYPE]] ;
    DATA = List[SEQUENCE,Map[Rest,DATA]] ;
    DATA = Merge[OptionValue["MERGE"]][Thread[Apply[Rule,DATA]]] ;
    DATA = Association[Rule["ELEMENT",DATA]] ;
    SEQUENCE = Association[Rule["SEQUENCE",SEQUENCE]] ;
    Join[LIST,COLUMN,TYPE,SEQUENCE,DATA]
  ]
] ;
(* ################################################################################################################################################################ *)
(* MAKE LATTICE FILES (LINE AND DEFINITIONS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$MAKE$LAT] ;
I$MAKE$LAT::usage = "
I$MAKE$LAT[NAME,TABLE,MASK] -- generate lattice file <NAME>.lat for given output name <NAME> (string), twiss table data <TABLE> and type rules <MASK>
" ;
Options[I$MAKE$LAT] = List[
  Rule["RENAME",List[]]           (* -- RENAME RULES FOR TYPES (LIST OF STRING RULES), E.G. List["QUADRUPOLE","QUAD"] *)
] ;
I$MAKE$LAT[                       (* -- MAKE LATTICE FILES (USE Get[] TO LOAD <NAME>.lin AND <NAME>.def) *)
  NAME_String,                    (* -- FILE NAME WITHOUT EXTENSION (STRING) *)
  TABLE_Association,              (* -- TABLE (ASSOCIATION), OUTPUT OF I$IMPORT$TFS[] FUNCTION *)
  MASK_Association,               (* -- TYPE RULES (LIST FOR EACH TYPE IN TABLE) *)
  OPTIONS:OptionsPattern[]        (* <MASK> = Association[...,<RULE_I>,...] *)
                                  (* <RULE_I> = <TYPE> \[Rule] {0/1,0/1,...}, SAME LENGTH AS # OF COLUMNS IN TABLE *)
                                  (* <TYPE> -- ELEMENT TYPE (STRING) *)
                                  (* 0/1 -- PARAMETER PRESENT/ABSENT FOR EACH TWISS COLUMN (POSITION FOR KEYWORD SHOULD ALWAYS BE THE 1ST AND PRESENT) *)
] := Catch[
  Block[
    {COLUMN,LINE,DEFINITION,LEGEND},
    COLUMN = TABLE["COLUMN"] ;
    If[SameQ[Head[COLUMN],Missing],Throw[$Failed]] ;
    COLUMN = Rest[COLUMN] ;
    LINE = TABLE["SEQUENCE"] ;
    If[SameQ[Head[LINE],Missing],Throw[$Failed]] ;
    DEFINITION = TABLE["ELEMENT"] ;
    If[SameQ[Head[DEFINITION],Missing],Throw[$Failed]] ;
    DEFINITION = Map[First,DEFINITION] ;
    DEFINITION = KeyValueMap[
      Function[
        Block[
          {ELEMENT,DATA,TYPE,LIST,PARAMETER,LENGTH},
          ELEMENT = Slot[1] ;
          DATA = Slot[2] ;
          TYPE = First[DATA] ;
          LIST = MASK[TYPE] ;
          If[SameQ[Head[LIST],Missing],Throw[$Failed]] ;
          PARAMETER = Rest[DATA] ;
          LIST = Rest[LIST] ;
          If[UnsameQ[Length[PARAMETER],Length[LIST]],Throw[$Failed]] ;
          PARAMETER = Thread[List[Pick[COLUMN,LIST,1],Pick[PARAMETER,LIST,1]]] ;
          LENGTH = Flatten[Cases[PARAMETER,List["L",_]]] ;
          If[SameQ[LENGTH,List[]],Throw[$Failed]] ;
          LENGTH = Last[LENGTH] ;
          PARAMETER = Map[
            Function[
              Block[
                {KEY,VALUE},
                {KEY,VALUE} = Slot[1] ;
                If[
                  Or[
                    StringMatchQ[KEY,StringExpression["K",Repeated[DigitCharacter],"L"]],
                    StringMatchQ[KEY,StringExpression["K",Repeated[DigitCharacter],"SL"]],
                    SameQ[KEY,"KSI"]
                  ],
                  If[UnsameQ[LENGTH,N[0]],Divide[VALUE,LENGTH],VALUE],
                  VALUE
                ]
              ]
            ],
            PARAMETER
          ] ;
          TYPE = StringReplace[TYPE,OptionValue["RENAME"]] ;
          Rule[ELEMENT,Flatten[List[TYPE,PARAMETER]]]
        ]
      ],
      DEFINITION
    ] ;
    DEFINITION = Association[DEFINITION] ;
    LEGEND = TABLE["COLUMN"] ;
    LEGEND = Map[Function[Pick[LEGEND,Slot[1],1]],MASK] ;
    LEGEND = Map[Rest,LEGEND] ;
    LEGEND = Merge[List[LEGEND,Map[Length,LEGEND]],Identity] ;
    DEFINITION = SortBy[DEFINITION,First] ;
    DEFINITION = Join[
      Association[Rule["LEGEND",SortBy[LEGEND,First]]],
      Association[Rule["ELEMENT",Keys[DEFINITION]]],
      Association[Rule["DEFINITION",DEFINITION]],
      Association[Rule["LINE",LINE]]
    ] ;
    Put[N[DEFINITION],StringTemplate["`1`.lat"][NAME]] ;
    StringTemplate["`1`.lat"][NAME]
  ]
] ;
(* ################################################################################################################################################################ *)
(* CONVERT LATTICE TO ELEGANT *)
(* ################################################################################################################################################################ *)
ClearAll[I$RULE$LTE$TYPES] ;
I$RULE$LTE$TYPES::usage = "
I$RULE$LTE$TYPES -- ELEGANT rules for type names
" ;
I$RULE$LTE$TYPES := List[
  Rule["DRIFT","DRIF"],
  Rule["MARKER","MONI"],
  Rule["QUADRUPOLE","QUAD"],
  Rule["SEXTUPOLE","SEXT"],
  Rule["OCTUPOLE","OCTU"],
  Rule["SBEND","SBEN"],
  Rule["RBEND","RBEN"],
  Rule["SOLENOID","SOLE"],
  Rule["MULTIPOLE","MULT"],
  Rule["HKICKER","DRIF"],
  Rule["KICKER","DRIF"],
  Rule["RFCAVITY","DRIF"],
  Rule["HMONITOR","MONI"],
  Rule["VMONITOR","MONI"],
  Rule["MONITOR","MONI"],
  Rule["INSTRUMENT","DRIF"]
] ;
ClearAll[I$RULE$LTE$PARAMETERS] ;
I$RULE$LTE$PARAMETERS::usage = "
I$RULE$LTE$PARAMETERS -- ELEGANT rules for parameter names
" ;
I$RULE$LTE$PARAMETERS := List[
  Rule["K0L","K0"],
  Rule["K1L","K1"],
  Rule["K2L","K2"],
  Rule["K3L","K3"],
  Rule["K4L","K4"],
  Rule["K5L","K5"],
  Rule["K6L","K6"],
  Rule["K7L","K7"],
  Rule["K8L","K8"],
  Rule["K9L","K9"],
  Rule["KSI","KS"]
] ;
ClearAll[I$MAKE$LTE] ;
I$MAKE$LTE::usage = "
I$MAKE$LTE[NAM,LIN,DEF] -- generate ELEGANT file with name <NAME> (string) for given lattice <LATTICE> (output of I$MAKE$LAT[] function)
" ;
I$MAKE$LTE[                       (* -- MAKE ELEGANT FILE *)
  NAME_String,                    (* -- FILE NAME WITHOUT EXTENSION (STRING) *)
  LATTICE_Association             (* -- LATTICE (OUTPUT OF I$MAKE$LAT[] FUNCTION) *)
] := Block[
  {SEQUENCE,LEGEND,ELEMENT},
  SEQUENCE = StringJoin[
    StringTemplate["``: line=("][NAME],
    StringTemplate["``)\n",Rule[InsertionFunction,Composition[ToLowerCase,Curry[StringRiffle][",&\n"]]]][LATTICE["LINE"]]
  ] ;
  LEGEND = LATTICE["LEGEND"] ;
  ELEMENT = LATTICE["DEFINITION"] ;
  ELEMENT = KeyValueMap[
    Function[
      Block[
        {KEY,VALUE,TYPE},
        KEY = Slot[1] ;
        KEY = ToLowerCase[KEY] ;
        VALUE = Slot[2] ;
        TYPE = First[VALUE] ;
        VALUE = Rest[VALUE] ;
        VALUE = Map[Composition[Curry[StringDelete][StringTemplate["_``"][I$FORTRAN$REAL$KIND]],I$FORTRAN$REAL],VALUE] ;
        VALUE = Transpose[List[First[LEGEND[TYPE]], VALUE]] ;
        VALUE = Map[Curry[StringRiffle]["="], VALUE] ;
        VALUE = ToLowerCase[StringRiffle[StringReplace[VALUE,I$RULE$LTE$PARAMETERS],", "]] ;
        TYPE = StringReplace[TYPE,I$RULE$LTE$TYPES] ;
        StringTemplate["`1`: `2`, `3`\n",Rule[InsertionFunction, ToLowerCase]][KEY,TYPE,VALUE]
      ]
    ],
    ELEMENT
  ] ;
  Export[StringTemplate["``.lte"][NAME],StringJoin[ELEMENT,SEQUENCE],"String"]
] ;
(* ################################################################################################################################################################ *)
(* TRACK (NUMBER OF TURNS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$TRACK] ;
I$TRACK::usage = "
I$TRACK[TUNE,INITIAL,MAP] -- track initial condition <INITIAL> for given number of turns <TUNR> (integer) and map <MAP> (function) with internal aperture
$TRACK[TUNE,INITIAL,MAP,APERTURE] -- track initial condition <INI> for given number of turns <TUN> (integer), map <MAP> (function) with or without internal aperture and external aperture <APERTURE> (function)
" ;
I$TRACK[                          (* -- TRACKING (INTERNAL APERTURE) *)
  TUNE_,                          (* -- NUMBER OF TURNS TO TRACK (INTEGER) *)
  INITIAL_,                       (* -- INITIAL CONDITION (STATE) *)
  MAP_                            (* -- MAP TO TRACK (FUNCTION) *)
] := Block[
  {LOOP,STATE},
  LOOP = 0 ;
  STATE = INITIAL ;
  While[
    And[Less[LOOP,TUNE],Greater[First[STATE],0.5]],
    STATE = MAP[STATE] ;
    LOOP+= 1 ;
  ] ;
  If[Greater[First[STATE],0.5],LOOP,Subtract[LOOP,1]]
] ;
I$TRACK[                          (* -- TRACKING (INTERNAL & EXTERNAL APERTURE) *)
  TUNE_,                          (* -- NUMBER OF TURNS TO TRACK (INTEGER) *)
  INITIAL_,                       (* -- INITIAL CONDITION (STATE) *)
  MAP_,                           (* -- MAP TO TRACK (FUNCTION) *)
  APERTURE_                       (* -- ARERTURE FUNCTION (LOGICAL FUNCTION) *)
] := Block[
  {LOOP,STATE},
  LOOP = 0 ;
  STATE = INITIAL ;
  While[
    And[Less[LOOP,TUNE],APERTURE[STATE],Greater[First[STATE],0.5]],
    LOOP+= 1 ;
    STATE = MAP[STATE] ;
  ] ;
  LOOP
] ;
(* ################################################################################################################################################################ *)
(* GENERATION OF LINES (TRANSVERSE PLANE) *)
(* ################################################################################################################################################################ *)
ClearAll[I$LINE] ;
I$LINE::usage = "
I$LINE[LENGTH,RADIUS,DISTANCE] -- generate <LENGTH> (integer) lines with maximal radius <RADIUS> (real) with distance between points <DISTANCE> (real)
" ;
I$LINE[                           (* -- INITIAL LINES GENERATION *)
  LENGTH_,                        (* -- NUMBER OF LINES (INTEGER) *)
  RADIUS_,                        (* -- OUTER RADIUS (REAL) *)
  DISTANCE_                       (* -- DISTANCE BETWEEN POINTS ON A LINE (REAL) *)
] := Catch[
  Block[
    {POINT,ANGLE},
    POINT = N[Chop[Transpose[Map[Function[N[CirclePoints[List[Slot[1],0.0],LENGTH]]],Reverse[Range[RADIUS,DISTANCE,Subtract[0,DISTANCE]]]]]]] ;
    ANGLE = Map[Composition[Apply[ArcTan],Last],POINT] ;
    ANGLE = Mod[ANGLE,Times[2,Pi]] ;
    First[Transpose[SortBy[Transpose[List[POINT,ANGLE]],Last]]]   
  ]
] ;
(* ################################################################################################################################################################ *)
(* GENERATE FULL INITIAL CONDITION *)
(* ################################################################################################################################################################ *)
ClearAll[I$INITIAL] ;
I$INITIAL::usage = "
I$INITIAL[SIGNATURE,INITIAL,LINE] -- generate full initial conditions for given planes signature <SIGNATURE>, initial conditions for other state variables <INITIAL> and lines <LINE>
" ;
I$INITIAL[                        (* -- GENERATE FULL INTIAL CONDITIONS *)
  SIGANTURE_,                     (* -- PLANES SIGNATURE (USE NON-ZERO VALUES FOR TRANSVERSE COORDINATE PLANES AND ZERO VALUES FOR OTHER COMPONENTS) *)
  INITIAL_,                       (* -- LIST OF INITIAL CONDITIONS FOR OTHER PLANES *)
  LINE_                           (* -- LINES *)
] := Block[
  {POSITION},
  POSITION = Flatten[Position[SIGANTURE,0]] ;
  Map[Map[Function[I$INSERT[Slot[1],INITIAL,POSITION]]],LINE]
] ;
(* ################################################################################################################################################################ *)
(* APERTURE SEARCH (ACTS ON A LIST OF LINES WITH FULL INITIAL CONDITIONS) *)
(* ################################################################################################################################################################ *)
ClearAll[I$APERTURE$SEARCH] ;
I$APERTURE$SEARCH::usage = "
I$APERTURE$SEARCH[TUNE,MAP,LINE] -- perform aperture search for given number of turns <TUNE> (integer), map (with internal aperture) <MAP> (function) and list of lines <LINE>
I$APERTURE$SEARCH[TUNE,MAP,LINE,APERTURE] -- perform aperture search for given number of turns <TUNE> (integer), map (with or without internal aperture) <MAP> (function), list of lines <LINE> and external apetrure function <APERTURE> (function)
" ;
Options[I$APERTURE$SEARCH] := List[
  Rule["MAP",I$PARALLEL$MAP]      (* -- MAP (OVER LINES) FUNCTION *)
] ;
I$APERTURE$SEARCH[                (* -- APERTURE SEARCH *)
  TUNE_,                          (* -- NUMBER OF MAP ITERATIONS (INTEGER) *)
  MAP_,                           (* -- MAP (FUNCTION) *)
  INITIAL_,                       (* -- LINES (WITH FULL INITIAL CONDITIONS) *)
  APERTURE_:Null,                 (* -- EXTERNAL APERTURE FUNCTION (LOGICAL FUNCTION) *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {TASK,LIST,RESULT},
    (* SET TRACKING FUNCTION *)
    TASK = If[
      SameQ[APERTURE,Null],
      Function[I$TRACK[TUNE,Slot[1],MAP]],
      Function[I$TRACK[TUNE,Slot[1],MAP,APERTURE]]
    ] ;
    (* TRACK LINES AND RETURN *)
    LIST = OptionValue["MAP"][
      Function[
        Catch[
          MapIndexed[
            Function[
              Block[
                {LENGTH,NUMBER},
                LENGTH = TASK[Slot[1]] ;
                NUMBER = First[Slot[2]] ;
                If[Less[LENGTH,TUNE],Throw[NUMBER]] ;
                NUMBER
              ]
            ],
            Slot[1]
          ]
        ]
      ],
      INITIAL
    ] ;
    LIST = Map[Function[If[SameQ[Head[Slot[1]],List],Last[Slot[1]],Subtract[Slot[1],1]]],LIST] ;
    RESULT = MapThread[Part,List[Pick[INITIAL,Unitize[LIST],1],DeleteCases[LIST,0]]] ;    
    RESULT
  ]
] ;
(* ################################################################################################################################################################ *)
(* PAIRWISE DISTANCE *)
(* ################################################################################################################################################################ *)
ClearAll[I$PAIRWISE$DISTANCE] ;
I$PAIRWISE$DISTANCE::usage = "
I$PAIRWISE$DISTANCE[SIGNATURE,DATA] -- return pairwise distance for planes signature <SIGNATURE> and dynamic aperture data <DATA>
" ;
I$PAIRWISE$DISTANCE[              (* -- RETURN PAIR WISE DISTANCE (SORTED) *)
  SIGNATURE_,                     (* -- PLANES SIGNATURE *)
  DATA_                           (* -- DA  *)
] := Block[
  {DISTANCE},
  DISTANCE = Map[Function[Pick[Slot[1],SIGNATURE,1]],DATA] ;
  Sort[Map[Apply[EuclideanDistance],Transpose[List[Most[DISTANCE],Rest[DISTANCE]]]]]
] ;
(* ################################################################################################################################################################ *)
(* REFINE DA FRINGE *)
(* ################################################################################################################################################################ *)
ClearAll[I$APERTURE$FRINGE] ;
I$APERTURE$FRINGE::usage = "
I$APERTURE$FRINGE[SIGNATURE,FACTOR,RADIUS,DISTANCE,TUNE,INITIAL,MAP] -- refine aperture fringe for given plane signature <SIGNATURE>, distance factor <FACTOR>, outer radius <RADIUS>, distance <DISTANCE>, number of iterations <TUNE>, initial aperture <INITIAL> and map <MAP> 
" ;
Options[I$APERTURE$FRINGE] := List[
  Rule["MAP",I$PARALLEL$MAP],
  Rule["APERTURE",Null]
] ;
I$APERTURE$FRINGE[                (* -- ADD POINTS TO DA *)
  SIGNATURE_,                     (* -- PLANES SIGNATURE *)
  FACTOR_,                        (* -- MULTIPLICATION FACTOR FOR MAX DISTANCE *)
  RADIUS_,                        (* -- OUTER RADIUS *)
  DISTANCE_,                      (* -- DISTANCE BETWEEN POINTS ON A LINE *)
  TUNE_,                          (* -- NUMBER OF TURNS TO TRACK *)
  INITIAL_,                       (* -- DA *)
  MAP_,                           (* -- MAP *)
  OPTIONS:OptionsPattern[]        (* -- OPTION(S) *)
] := Catch[
  Block[
    {TASK,LINE,NUMBER,MAX,DELTA,INPUT,INDEX,OUT,ANGLE},
    TASK = If[
      SameQ[OptionValue["APERTURE"],Null],
      Function[I$APERTURE$SEARCH[TUNE,MAP,Slot[1]]],
      Function[I$APERTURE$SEARCH[TUNE,MAP,Slot[1],OptionValue["APERTURE"]]]
    ] ;
    MAX = Times[FACTOR,Max[I$PAIRWISE$DISTANCE[SIGNATURE,INITIAL]]] ;
    LINE = INITIAL ;
    DELTA = LINE ;
    DELTA = Map[Function[Pick[Slot[1],SIGNATURE,1]],DELTA] ;
    DELTA = Map[Apply[EuclideanDistance],Transpose[List[Most[DELTA],Rest[DELTA]]]] ;
    NUMBER = Length[INITIAL] ;
    INDEX = Plus[Most[Range[NUMBER]],0.5] ;
    LINE = Map[Interpolation[MapThread[List,List[Range[NUMBER],Map[Function[Pick[Slot[1],SIGNATURE,1]],LINE]]],Rule[InterpolationOrder,1]],INDEX] ;
    LINE = Transpose[List[INDEX,LINE,DELTA]] ;
    LINE = Select[LINE,Function[GreaterEqual[Last[Slot[1]],MAX]]] ;
    If[SameQ[LINE,List[]],Throw[INITIAL]] ;
    List[INDEX,LINE] = Most[Transpose[LINE]] ;
    INPUT = Pick[First[INITIAL],SIGNATURE,0] ;
    LINE = Map[Composition[Apply[ArcTan]],LINE] ;
    LINE = Outer[Function[Flatten[N[CirclePoints[List[Slot[2],Slot[1]],1]]]],LINE,Reverse[Range[RADIUS,DISTANCE,Subtract[0,DISTANCE]]]] ;
    OUT = LINE ;
    LINE = I$INITIAL[SIGNATURE,INPUT,LINE] ;
    LINE = TASK[LINE] ;
    LINE = Join[INITIAL,LINE] ;
    ANGLE = Map[Composition[Apply[ArcTan],Function[Pick[Slot[1],SIGNATURE,1]]],LINE] ;
    ANGLE = Mod[ANGLE,Times[2,Pi]] ;
    List[First[Transpose[SortBy[Transpose[List[LINE,ANGLE]],Last]]],OUT]
  ]
] ;
(* ################################################################################################################################################################ *)
(* REFINE DA LENGTH *)
(* ################################################################################################################################################################ *)
ClearAll[I$APERTURE$REFINE] ;
I$APERTURE$REFINE::usage = "
I$APERTURE$REFINE[SIGNATURE,TUN,INI,MAP,LEN,ERR,LIM,APR]
" ;
I$APERTURE$REFINE[                (* -- REFINE DA *)
  SIGNATURE_,                     (* -- PLANES SIGNATURE *)
  TUNE_,                          (* -- NUMBER OF TURNS *)
  INITIAL_,                       (* -- INITIAL CONDITION (SINGLE POINT) *)
  MAP_,                           (* -- MAP TO TRACK *)
  LENGTH_,                        (* -- SEARCH INTERVAL LENGTH *)
  ERROR_,                         (* -- NORM ERROR STOPPING CONDITION *)
  LIMIT_,                         (* -- ITERATIONS LIMIT STOPPING CONDITION *)
  APERTURE_:Null                  (* -- APERTURE FUNCTION (EXTERNAL) *)
] := Block[
  {LINE,INPUT,ANGLE,RADIUS,DIRECTION,FUC,RULE,FUN,LOOP,NORM,ARGUMENT,FST,LST,DELTA,OUT},
  LINE = Extract[INITIAL,Position[SIGNATURE,1]] ;
  INPUT = Extract[INITIAL,Position[SIGNATURE,0]] ;
  ANGLE = Apply[ArcTan,LINE] ;
  RADIUS = Norm[LINE] ;
  RULE = Thread[Rule[Flatten[Position[SIGNATURE,0]],INPUT]] ;
  FUC = Function[I$INSERT[First[N[CirclePoints[List[Slot[1],ANGLE],1]]],ConstantArray[0,Length[INPUT]],Map[First,RULE]]] ;
  RULE = Thread[Rule[Flatten[Position[SIGNATURE,0]],INPUT]] ;
  FUN = Composition[Function[ReplacePart[Slot[1],RULE]],FUC] ;
  DIRECTION = If[
    UnsameQ[APERTURE,Null],
    Function[If[SameQ[I$TRACK[TUNE,FUN[Slot[1]],MAP,APERTURE],TUNE],True,False]],
    Function[If[SameQ[I$TRACK[TUNE,FUN[Slot[1]],MAP],TUNE],True,False]]
  ] ;
  LOOP = 0 ;
  NORM = 2*ERROR ;
  DELTA = LENGTH ;
  ARGUMENT = FST = LST = DIRECTION[RADIUS] ;
  While[
    And[LessEqual[LOOP,LIMIT],GreaterEqual[DELTA,ERROR]],
    If[
      ARGUMENT,
      List[
        OUT = RADIUS ;
        RADIUS = RADIUS+DELTA ;
        LST = ARGUMENT ;
        If[UnsameQ[FST,LST],List[FST,DELTA] = List[LST,DELTA/2]] ;
      ],
      List[
        RADIUS = RADIUS-DELTA ;
        LST = ARGUMENT ;
        If[UnsameQ[FST,LST],List[FST,DELTA] = List[LST,DELTA/2]] ;
      ]
    ] ;
    LOOP+=1 ;
    ARGUMENT = DIRECTION[RADIUS] ;
  ] ;
  FUN[OUT]      
] ;
(* ################################################################################################################################################################ *)
(* DA POLYGON AREA *)
(* ################################################################################################################################################################ *)
ClearAll[I$POLYGON$AREA] ;
I$POLYGON$AREA::usage = "
I$POLYGON$AREA[SIGNATURE,DATA] -- DA plolygon area for given plane signature <SIGNATURE> and DA data <DATA>
" ;
I$POLYGON$AREA[                   (* -- DA AREA *)
  SIGNATURE_,                     (* -- SIGNATURE *)
  DATA_                           (* -- DA DATA *)
] := Area[Polygon[Map[Function[Pick[Slot[1],SIGNATURE,1]],DATA]]] ;
