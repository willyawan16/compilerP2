%{
#define ST_LENGTH 2
#define MAX_LINE_LENGTH 256
#define NUMBER_OF_ST 100
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdbool.h>
#include<math.h>
#define Trace(t)        printf(t)
int yylex(void);
int yyerror(char *s);

extern char* yytext;
char funcNameBuf[MAX_LINE_LENGTH];



union Value {
    int     integer;
    double  real;
    char*   string;
    bool    boolean;
};

struct MultiValue {
    int type; // 1: int 2: real 3: string 4: bool
    union Value value;
};

enum ValueType { // or DECLARATION TYPE
    VALNONE,
	VALINT,
	VALREAL,
	VALSTR,
	VALBOOL
};

enum StoreType {
    NONE,
	CONSTANT,
	VARIABLE,
    MULTIVAR, // ARRAY
    FUNC,
    PROC
};

struct ArrayAtr {
    int startIndex;
    int capacity;
};

struct SymbolTable {
    int index;
    char** strList;
	enum ValueType* valueType;
	enum StoreType* storeType;
    struct ArrayAtr* arrayAtr;
    size_t capacity;
    size_t size;
    struct SymbolTable* parent;
};

struct SymbolTable** Col_ST;
int currentSize;
int currentTable;

struct SymbolTable symbols;

void create();
void createChildTable();
int lookup(char* s);
int insert(char* s);
void dump();
void printSymbols();
void returnToParent();
void initFlags();
enum ValueType valTypeBuffer = VALINT;
enum ValueType constExpTypeBuffer = VALINT;

// Flags
bool funcScope;
bool procScope;
%}

%union {
    int     ival;
    double  dval;
    char*   sval;
    bool    bval;
    struct {
        int type;
        union {
            int     uint;
            double  ureal;
            char*   ustr;
            bool    ubool;
        } value;
    } multival;
};

/* tokens */
%token ARRAY BEGINT CHAR BOOL CONST DECREASING DEFAULT DO ELSE END EXIT FOR FUNCTION GET IF LOOP OF PUT PROCEDURE RESULT RETURN SKIP THEN VAR WHEN
%token RELOPL RELOPLT RELOPG RELOPGT RELOPEQ AND OR NOT
%token DOT COMMA COLON SEMICOLON POPEN PCLOSE SOPEN SCLOSE BOPEN BCLOSE
%token ADD SUB MUL DIV MOD
%token ASSIGN
%token NEWLINE

%token <sval> STRING
%token <ival> NUMBER INT ID
%token <dval> REAL
%token <bval> TRUE FALSE

%type <bval> bool_exp
%type <multival> exp
%type <ival> array_ref


%left OR
%left AND
%right NOT
%left RELOPLT RELOPLE RELOPGT RELOPGE RELOPEQ RELOPNE
%left ADD SUB
%left MUL DIV MOD
%nonassoc UMINUS

%start program
%%
program: /* empty */
    | decls
    | stmts
    | decls stmts
    ;
    
newline: /* empty */ | NEWLINE

decls: /* empty */
    | decl decls 
    ;

stmts: /* empty */ 
    | stmt stmts { Trace("Reducing to statements\n"); }
    ;

const_exp: INT          { 
                            constExpTypeBuffer = VALINT;
                            $<ival>$ = $1;
                        }
    | REAL              { 
                            constExpTypeBuffer = VALREAL;
                            $<dval>$ = $1;
                        }
    | STRING            { 
                            constExpTypeBuffer = VALSTR;
                            $<sval>$ = $1;
                        }
    | TRUE              { 
                            constExpTypeBuffer = VALBOOL;
                            $<bval>$ = $1;
                        }
    | FALSE             { 
                            constExpTypeBuffer = VALBOOL;
                            $<bval>$ = $1;
                        }
    ;

val_type: INT           { 
                            valTypeBuffer = VALINT;
                        }
    | REAL              { 
                            valTypeBuffer = VALREAL;
                        }
    | STRING            { 
                            valTypeBuffer = VALSTR;
                        }
    | BOOL              { 
                            valTypeBuffer = VALBOOL;
                        }
    ;

decl: var_decl | func_decl

var_decl: CONST ID ASSIGN const_exp             { 
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 

                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    // TODO: store value

                                                    printSymbols();
                                                } 
    | CONST ID COLON val_type ASSIGN const_exp  { 
                                                    if(valTypeBuffer != constExpTypeBuffer) {
                                                        return yyerror("Error: Different value type");
                                                    } 

                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    // TODO: store value

                                                    printSymbols();
                                                }
    | VAR ID ASSIGN const_exp                   {
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    // TODO: store value

                                                    printSymbols();
                                                }
    | VAR ID COLON val_type ASSIGN const_exp    {
                                                    if(valTypeBuffer != constExpTypeBuffer) {
                                                        return yyerror("Error: Different value type");
                                                    } 
                                                    
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    // TODO: store value
                                                    
                                                    printSymbols();
                                                }
    | VAR ID COLON val_type newline                    {
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    Col_ST[currentTable]->valueType[$2] = valTypeBuffer;

                                                    printSymbols();
                                                }
    | VAR ID COLON ARRAY INT DOT DOT INT OF val_type    {
                                                            if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                                return yyerror("Error: Identifier existed");
                                                            } 
                                                            
                                                            Col_ST[currentTable]->storeType[$2] = MULTIVAR;
                                                            Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                            
                                                            if($5 != 0 && $5 != 1) {
                                                                return yyerror("Error: Start index should be either 0 or 1");
                                                            }

                                                            if($5 > $8) {
                                                                return yyerror("Error: Invalid index range");
                                                            }

                                                            Col_ST[currentTable]->arrayAtr[$2].startIndex = $5;
                                                            Col_ST[currentTable]->arrayAtr[$2].capacity = $8 - $5 + 1;
                                                            // TODO: store value
                                                            printSymbols();
                                                        }
    ;

var_decls: /* empty */ 
    | var_decl var_decls
    ;

stmt_body: var_decls stmts ;

func_args: /* empty */
    | POPEN formal_args PCLOSE
    ;
formal_args: formal_arg nextFormal_args
    ;
nextFormal_args: /* empty */
    | COMMA formal_args
    ;
    
formal_arg: ID COLON val_type   { 
                                    
                                }
    ;

// TODO: Add new symbol table
//       return?
func_decl: 
    /* FUNCTION Declaration */
    FUNCTION ID     { 
                        if(Col_ST[currentTable]->storeType[$2] != NONE) {
                            return yyerror("Error: Identifier existed");
                        }
                            
                        Col_ST[currentTable]->storeType[$2] = FUNC;
                        Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                    } 
    func_args COLON val_type stmt_body END ID     {
                                            if($2 != $9) {
                                                return yyerror("Error: Function closure does not match the function name");
                                            }
                                        }
    /* PROCEDURE Declaration */
    | PROCEDURE ID  { 
                                                if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                    return yyerror("Error: Identifier existed");
                                                }
                                                
                                                Col_ST[currentTable]->storeType[$2] = PROC;
                                                Col_ST[currentTable]->valueType[$2] = VALNONE;
                                            } 
    func_args stmt_body END ID          {
                                            if($2 != $7) {
                                                return yyerror("Error: Function closure does not match the function name");
                                            }   
                                        }
    ;

stmt: block
    | simple
    | func_invoke
    | if_stmt
    | loop_stmt
    ;


block: BEGINT                   {
                                    createChildTable();
                                    printSymbols();
                                } 
    stmt_body END               {
                                    returnToParent();
                                    printSymbols();
                                }
    ;

// TODO: seperate RESULT and RETURN. define EXIT and SKIP 
    // add array reference
simple: ID ASSIGN exp               {
                                        struct SymbolTable* parentTable = NULL;
                                        // Check whether is initialized
                                        if(Col_ST[currentTable]->storeType[$1] == NONE) {
                                            int foundIndex;
                                            // Can be in its parent symbol table
                                            bool found = false;
                                            struct SymbolTable* parentTable = Col_ST[currentTable]->parent;
                                            while (!found) {
                                                if(parentTable == NULL) {
                                                    return yyerror("Error: Identifier is not assigned yet anywhere");
                                                }
                                                
                                                foundIndex = lookup(Col_ST[currentTable]->strList[$1]);
                                                if(foundIndex != -1) {
                                                    found = true;
                                                    break;
                                                }

                                                parentTable = parentTable->parent;
                                            }
                                            printf("FOUND in parent!!");
                                            if(parentTable->storeType[$1] != VARIABLE) {
                                                return yyerror("Error: Only variable type identifier can be assign with values");
                                            }

                                            // TODO: access table and do assignment
                                        }

                                        if(parentTable == NULL) {
                                            if(Col_ST[currentTable]->storeType[$1] != VARIABLE) {
                                                return yyerror("Error: Only variable type identifier can be assign with values");
                                            }

                                            // TODO: Assign to variable
                                            printf("TODO: Assign to variable\n");
                                            
                                            switch($<multival.type>3) {
                                                case (int)VALREAL:
                                                    printf("arth_exp: %lf\n", $<multival.value.ureal>3);
                                                    break;
                                                case (int)VALSTR:
                                                    printf("string: %s\n", $<multival.value.ustr>3);
                                                    break;  
                                                case (int)VALBOOL:
                                                    printf("bool_exp: %d\n", $<multival.value.ubool>3);
                                                    break;
                                            }
                                        }
                                    }
    | PUT exp                       {
                                        switch($<multival.type>2) {
                                            case (int)VALREAL:
                                                printf("arth_exp: %lf\n", $<multival.value.ureal>2);
                                                break;
                                            case (int)VALSTR:
                                                printf("string: %s\n", $<multival.value.ustr>2);
                                                break;  
                                            case (int)VALBOOL:
                                                printf("bool_exp: %d\n", $<multival.value.ubool>2);
                                                break;
                                        }
                                    }
    | GET ID                        {
                                        if(Col_ST[currentTable]->storeType[$2] == CONSTANT) {
                                            return yyerror("Error: Cannot process input to constant variables");
                                        }

                                        if(Col_ST[currentTable]->storeType[$2] == FUNC) {
                                            return yyerror("Error: Cannot process input to functions");
                                        }

                                        if(Col_ST[currentTable]->storeType[$2] == PROC) {
                                            return yyerror("Error: Cannot process input to procedures");
                                        }

                                        // Process input
                                        // TODO: THINK
                                    }
    | RESULT ID
    | RETURN
    | EXIT
    | SKIP
    ;

exp: POPEN exp PCLOSE           { 
                                    if($<multival.type>2 == (int)VALSTR) {
                                        return yyerror("Error: Parentheses cannot accept string");
                                    }

                                    if($<multival.type>2 == (int)VALREAL) {
                                        $<multival.value.ureal>$ = $<multival.value.ureal>2; 
                                        $<multival.type>$ = (int)VALREAL;
                                        // printf("%lf\n", $<multival.value.ureal>$);
                                    } else if ($<multival.type>2 == (int)VALBOOL) {
                                        $<multival.value.ubool>$ = $<multival.value.ubool>2; 
                                        $<multival.type>$ = (int)VALBOOL;
                                        // printf("%d\n", $<multival.value.ubool>$);
                                    }
                                    
                                }   
    | arth_exp                  { 
                                    if($<multival.type>1 != (int)VALREAL) {
                                        return yyerror("Error: Arithmethic expression only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = $<multival.value.ureal>1;
                                    $<multival.type>$ = (int)VALREAL;
                                    // printf("%lf\n", $<multival.value.ureal>$);

                                }
    | bool_exp                  {
                                    $<multival.value.ubool>$ = $1;
                                    $<multival.type>$ = (int)VALBOOL;
                                }
    | term                      {
                                    switch($<multival.type>1) {
                                        case (int)VALREAL:
                                            $<multival.value.ureal>$ = $<multival.value.ureal>1;
                                            $<multival.type>$ = (int)VALREAL;
                                            break;
                                        case (int)VALSTR:
                                            $<multival.value.ustr>$ = strdup($<multival.value.ustr>1);
                                            $<multival.type>$ = (int)VALSTR;
                                            break;  
                                        case (int)VALBOOL:
                                            $<multival.value.ubool>$ = $<multival.value.ubool>1;
                                            $<multival.type>$ = (int)VALBOOL;
                                            break;
                                    }
                                }
    ;

bool_exp:  
    exp RELOPLT exp           { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }
                                        
                                    $$ = $<multival.value.ureal>1 < $<multival.value.ureal>3;
                                }
    | exp RELOPLE exp           { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $$ = $<multival.value.ureal>1 <= $<multival.value.ureal>3;
                                }
    | exp RELOPGT exp           { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $$ = $<multival.value.ureal>1 > $<multival.value.ureal>3;
                                }
    | exp RELOPGE exp           { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }
                                    
                                    $$ = $<multival.value.ureal>1 >= $<multival.value.ureal>3;
                                }
    | exp RELOPEQ exp           { 
                                    if($<multival.type>1 != $<multival.type>3) {
                                        return yyerror("Error: Cannot compare different expression type");
                                    }
                                    
                                    // --- RECHECK and COMPARE---

                                    
                                    if($<multival.type>1 == (int)VALREAL || $<multival.type>3 == (int)VALREAL) {
                                        // Compare REAL
                                        $$ = $<multival.value.ureal>1 == $<multival.value.ureal>3; 

                                    } else if($<multival.type>1 == (int)VALSTR || $<multival.type>3 == (int)VALSTR) {
                                        // Compare STRING
                                        printf("%s >< %s\n", $<multival.value.ustr>1, $<multival.value.ustr>3);
                                        if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) == 0) {
                                            $$ = true;
                                            printf("masukkkk\n");
                                        } else {
                                            $$ = false;
                                            printf("nooo\n");
                                        }

                                        // Need to free cuz of strdup previously
                                        free($<multival.value.ustr>1);
                                        free($<multival.value.ustr>3);

                                    } else if($<multival.type>1 == (int)VALBOOL || $<multival.type>3 == (int)VALBOOL) {
                                        // Compare BOOL
                                        $$ = $<multival.value.ubool>1 == $<multival.value.ubool>3; 
                                    }
                                }
    | exp RELOPNE exp           { 
                                    if($<multival.type>1 != $<multival.type>3) {
                                        return yyerror("Error: Cannot compare different expression type");
                                    }
                                    
                                    // --- RECHECK and COMPARE---

                                    
                                    if($<multival.type>1 == (int)VALREAL || $<multival.type>3 == (int)VALREAL) {
                                        // Compare REAL
                                        $$ = $<multival.value.ureal>1 != $<multival.value.ureal>3; 

                                    } else if($<multival.type>1 == (int)VALSTR || $<multival.type>3 == (int)VALSTR) {
                                        // Compare STRING
                                        printf("%s >< %s\n", $<multival.value.ustr>1, $<multival.value.ustr>3);
                                        if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) != 0) {
                                            $$ = true;
                                            printf("masukkkk\n");
                                        } else {
                                            $$ = false;
                                            printf("nooo\n");
                                        }

                                        // Need to free cuz of strdup previously
                                        free($<multival.value.ustr>1);
                                        free($<multival.value.ustr>3);

                                    } else if($<multival.type>1 == (int)VALBOOL || $<multival.type>3 == (int)VALBOOL) {
                                        // Compare BOOL
                                        $$ = $<multival.value.ubool>1 != $<multival.value.ubool>3; 
                                    }
                                }
    | exp AND exp               { 
                                    if($<multival.type>1 != (int)VALBOOL || $<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }

                                    $$ = $<multival.value.ubool>1 && $<multival.value.ubool>3; 
                                    // $$ = $1 && $3;
                                }
    | exp OR exp                { 
                                    if($<multival.type>1 != (int)VALBOOL || $<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }

                                    $$ = $<multival.value.ubool>1 || $<multival.value.ubool>3;
                                    // $$ = $1 || $3; 
                                }
    | NOT exp              { 
                                    if($<multival.type>2 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }
                                    
                                    $$ = !$<multival.value.ubool>2;
                                }
    ;

// TODO: need to add function calling
arth_exp:  
    exp ADD exp                 { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = $<multival.value.ureal>1 + $<multival.value.ureal>3;
                                    $<multival.type>$ = (int)VALREAL;
                                    printf("%lf\n", $<multival.value.ureal>$);

                                }
    | exp SUB exp               { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = $<multival.value.ureal>1 - $<multival.value.ureal>3;
                                    $<multival.type>$ = (int)VALREAL;
                                    printf("%lf\n", $<multival.value.ureal>$);
                                }
    | exp MUL exp               { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = $<multival.value.ureal>1 * $<multival.value.ureal>3;
                                    $<multival.type>$ = (int)VALREAL;
                                    printf("%lf\n", $<multival.value.ureal>$);
                                }
    | exp DIV exp               { 
                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    if($<multival.value.ureal>3 == 0) return yyerror("divide by zero");
                                    else {
                                        $<multival.value.ureal>$ = $<multival.value.ureal>1 / $<multival.value.ureal>3;
                                        $<multival.type>$ = (int)VALREAL;
                                        printf("%lf\n", $<multival.value.ureal>$);
                                    }      
                                }
    | exp MOD exp               { 

                                    if($<multival.type>1 != (int)VALREAL || $<multival.type>3 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = fmod($<multival.value.ureal>1, $<multival.value.ureal>3);
                                    $<multival.type>$ = (int)VALREAL;
                                    printf("%lf\n", $<multival.value.ureal>$);
                                }
    | SUB exp %prec UMINUS      { 
                                    if($<multival.type>2 != (int)VALREAL) {
                                        return yyerror("Error: Expressions only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = -$<multival.value.ureal>2; 
                                    $<multival.type>$ = (int)VALREAL;
                                    printf("%lf", $<multival.value.ureal>$); 
                                }
    ;

term: ID                        { 
                                    if(Col_ST[currentTable]->storeType[$1] == NONE) {
                                        return yyerror("Error: Identifier is not assigned yet");
                                    } 
                                }
    array_ref                   { // NB: will return index
                                    // Check whether this is array or not
                                    if($3 == -1) {
                                        if(Col_ST[currentTable]->storeType[$1] == MULTIVAR) {
                                            return yyerror("Error: Expressions of array must have reference form");
                                        } 

                                        // TODO: Return normal variable value
                                        $<multival.value.ureal>$ = 3; 
                                        $<multival.type>$ = (int)VALREAL;

                                    } else { // is an array
                                        if(Col_ST[currentTable]->storeType[$1] != MULTIVAR) {
                                            return yyerror("Error: Non array expression must not have reference form");
                                        } 

                                        // check start index
                                        int sIndex = Col_ST[currentTable]->arrayAtr[$1].startIndex;
                                        if($3 < sIndex) {
                                            return yyerror("Error: Wrong start index");
                                        }

                                        // TODO: Return array value
                                        $<multival.value.ureal>$ = 4; 
                                        $<multival.type>$ = (int)VALREAL;
                                    }
                                }
    | STRING                    { $<multival.value.ustr>$ = $1; $<multival.type>$ = (int)VALSTR; }
    | INT                       { $<multival.value.ureal>$ = $1; $<multival.type>$ = (int)VALREAL; }
    | REAL                      { $<multival.value.ureal>$ = $1; $<multival.type>$ = (int)VALREAL; }
    | TRUE                      { $<multival.value.ubool>$ = true; $<multival.type>$ = (int)VALBOOL; }
    | FALSE                     { $<multival.value.ubool>$ = false; $<multival.type>$ = (int)VALBOOL; }
    ;

array_ref: /* empty */  { $$ = -1; }
    | SOPEN INT SCLOSE  {   
                            if($2 < 0) {
                                return yyerror("Error: Invalid index range");
                            } 
                            
                            $$ = $2; 
                        }
    ;

invoc_param: /* empty */
    | POPEN param PCLOSE 
    ;
param: exp next_exps
    ;
next_exps: /* empty */
    | COMMA invoc_param
    ;

func_invoke: ID invoc_param
    ;

if_stmt: IF exp                         {
                                            if($<multival.type>2 != (int)VALBOOL) {
                                                return yyerror("Error: Conditional expression can only receive bool");
                                            }
                                            printf("Conditional Expression pass\n");
                                        } 
    THEN                                {
                                            createChildTable();
                                            printSymbols();
                                        } 
    stmt_body                           
    else_exist
    END IF                              {
                                            returnToParent();
                                            printSymbols();
                                        }
    ;

else_exist: /* empty */ 
    | ELSE                              {
                                            returnToParent();
                                            printSymbols();

                                            createChildTable();
                                            printSymbols();
                                        } 
    stmt_body 

loop_stmt: LOOP                         {
                                            createChildTable();
                                            printSymbols();
                                        } 
    stmt_body END LOOP                  {
                                            returnToParent();
                                            printSymbols();
                                        }
    | FOR                               {
                                            createChildTable(); 
                                        } 
    
    for_arg stmt_body END FOR          {
                                            returnToParent();
                                            printSymbols();
                                        }
    ;
for_arg: ID COLON INT DOT DOT INT     {
                                            Col_ST[currentTable]->storeType[$1] = VARIABLE;
                                            Col_ST[currentTable]->valueType[$1] = VALINT;
                                            printSymbols();
                                        } 
    | DECREASING ID COLON INT DOT DOT INT   {
                                                Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                Col_ST[currentTable]->valueType[$2] = VALINT;
                                                printSymbols();
                                            } 

%%


void create() {
    // Initiate Collection of Symbol Tables
    currentSize = 0;
    currentTable = -1;
    Col_ST = malloc(NUMBER_OF_ST * sizeof(struct SymbolTable*));

	// initiate symbol table
	// symbols.capacity = ST_LENGTH;
    // 	symbols.size = 0;
	// symbols.strList = malloc(symbols.capacity * sizeof(char*));
	// symbols.valueType = malloc(symbols.capacity * sizeof(enum ValueType));
	// symbols.storeType = malloc(symbols.capacity * sizeof(enum StoreType));
	// if(symbols.strList == NULL) {
    //     printf("Unable to allocate memory for strList:(\n");
    //     return;
    // }

    // if(symbols.valueType == NULL) {
    //     printf("Unable to allocate memory for valueType:(\n");
    //     return;
    // }

    // if(symbols.storeType == NULL) {
    //     printf("Unable to allocate memory for storeType:(\n");
    //     return;
    // }

    createChildTable();
}

void createChildTable() {
    int oldTableIndex = currentTable;
    currentSize++;
    currentTable = currentSize - 1;
    Col_ST[currentTable] = malloc(sizeof(struct SymbolTable));
    Col_ST[currentTable]->index = currentTable;
    Col_ST[currentTable]->capacity = ST_LENGTH;
    Col_ST[currentTable]->size = 0;
    Col_ST[currentTable]->strList = malloc(Col_ST[currentTable]->capacity * sizeof(char*));
	Col_ST[currentTable]->valueType = malloc(Col_ST[currentTable]->capacity * sizeof(enum ValueType));
	Col_ST[currentTable]->storeType = malloc(Col_ST[currentTable]->capacity * sizeof(enum StoreType));
    Col_ST[currentTable]->arrayAtr = malloc(Col_ST[currentTable]->capacity * sizeof(struct ArrayAtr));

	if(Col_ST[currentTable]->strList == NULL) {
        printf("Unable to allocate memory for strList:(\n");
        return;
    }

    if(Col_ST[currentTable]->valueType == NULL) {
        printf("Unable to allocate memory for valueType:(\n");
        return;
    }

    if(Col_ST[currentTable]->storeType == NULL) {
        printf("Unable to allocate memory for storeType:(\n");
        return;
    }

    if(currentTable > 0) {
        Col_ST[currentTable]->parent = Col_ST[oldTableIndex];
    } else {
        Col_ST[currentTable]->parent = NULL;
    }

    printf("New table made. Checking..\n");
    printf("Index: %d\n", Col_ST[currentTable]->index);
    printf("Capacity: %lu\n", Col_ST[currentTable]->capacity);
    printf("Size: %lu\n", Col_ST[currentTable]->size);

}

void returnToParent() {
    struct SymbolTable* parentTable = Col_ST[currentTable]->parent;
    currentTable = parentTable->index;
}

int lookup(char* s) {
	// for(int i = 0; i < symbols.size; i++) {
	// 	if(strcmp(symbols.strList[i], s) == 0) {
	// 		return i;
	// 	}
	// }

    for(int i = 0; i < Col_ST[currentTable]->size; i++) {
		if(strcmp(Col_ST[currentTable]->strList[i], s) == 0) {
			return i;
		}
	}
	
	return -1;
}

int insert(char* s) {
	// printSymbols();
	// if(symbols.size == symbols.capacity) {
    //     symbols.capacity *= 2;
    //     symbols.strList = realloc(symbols.strList, symbols.capacity * sizeof(char*));
	// 	symbols.valueType = realloc(symbols.valueType, symbols.capacity * sizeof(*symbols.valueType));
	// 	symbols.storeType = realloc(symbols.storeType, symbols.capacity * sizeof(*symbols.storeType));

    //     if(symbols.strList == NULL) {
    //         printf("Unable to reallocate memory :(\n");
    //         return -1;
    //     }
    // }
	// symbols.strList[symbols.size] = malloc(strlen(s) + 1);
	// strcpy(symbols.strList[symbols.size], s);

	// symbols.size++;
	
	// return symbols.size - 1;      
    if(Col_ST[currentTable]->size == Col_ST[currentTable]->capacity) {
        Col_ST[currentTable]->capacity *= 2;
        Col_ST[currentTable]->strList = realloc(Col_ST[currentTable]->strList, Col_ST[currentTable]->capacity * sizeof(char*));
		Col_ST[currentTable]->valueType = realloc(Col_ST[currentTable]->valueType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->valueType));
		Col_ST[currentTable]->storeType = realloc(Col_ST[currentTable]->storeType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->storeType));
        Col_ST[currentTable]->arrayAtr = realloc(Col_ST[currentTable]->arrayAtr, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->arrayAtr));

        if(Col_ST[currentTable]->strList == NULL) {
            printf("Unable to reallocate memory :(\n");
            return -1;
        }
    }
	Col_ST[currentTable]->strList[Col_ST[currentTable]->size] = malloc(strlen(s) + 1);
	strcpy(Col_ST[currentTable]->strList[Col_ST[currentTable]->size], s);

	Col_ST[currentTable]->size++;
	
	return Col_ST[currentTable]->size - 1;
}

void dump() {
	printf("\n");
	printSymbols();
	printf("Dumping all identifiers\n");
	// free(symbols.strList);
    // free(symbols.valueType);
    // free(symbols.storeType);
    for(int i = 0; i < currentSize; i++) {
        // for(int j = 0; j < Col_ST[i]->size; j++) {
        //     free(Col_ST[i]->strList[j]);
        // }
        free(Col_ST[i]->strList);
        printf("Free strList\n");
        free(Col_ST[i]->valueType);
        printf("Free valueType\n");
        free(Col_ST[i]->storeType);
        printf("Free storeType\n");
        free(Col_ST[i]);
        printf("Free Col_ST INDEX %d\n", i);
    }
    free(Col_ST);
}

void printSymbols() {
	printf("Symbol Table %d:\n", currentTable);
    printf("==================\n");
	for(int i = 0; i < Col_ST[currentTable]->size; i++) {
		printf("%d %d %d %s\n", i, Col_ST[currentTable]->storeType[i], Col_ST[currentTable]->valueType[i], Col_ST[currentTable]->strList[i]);
    }
    printf("==================\n");
}

void initFlags() {
    funcScope = false;
    procScope = false;
}

int yyerror(char *s) {
   fprintf(stderr, "%s\n", s);   
   return 0; 
}

int main(void)
{
    /* open the source program file */
    // if (argc != 2) {
    //     printf ("Usage: sc filename\n");
    //     exit(1);
    // }
    // yyin = fopen(argv[1], "r");         /* open input file */

    /* perform parsing */
    create();
    yyparse();
    dump();

    return 0;
}

