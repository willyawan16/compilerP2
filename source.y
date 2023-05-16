%{
#define ST_LENGTH 2
#define MAX_LINE_LENGTH 256
#define NUMBER_OF_ST 100
#define MAX_NUMBER_OF_PARAM 100
#define NO_EXT 99999
#define FUNCPARAM 99998
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdbool.h>
#include<math.h>
#define Trace(t)        printf(t)
int yylex(void);
int yyerror(char *s);

extern FILE *yyin;
extern int yylineno;

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
	VALBOOL,
    VALARRAYREF,
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

struct FuncAtr {
    int size;
    int paramType[MAX_NUMBER_OF_PARAM];
    struct ArrayAtr arrayAtr[MAX_NUMBER_OF_PARAM];
};

struct SymbolTable {
    int index;
    char** strList;
	enum ValueType* valueType;
	enum StoreType* storeType;
    struct ArrayAtr* arrayAtr;
    struct FuncAtr* funcAtr;
    size_t capacity;
    size_t size;
    struct SymbolTable* parent;
};


struct TermStack {
    struct SymbolTable* chosenTableStack;
    int foundIndex;
};

struct FuncCallingStack {
    bool funcCalling;
    int atParentIndex;
    int currentParamIndex;
};

struct SymbolTable** Col_ST;
int currentSize;
int currentTable;

struct TermStack termStack[10];
int topTerm;

struct FuncCallingStack funcCallingStack[10];
int topFuncCalling;

struct SymbolTable symbols;

void create();
void createChildTable();
int lookup(char* s);
int insert(char* s);
void dump();
int lookupInTable(char* s, struct SymbolTable* parentTable);
void printSymbols();
void printChosenSymbols(int index);
void returnToParent();
void initFlags();
void resetFunctionGrammarFlags();
enum ValueType valTypeBuffer = VALINT;
enum ValueType constExpTypeBuffer = VALINT;

// Flags
bool funcScope;
bool procScope;
bool needConstExp;
bool funcDeclaration;
bool funcCalling;
int currentParamIndex;
struct SymbolTable* functionParent;
int atParentIndex;
int foundIndex;
int scopeReturnType;
bool isCallingArray;
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
            struct {
                int arrayType;
                int startIndex;
                int capacity;
            } uarray;
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
%token <ival> INT ID
%token <dval> REAL
%token <bval> TRUE FALSE

%type <bval> bool_exp
%type <multival> exp
%type <ival> array_ref id_ext


%left OR
%left AND
%right NOT
%left RELOPLT RELOPLE RELOPGT RELOPGE RELOPEQ RELOPNE
%left ADD SUB
%left MUL DIV MOD
%nonassoc UMINUS

%start program
%%

/* ----------- Main Structure ----------- */
program: /* empty */
    | decls
    | stmts
    | decls stmts
    ;
    
// newline: /* empty */ | NEWLINE

decls: /* empty */
    | decl decls 
    ;

stmts: /* empty */ 
    | stmt stmts 
    ;
/* ----------- Main Structure ----------- */

// const_exp: INT          { 
//                             constExpTypeBuffer = VALINT;
//                             $<ival>$ = $1;
//                         }
//     | REAL              { 
//                             constExpTypeBuffer = VALREAL;
//                             $<dval>$ = $1;
//                         }
//     | STRING            { 
//                             constExpTypeBuffer = VALSTR;
//                             $<sval>$ = $1;
//                         }
//     | TRUE              { 
//                             constExpTypeBuffer = VALBOOL;
//                             $<bval>$ = $1;
//                         }
//     | FALSE             { 
//                             constExpTypeBuffer = VALBOOL;
//                             $<bval>$ = $1;
//                         }
//     ;

/* Value Type Non-Terminal */
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

/* Declaration is divided into variables and functions declaration */
decl: var_decl | func_decl

/* Variable Declaration */
var_decl: 
    /* const id := const_exp */
    CONST  ID ASSIGN                            {needConstExp = true; } 
    exp                                         { 
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 

                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    // Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    switch($<multival.type>5) {
                                                        case (int)VALREAL:
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            break;
                                                        case (int)VALSTR:
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            break;
                                                    }
                                                    // TODO: store value

                                                    printSymbols();
                                                    needConstExp = false;
                                                } 
    /* const id :val_type := const_exp */
    | CONST ID COLON val_type ASSIGN            {needConstExp = true;}
    exp                                         { 
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    
                                                    switch($<multival.type>7) {
                                                        case (int)VALREAL:
                                                            if(valTypeBuffer == VALINT) {
                                                                Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            } else if(valTypeBuffer == VALREAL) {
                                                                Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            } else {
                                                                return yyerror("Error: Different value type");
                                                            }
                                                            break;
                                                        case (int)VALSTR:
                                                            if(valTypeBuffer != VALSTR) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            if(valTypeBuffer != VALBOOL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            break;
                                                    }
                                                    // TODO: store value

                                                    printSymbols();
                                                    needConstExp = false;
                                                }
    /* var id := const_exp */
    | VAR ID ASSIGN                             {needConstExp = true;} 
    exp                                         {
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;

                                                    switch($<multival.type>5) {
                                                        case (int)VALREAL:
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            break;
                                                        case (int)VALSTR:
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            break;
                                                    }
                                                    // TODO: store value

                                                    printSymbols();
                                                    needConstExp = false;
                                                }
    /* var id :val_type := const_exp */
    | VAR ID COLON val_type ASSIGN              {needConstExp = true;}
    exp                                         {                                                    
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;

                                                    switch($<multival.type>7) {
                                                        case (int)VALREAL:
                                                            if(valTypeBuffer == VALINT) {
                                                                Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            } else if(valTypeBuffer == VALREAL) {
                                                                Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            } else {
                                                                return yyerror("Error: Different value type");
                                                            }
                                                            break;
                                                        case (int)VALSTR:
                                                            if(valTypeBuffer != VALSTR) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            if(valTypeBuffer != VALBOOL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            break;
                                                    }
                                                    // TODO: store value
                                                    
                                                    printSymbols();
                                                    needConstExp = false;
                                                }
    /* var id :val_type */
    | VAR ID COLON val_type                     {
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    Col_ST[currentTable]->valueType[$2] = valTypeBuffer;

                                                    printSymbols();
                                                }
    /* var id :array int..int of val_type */
    | VAR ID COLON ARRAY INT DOT DOT INT OF val_type    {
                                                            if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                                return yyerror("Error: Identifier existed");
                                                            } 
                                                            
                                                            Col_ST[currentTable]->storeType[$2] = MULTIVAR;
                                                            Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                                                            
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

/* Simultanoues variable declaration */
var_decls: /* empty */ 
    | var_decl var_decls
    ;

/* Statement Body */
stmt_body: var_decls stmts ;

/* For Function argument declaration */
func_args: POPEN formal_args PCLOSE 
    ;
formal_args: /*empty*/
    | formal_arg nextFormal_args
    ;
nextFormal_args: /* empty */
    | COMMA formal_args
    ;
formal_arg: ID COLON val_type   { // formal argument usual form
                                    if(Col_ST[currentTable]->storeType[$1] != NONE) {
                                        return yyerror("Error: Identifier existed");
                                    } 
                                    
                                    Col_ST[currentTable]->storeType[$1] = VARIABLE;
                                    Col_ST[currentTable]->valueType[$1] = valTypeBuffer;
                                    functionParent->funcAtr[atParentIndex].paramType[currentParamIndex] = (int)valTypeBuffer;
                                    currentParamIndex++;
                                }
    | ID COLON ARRAY INT DOT DOT INT OF val_type        { // formal argument array form
                                                            if(Col_ST[currentTable]->storeType[$1] != NONE) {
                                                                return yyerror("Error: Identifier existed");
                                                            } 
                                                            
                                                            Col_ST[currentTable]->storeType[$1] = MULTIVAR;
                                                            Col_ST[currentTable]->valueType[$1] = valTypeBuffer;
                                                            Col_ST[currentTable]->arrayAtr[$1].startIndex = $4;
                                                            Col_ST[currentTable]->arrayAtr[$1].capacity = $7 - $4 + 1;

                                                            functionParent->funcAtr[atParentIndex].paramType[currentParamIndex] = (int)valTypeBuffer;
                                                            functionParent->funcAtr[atParentIndex].arrayAtr[currentParamIndex].startIndex = $4;
                                                            functionParent->funcAtr[atParentIndex].arrayAtr[currentParamIndex].capacity = $7 - $4 + 1;
                                                            currentParamIndex++;
                                                        }
    ;
/* --------------------------------- */

/* Function or Procedure declaration */
func_decl: 
    /* FUNCTION Declaration */
    FUNCTION ID     { 
                        funcScope = true;
                        funcDeclaration = true;
                        functionParent = Col_ST[currentTable];
                        atParentIndex = $2;
                        if(Col_ST[currentTable]->storeType[$2] != NONE) {
                            return yyerror("Error: Identifier existed");
                        }
                            
                        Col_ST[currentTable]->storeType[$2] = FUNC;
                        
                        createChildTable();
                        printSymbols();
                    } 
    func_args                               { printSymbols(); functionParent->funcAtr[atParentIndex].size = currentParamIndex;} 
    COLON val_type                          { scopeReturnType = (int)valTypeBuffer;} 
    stmt_body END                           {
                                                returnToParent();
                                                
                                                // assign return type
                                                Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                                                printSymbols();
                                            } 
    ID                                      {
                                                if($2 != $12) {
                                                    return yyerror("Error: Function closure does not match the function name");
                                                }

                                                // Reset
                                                resetFunctionGrammarFlags();
                                            }
    /* PROCEDURE Declaration */
    | PROCEDURE ID                          { 
                                                procScope = true;
                                                scopeReturnType = -1;
                                                funcDeclaration = true;
                                                functionParent = Col_ST[currentTable];
                                                atParentIndex = $2;
                                                if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                    return yyerror("Error: Identifier existed");
                                                }
                                                    
                                                Col_ST[currentTable]->storeType[$2] = PROC;
                                                Col_ST[currentTable]->valueType[$2] = VALNONE;
                                                
                                                createChildTable();
                                                printSymbols();
                                            } 
    func_args                               { printSymbols(); functionParent->funcAtr[atParentIndex].size = currentParamIndex; }  
    stmt_body END                           {
                                                returnToParent();
                                                printSymbols();
                                            } 
    ID                                      {
                                                if($2 != $9) {
                                                    return yyerror("Error: Function closure does not match the function name");
                                                }
                                                // Reset
                                                resetFunctionGrammarFlags();
                                            }
    ;

/* Statements */
stmt: block
    | simple
    | proc_invoke
    | if_stmt
    | loop_stmt
    ;

/* Blocks */
block: BEGINT                   {
                                    createChildTable();
                                    printSymbols();
                                } 
    stmt_body END               {
                                    returnToParent(); 
                                    printSymbols();
                                }
    ;

/* Simple Statement */
simple: 
    ID array_ref ASSIGN exp         { 

                                        struct SymbolTable* chosenTable = NULL;
                                        struct SymbolTable* parentTable = NULL;
                                        int foundIndex = -1;
                                        
                                        // Check whether is initialized
                                        if(Col_ST[currentTable]->storeType[$1] == NONE) {
                                            
                                            // Search in its ancestor symbol table
                                            bool found = false;
                                            parentTable = Col_ST[currentTable]->parent;
                                            while (!found) {
                                                if(parentTable == NULL) {
                                                    return yyerror("Error: Identifier is not assigned yet anywhere");
                                                }
                                                foundIndex = lookupInTable(Col_ST[currentTable]->strList[$1], parentTable);
                                                if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
                                                    found = true;
                                                    break;
                                                }

                                                parentTable = parentTable->parent;
                                            }
                                            printf("FOUND in parent %s (table %d, index %d)! for assignment\n", Col_ST[currentTable]->strList[$1],parentTable->index, foundIndex );
                                            
                                            // CHOSEN TABLE TO PARENT TABLE
                                            chosenTable = parentTable;
                                        }

                                        if(parentTable == NULL) {
                                            // JUST FOR CURRENT TABLE VARIABLE
                                            chosenTable = Col_ST[currentTable];
                                            foundIndex = $1;
                                        }

                                        if(chosenTable->storeType[foundIndex] == CONSTANT) {
                                            return yyerror("Error: Cannot assign to constant variables");
                                        }

                                        if(chosenTable->storeType[foundIndex] == FUNC) {
                                            return yyerror("Error: Cannot assign to functions");
                                        }

                                        if(chosenTable->storeType[foundIndex] == PROC) {
                                            return yyerror("Error: Cannot assign to procedures");
                                        }

                                        // Check whether this is array or not AND grammar exception
                                        if($2 == -1) { // grammar WITHOUT array reference
                                            if(chosenTable->storeType[foundIndex] == MULTIVAR) {
                                                return yyerror("Error: Expressions of array must have reference form");
                                            } 

                                            // TODO: Do assignment to normal variable
                                            if(chosenTable->valueType[foundIndex] == VALINT && $<multival.type>4 == (int)VALREAL) {
                                                printf("Convert real(rhs) to int(lhs)! Do assignment\n");
                                            } else if((int)chosenTable->valueType[foundIndex] == $<multival.type>4) {
                                                printf("Value match! Do assignment\n");
                                            } else {
                                                return yyerror("Error: Left hand side does not match right hand side value type");
                                            }
                                            

                                        } else { // grammar WITH array reference
                                            if(chosenTable->storeType[foundIndex] != MULTIVAR) {
                                                return yyerror("Error: Non array expression must not have reference form");
                                            } 

                                            // check start index
                                            int sIndex = chosenTable->arrayAtr[foundIndex].startIndex;
                                            if($2 < sIndex) {
                                                return yyerror("Error: Wrong start index");
                                            }

                                            // TODO: Do assignment to array
                                            if(chosenTable->valueType[foundIndex] == VALINT && $<multival.type>4 == (int)VALREAL) {
                                                printf("Convert real(rhs) to int(lhs)! Do assignment\n");
                                            } else if((int)chosenTable->valueType[foundIndex] == $<multival.type>4) {
                                                printf("Value match! Do assignment\n");
                                            } else {
                                                return yyerror("Error: Left hand side does not match right hand side value type");
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
                                            case (int)VALARRAYREF:
                                                printf("array: print whole array");
                                                break;
                                        }
                                    }
    | GET ID array_ref              {
                                        struct SymbolTable* chosenTable = NULL;
                                        struct SymbolTable* parentTable = NULL;
                                        int foundIndex;

                                        // Check whether is initialized
                                        if(Col_ST[currentTable]->storeType[$2] == NONE) {
                                            // Search in its ancestor symbol table
                                            bool found = false;
                                            parentTable = Col_ST[currentTable]->parent;
                                            while (!found) {
                                                if(parentTable == NULL) {
                                                    return yyerror("Error: Identifier is not assigned yet anywhere");
                                                }
                                                
                                                foundIndex = lookupInTable(Col_ST[currentTable]->strList[$2], parentTable);
                                                if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
                                                    found = true;
                                                    break;
                                                }

                                                parentTable = parentTable->parent;
                                            }
                                            printf("FOUND in parent %s (table %d, index %d)! for get ID\n", Col_ST[currentTable]->strList[$2], parentTable->index, foundIndex);
                                            
                                            // CHOSEN TABLE TO PARENT TABLE
                                            chosenTable = parentTable;
                                        }

                                        if(parentTable == NULL) {
                                            // JUST FOR CURRENT TABLE VARIABLE
                                            chosenTable = Col_ST[currentTable];
                                            foundIndex = $2;
                                        }
                                        
                                        if(chosenTable->storeType[foundIndex] == CONSTANT) {
                                            return yyerror("Error: Cannot process input to constant variables");
                                        }

                                        if(chosenTable->storeType[foundIndex] == FUNC) {
                                            return yyerror("Error: Cannot process input to functions");
                                        }

                                        if(chosenTable->storeType[foundIndex] == PROC) {
                                            return yyerror("Error: Cannot process input to procedures");
                                        }

                                        // Check whether this is array or not AND grammar exception
                                        if($2 == -1) { // grammar WITHOUT array reference
                                            if(chosenTable->storeType[foundIndex] == MULTIVAR) {
                                                return yyerror("Error: Expressions of array must have reference form");
                                            } 

                                            // TODO: Do scanning into normal variable
                                            printf("Scan input to noraml variable");                                            

                                        } else { // grammar WITH array reference
                                            if(chosenTable->storeType[foundIndex] != MULTIVAR) {
                                                return yyerror("Error: Non array expression must not have reference form");
                                            } 

                                            // check start index
                                            int sIndex = chosenTable->arrayAtr[foundIndex].startIndex;
                                            if($3 < sIndex) {
                                                return yyerror("Error: Wrong start index");
                                            }

                                            // TODO: Do scanning into array
                                            printf("Scan input to noraml variable"); 
                                        }
                                    }
    | RESULT exp                    {
                                        if(procScope) {
                                            return yyerror("Error: RESULT EXPRESSION is only used in function scope");
                                        }

                                        if(funcScope) {
                                            // check return type
                                            int expType = $<multival.type>2;
                                            if(expType == VALREAL && scopeReturnType == (int)VALINT) {
                                                // Convert real to int
                                            } else if (expType == scopeReturnType) {
                                                // ok
                                            } else{
                                                return yyerror("Error: Expression of result must be the same as return type of the function");
                                            }
                                        }
                                    }        
    | RETURN                        {
                                        if(funcScope) {
                                            return yyerror("Error: RETURN is only used in procedure scope");
                                        }

                                        if(procScope) {
                                            // do nothing
                                        }
                                    }
    | EXIT exit_cond
    | SKIP
    ;
/* Exit Condition */
exit_cond: /* empty */
    | WHEN exp  {
                    if($<multival.type>2 != (int)VALBOOL) {
                        return yyerror("Error: Exit condition must be bool expression");
                    }

                }

/* Expression Forms */
exp:
    /* Parantheses */
    POPEN exp PCLOSE           { 
                                    // if($<multival.type>2 == (int)VALSTR) {
                                    //     return yyerror("Error: Parentheses cannot accept string");
                                    // }

                                    if($<multival.type>2 == (int)VALREAL) {
                                        $<multival.value.ureal>$ = $<multival.value.ureal>2; 
                                        $<multival.type>$ = (int)VALREAL;
                                        // printf("%lf\n", $<multival.value.ureal>$);
                                    } else if ($<multival.type>2 == (int)VALBOOL) {
                                        $<multival.value.ubool>$ = $<multival.value.ubool>2; 
                                        $<multival.type>$ = (int)VALBOOL;
                                        // printf("%d\n", $<multival.value.ubool>$);
                                    } else if ($<multival.type>2 == (int)VALSTR) {
                                        $<multival.value.ustr>$ = $<multival.value.ustr>2; 
                                        $<multival.type>$ = (int)VALSTR;
                                        // printf("%d\n", $<multival.value.ubool>$);
                                    }
                                    
                                }   
    /* Arithmetic Expression */
    | arth_exp                  { 
                                    if($<multival.type>1 != (int)VALREAL) {
                                        return yyerror("Error: Arithmethic expression only can receive numbers");
                                    }

                                    $<multival.value.ureal>$ = $<multival.value.ureal>1;
                                    $<multival.type>$ = (int)VALREAL;
                                    // printf("%lf\n", $<multival.value.ureal>$);

                                }
    /* Boolean Expression */
    | bool_exp                  {
                                    $<multival.value.ubool>$ = $1;
                                    $<multival.type>$ = (int)VALBOOL;
                                }
    /* Expression Components */
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
                                        case (int)VALARRAYREF:
                                            $<multival.value.uarray.arrayType>$ = $<multival.value.uarray.arrayType>1;
                                            $<multival.value.uarray.startIndex>$ = $<multival.value.uarray.startIndex>1;
                                            $<multival.value.uarray.capacity>$ = $<multival.value.uarray.capacity>1;
                                            $<multival.type>$ = (int)VALARRAYREF;
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
                                        if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) == 0) {
                                            $$ = true;
                                        } else {
                                            $$ = false;
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
                                        if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) != 0) {
                                            $$ = true;
                                        } else {
                                            $$ = false;
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
                                }
    | exp OR exp                { 
                                    if($<multival.type>1 != (int)VALBOOL || $<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }

                                    $$ = $<multival.value.ubool>1 || $<multival.value.ubool>3;
                                }
    | NOT exp              { 
                                    if($<multival.type>2 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }
                                    
                                    $$ = !$<multival.value.ubool>2;
                                }
    ;

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


term:
    /* variable names and array reference */
    ID          { 
                    topTerm++;
                    termStack[topTerm].chosenTableStack = NULL;
                    termStack[topTerm].foundIndex = -1;
                    struct SymbolTable* parentTable = NULL;
                    // Check whether is initialized
                    if(Col_ST[currentTable]->storeType[$1] == NONE) {
                        // Search in its ancestor symbol table
                        bool found = false;
                        
                        parentTable = Col_ST[currentTable]->parent;
                        while (!found) {
                            if(parentTable == NULL) {
                                return yyerror("Error: Identifier is not assigned yet anywhere");
                            }
                            
                            termStack[topTerm].foundIndex = lookupInTable(Col_ST[currentTable]->strList[$1], parentTable);
                            if(termStack[topTerm].foundIndex != -1 && parentTable->storeType[termStack[topTerm].foundIndex] != NONE) {
                                found = true;
                                break;
                            }

                            parentTable = parentTable->parent;
                        }
                        printf("FOUND in parent %s (table %d, index %d)! for const oprnd\n", Col_ST[currentTable]->strList[$1], parentTable->index, termStack[topTerm].foundIndex );
                        
                        // CHOSEN TABLE TO PARENT TABLE
                        termStack[topTerm].chosenTableStack = parentTable;
                    }

                    if(parentTable == NULL) {
                        // JUST FOR CURRENT TABLE VARIABLE
                        termStack[topTerm].chosenTableStack = Col_ST[currentTable];
                        termStack[topTerm].foundIndex = $1;
                    }

                    if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == PROC) {
                        return yyerror("Error: Cannot get procedures");
                    }

                    if(needConstExp) {
                        if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] != CONSTANT) {
                            return yyerror("Error: Declaration must not have non constant expression");
                        }
                    }

                    if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                        // funcCalling = true;
                        // atParentIndex = termStack[topTerm].foundIndex;
                        // currentParamIndex = 0;

                        topFuncCalling++;
                        funcCallingStack[topFuncCalling].funcCalling = true; 
                        funcCallingStack[topFuncCalling].atParentIndex = termStack[topTerm].foundIndex;
                        funcCallingStack[topFuncCalling].currentParamIndex = 0;
                        functionParent =  termStack[topTerm].chosenTableStack;
                        // functionParent = Col_ST[0];
                    }
                } 
    id_ext                      { 
                                    // Check whether this is array or not AND grammar exception
                                    if($3 == NO_EXT && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == MULTIVAR) {
                                        // return as an array
                                        isCallingArray = true;
                                        $<multival.value.uarray.arrayType>$ = (int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex];
                                        $<multival.value.uarray.startIndex>$ = termStack[topTerm].chosenTableStack->arrayAtr[termStack[topTerm].foundIndex].startIndex;
                                        $<multival.value.uarray.capacity>$ = termStack[topTerm].chosenTableStack->arrayAtr[termStack[topTerm].foundIndex].capacity;
                                        $<multival.type>$ = (int)VALARRAYREF;
                                    } else if($3 == NO_EXT && (termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == CONSTANT || termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == VARIABLE)) { 
                                        // grammar WITHOUT array reference
                                        if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == MULTIVAR) {
                                            return yyerror("Error: Expressions of array must have reference form");
                                        } 

                                        // TODO: Return normal variable/constant value
                                        switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                            case (int)VALINT:
                                                // CONVERT INTEGER TO REAL
                                                $<multival.value.ureal>$ = 999; // get integer
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALREAL:
                                                $<multival.value.ureal>$ = 999; // get real
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALSTR:
                                                $<multival.value.ustr>$ = "Temp String"; // get string
                                                $<multival.type>$ = (int)VALSTR;
                                                break;  
                                            case (int)VALBOOL:
                                                $<multival.value.ubool>$ = true; // get bool
                                                $<multival.type>$ = (int)VALBOOL;
                                                break;
                                        }
                                        

                                    } else if($3 == NO_EXT && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                                        // Function with no param
                                        if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] != MULTIVAR) {
                                            return yyerror("Error: Non array expression must not have reference form");
                                        } 
                                        // TODO: Return normal function(no parameter) value
                                        // switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                        //     case (int)VALINT:
                                        //         // CONVERT INTEGER TO REAL
                                        //         $<multival.value.ureal>$ = 998; // get integer
                                        //         $<multival.type>$ = (int)VALREAL;
                                        //         break;
                                        //     case (int)VALREAL:
                                        //         $<multival.value.ureal>$ = 998; // get real
                                        //         $<multival.type>$ = (int)VALREAL;
                                        //         break;
                                        //     case (int)VALSTR:
                                        //         $<multival.value.ustr>$ = "Temp String for funcnoparam"; // get string
                                        //         $<multival.type>$ = (int)VALSTR;
                                        //         break;  
                                        //     case (int)VALBOOL:
                                        //         $<multival.value.ubool>$ = false; // get bool
                                        //         $<multival.type>$ = (int)VALBOOL;
                                        //         break;
                                        // }
                                        // resetFunctionGrammarFlags();

                                    } else if($3 == FUNCPARAM && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                                        // Function with param
                                        // TODO: Return normal function with value 
                                        funcCallingStack[topFuncCalling].funcCalling = false; 
                                        funcCallingStack[topFuncCalling].atParentIndex = -1;
                                        topFuncCalling--;
                                        switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                            case (int)VALINT:
                                                // CONVERT INTEGER TO REAL
                                                $<multival.value.ureal>$ = 997; // get integer
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALREAL:
                                                $<multival.value.ureal>$ = 997; // get real
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALSTR:
                                                $<multival.value.ustr>$ = "Temp String funcwithparam"; // get string
                                                $<multival.type>$ = (int)VALSTR;
                                                break;  
                                            case (int)VALBOOL:
                                                $<multival.value.ubool>$ = true; // get bool
                                                $<multival.type>$ = (int)VALBOOL;
                                                break;
                                        }
                                        resetFunctionGrammarFlags();
                                    } else { // grammar WITH array reference
                                        if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] != MULTIVAR) {
                                            return yyerror("Error: Non array expression must not have reference form");
                                        } 

                                        // check start index
                                        int sIndex = termStack[topTerm].chosenTableStack->arrayAtr[termStack[topTerm].foundIndex].startIndex;
                                        if($3 < sIndex) {
                                            return yyerror("Error: Wrong start index");
                                        }

                                        // TODO: Return array value
                                        switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                            case (int)VALINT:
                                                // CONVERT INTEGER TO REAL
                                                $<multival.value.ureal>$ = 991; // get integer
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALREAL:
                                                $<multival.value.ureal>$ = 991; // get real
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALSTR:
                                                $<multival.value.ustr>$ = "Temp String for array"; // get string
                                                $<multival.type>$ = (int)VALSTR;
                                                break;  
                                            case (int)VALBOOL:
                                                $<multival.value.ubool>$ = false; // get bool
                                                $<multival.type>$ = (int)VALBOOL;
                                                break;
                                        }
                                    }

                                    //reset
                                    termStack[topTerm].chosenTableStack = NULL;
                                    termStack[topTerm].foundIndex = -1;
                                    topTerm--;
                                }
    /* Literal Constant */
    | const_oprnd               {
                                    switch($<multival.type>1) {
                                        case (int)VALINT:
                                            // CONVERT INTEGER TO REAL
                                            $<multival.value.ureal>$ = $<multival.value.uint>1;
                                            $<multival.type>$ = (int)VALREAL;
                                            break;
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

const_oprnd: STRING             { $<multival.value.ustr>$ = $1; $<multival.type>$ = (int)VALSTR; }
    | INT                       { $<multival.value.uint>$ = $1; $<multival.type>$ = (int)VALINT; }
    | REAL                      { $<multival.value.ureal>$ = $1; $<multival.type>$ = (int)VALREAL; }
    | TRUE                      { $<multival.value.ubool>$ = true; $<multival.type>$ = (int)VALBOOL; }
    | FALSE                     { $<multival.value.ubool>$ = false; $<multival.type>$ = (int)VALBOOL; }
    ;

/* Check whether it has [exp] or not after ID*/
array_ref: /* empty */  { $$ = -1; }
    | SOPEN exp SCLOSE  {   
                            if($<multival.value.ureal>2 < 0) {
                                return yyerror("Error: Invalid index range");
                            } 
                            
                            $$ = (int)$<multival.value.ureal>2; 
                        }
    ;

/* Check whether it has [exp] or (parameter) or not after ID*/
id_ext: /* empty */         { $$ = NO_EXT; }
    | SOPEN exp SCLOSE      {   
                                if($<multival.value.ureal>2 < 0) {
                                    return yyerror("Error: Invalid index range");
                                } 
                                
                                $$ = (int)$<multival.value.ureal>2; 
                            } 
    | POPEN param PCLOSE    { 
                                $$ = FUNCPARAM;
                                int calledParameter = functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].size;
                                printf("%d><%d\n", funcCallingStack[topFuncCalling].currentParamIndex, calledParameter);
                                if(funcCallingStack[topFuncCalling].currentParamIndex != calledParameter) {
                                    return yyerror("Number of parameter does not match");
                                } else {
                                    printf("Parameter counting PASS %d\n ", calledParameter);
                                }
                            };

/* Procedure Invocation */
invoc_param: POPEN param PCLOSE 
    ;
param: /*empty*/ 
    | exp           {
                        if(funcCallingStack[topFuncCalling].funcCalling) {
                            printf("top funcCalling: %d\n", topFuncCalling);
                            if(isCallingArray
                                && $<multival.value.uarray.arrayType>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex] 
                                && $<multival.value.uarray.capacity>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].arrayAtr[funcCallingStack[topFuncCalling].currentParamIndex].capacity) {
                                
                                printf("same\n");
                            } else if(
                                $<multival.type>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex]
                                || ($<multival.type>1 == (int)VALREAL && functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex] == (int)VALINT)
                            ) {
                                printf("same\n");
                            } else {
                                return yyerror("Error: Parameter does not match");
                            }
                            funcCallingStack[topFuncCalling].currentParamIndex++;
                            isCallingArray = false;
                        }
                    } 
    next_exps
    ;
next_exps: /* empty */
    | COMMA param
    ;
proc_invoke: ID { 
                    // funcCalling = true; 
                    // atParentIndex = $1;
                    currentParamIndex = 0;
                    topFuncCalling++;
                    funcCallingStack[topFuncCalling].funcCalling = true; 
                    funcCallingStack[topFuncCalling].atParentIndex = $1;
                    funcCallingStack[topFuncCalling].currentParamIndex = 0;
                    functionParent = Col_ST[currentTable];
                } 
    invoc_param {
                    int calledParameter = functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].size;
                    if(funcCallingStack[topFuncCalling].currentParamIndex != calledParameter) {
                        return yyerror("Number of parameter does not match");
                    } else {
                        printf("Parameter counting PASS %d\n ", calledParameter);
                    }
                    funcCallingStack[topFuncCalling].funcCalling = false; 
                    funcCallingStack[topFuncCalling].atParentIndex = -1;
                    funcCallingStack[topFuncCalling].currentParamIndex = -1;
                    topFuncCalling--;
                    resetFunctionGrammarFlags();
                    // functionParent = NULL;
                }
    ;
/* ----------------------- */

/* IF statement */
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
    ;
/* ----------------------- */

/* LOOP Statement */
loop_stmt: LOOP                         {
                                            createChildTable();
                                            printSymbols();
                                        } 
    stmt_body END LOOP                  {
                                            returnToParent();
                                            printSymbols();
                                        }
    | FOR {needConstExp = true;} 
    for_ext ID                                  {
                                                    struct SymbolTable* chosenTable = NULL;
                                                    struct SymbolTable* parentTable = NULL;
                                                    int foundIndex = -1;

                                                    // Check whether is initialized
                                                    if(Col_ST[currentTable]->storeType[$4] == NONE) {
                                                        // Search in its ancestor symbol table
                                                        bool found = false;
                                                        parentTable = Col_ST[currentTable]->parent;
                                                        while (!found) {
                                                            if(parentTable == NULL) {
                                                                return yyerror("Error: Identifier is not assigned yet anywhere");
                                                            }
                                                            
                                                            foundIndex = lookupInTable(Col_ST[currentTable]->strList[$4], parentTable);
                                                            if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
                                                                found = true;
                                                                break;
                                                            }
                                                            printf("FOUND in parent %s (table %d, index %d)! for get ID\n", Col_ST[currentTable]->strList[$4], parentTable->index, foundIndex);

                                                            parentTable = parentTable->parent;
                                                        }
                                                        printf("FOUND in parent! for FOR loop\n");
                                                        
                                                        // CHOSEN TABLE TO PARENT TABLE
                                                        chosenTable = parentTable;
                                                    }

                                                    if(parentTable == NULL) {
                                                        // JUST FOR CURRENT TABLE VARIABLE
                                                        chosenTable = Col_ST[currentTable];
                                                        foundIndex = $4;
                                                    }
                                                    
                                                    //CheckType
                                                    if(chosenTable->storeType[foundIndex] != VARIABLE || chosenTable->valueType[foundIndex] != VALINT) {
                                                        return yyerror("Error: Identifier is not VARIABLE type and not INTEGER type");
                                                    }
                                                }
    COLON                                       { // need const expression
                                                    createChildTable(); 
                                                    printSymbols();
                                                    needConstExp = false;
                                                } 
    exp DOT DOT exp stmt_body END FOR                           {
                                                    returnToParent();
                                                    printSymbols();
                                                }
    ;
for_ext: /* empty */
    | DECREASING    
/* ----------------------- */

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
    Col_ST[currentTable]->funcAtr = malloc(Col_ST[currentTable]->capacity * sizeof(struct FuncAtr));

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

    if(Col_ST[currentTable]->arrayAtr == NULL) {
        printf("Unable to allocate memory for arrayAtr:(\n");
        return;
    }

    if(Col_ST[currentTable]->funcAtr == NULL) {
        printf("Unable to allocate memory for funcAtr:(\n");
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
    for(int i = 0; i < Col_ST[currentTable]->size; i++) {
		if(strcmp(Col_ST[currentTable]->strList[i], s) == 0) {
			return i;
		}
	}
	
	return -1;
}

int lookupInTable(char* s, struct SymbolTable* parentTable) {

    for(int i = 0; i < parentTable->size; i++) {
		if(strcmp(parentTable->strList[i], s) == 0) {
			return i;
		}
	}
	
	return -1;
}

int insert(char* s) {     
    if(Col_ST[currentTable]->size == Col_ST[currentTable]->capacity) {
        Col_ST[currentTable]->capacity *= 2;
        Col_ST[currentTable]->strList = realloc(Col_ST[currentTable]->strList, Col_ST[currentTable]->capacity * sizeof(char*));
		Col_ST[currentTable]->valueType = realloc(Col_ST[currentTable]->valueType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->valueType));
		Col_ST[currentTable]->storeType = realloc(Col_ST[currentTable]->storeType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->storeType));
        Col_ST[currentTable]->arrayAtr = realloc(Col_ST[currentTable]->arrayAtr, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->arrayAtr));
        Col_ST[currentTable]->funcAtr = realloc(Col_ST[currentTable]->funcAtr, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->funcAtr));

        if(Col_ST[currentTable]->strList == NULL) {
            printf("Unable to reallocate memory :(\n");
            return -1;
        }
    }
	Col_ST[currentTable]->strList[Col_ST[currentTable]->size] = malloc(strlen(s) + 1);
	strcpy(Col_ST[currentTable]->strList[Col_ST[currentTable]->size], s);
    Col_ST[currentTable]->storeType[Col_ST[currentTable]->size] = NONE;
    Col_ST[currentTable]->valueType[Col_ST[currentTable]->size] = VALNONE;

	Col_ST[currentTable]->size++;
	
	return Col_ST[currentTable]->size - 1;
}

void dump() {
	printf("\n");
	printSymbols();
	printf("Dumping all identifiers\n");
    for(int i = 0; i < currentSize; i++) {
        free(Col_ST[i]->strList);
        printf("Free strList\n");
        free(Col_ST[i]->valueType);
        printf("Free valueType\n");
        free(Col_ST[i]->storeType);
        printf("Free storeType\n");
        free(Col_ST[i]->arrayAtr);
        printf("Free arrayAtr\n");
        free(Col_ST[i]->funcAtr);
        printf("Free funcAtr\n");
        free(Col_ST[i]);
        printf("Free Col_ST INDEX %d\n", i);
    }
    free(Col_ST);
}

void printSymbols() {
    printf("======================================================\n");
	printf("\nCurrent Table is  %d:\n", currentTable);
	for(int i = 0; i < currentSize; i++) {
	    printf("\nSymbol Table %d:\n", i);
        printf("Index\tStoreType\tValueType\tIDname\n");
        for(int j = 0; j < Col_ST[i]->size; j++) {
            // printf("%d\t%d\t\t%d\t\t%s\n", j, Col_ST[i]->storeType[j], Col_ST[i]->valueType[j], Col_ST[i]->strList[j]);
            printf("%d\t", j);
            switch((int)Col_ST[i]->storeType[j]) {
                case NONE:
                    printf("NONE\t\t");
                    break;
                case CONSTANT:
                    printf("CONST\t\t");
                    break;
                case VARIABLE:
                    printf("VAR\t\t");
                    break;
                case MULTIVAR:
                    printf("ARRAY\t\t");
                    break;
                case FUNC:
                    printf("FUNC\t\t");
                    break;
                case PROC:
                    printf("PROC\t\t");
                    break;
            }
            switch((int)Col_ST[i]->valueType[j]) {
                case VALNONE:
                    printf("NONE\t\t");
                    break;
                case VALINT:
                    printf("INT\t\t");
                    break;
                case VALREAL:
                    printf("REAL\t\t");
                    break;
                case VALSTR:
                    printf("STR\t\t");
                    break;
                case VALBOOL:
                    printf("BOOL\t\t");
                    break;
            }
            printf("%s\n", Col_ST[i]->strList[j]);
        }
    }
    printf("======================================================\n\n");
}

void printChosenSymbols(int index) {
	printf("\nSymbol Table %d:\n", index);
    printf("==================\n");
    printf("Index\tStoreType\tValueType\tIDname\n");
	for(int i = 0; i < Col_ST[index]->size; i++) {
		printf("%d\t%d\t\t%d\t\t%s\n", i, Col_ST[index]->storeType[i], Col_ST[index]->valueType[i], Col_ST[index]->strList[i]);
    }
    printf("==================\n\n");
}

void resetFunctionGrammarFlags() {
    funcCalling = false;
    funcDeclaration = false;
    currentParamIndex = 0;
    // functionParent = NULL;
    atParentIndex = -1;
    scopeReturnType = -1;
    funcScope = false;
    procScope = false;
    isCallingArray = false;
}

void initFlags() {
    funcScope = false;
    procScope = false;
    needConstExp = false;
    funcDeclaration = false;
    funcCalling = false;
    currentParamIndex = 0;
    atParentIndex = -1;
    functionParent = NULL;
    scopeReturnType = -1;
    foundIndex = -1;
    topTerm = -1;
    topFuncCalling = -1;
    isCallingArray = false;
}

int yyerror(char *s) {
   fprintf(stderr, "(Line No:%d) %s\n", yylineno, s);   
   return 0; 
}

int main(int argc, char** argv)
{
    if (argc > 0) {
        yyin = fopen(argv[1], "r");/* open input file */
    } else {
        yyin = stdin;
    }         
    initFlags();

    /* perform parsing */
    create();
    yyparse();
    dump();

    return 0;
}

