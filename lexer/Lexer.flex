
%option case-insensitive
%option noyywrap

%{
#include <brl.mod/blitz.mod/blitz.h>

static char* strndup_(const char* str, size_t size) { // strndup might or might now be available in string.h
	char* newstr = (char*)malloc(sizeof(char) * (size + 1));
	memcpy(newstr, str, size);
	newstr[size] = '\0';
	return newstr;
}

typedef struct TokenC {
	char* value;
	char* tokenKind;
	char* filePath;
	unsigned int line;
	unsigned int column;
} TokenC;

void TokenC_freeStrings(TokenC* token) {
	free(token->value);
	free(token->tokenKind);
	free(token->filePath);
}



void (*ReportLexerError)(BBSTRING filePath, BBUINT line, BBUINT column) = NULL;



#define YY_DECL int yylex(TokenC* token, FilePosition* pos)

typedef struct FilePosition {
	unsigned int currentLine;
	unsigned int currentColumn;
	unsigned int nextLine;
	unsigned int nextColumn;
} FilePosition;

void resetFilePosition(FilePosition* pos) {
	pos->nextLine = 1;
	pos->nextColumn = 1;
}

#define YY_USER_ACTION {                        \
	pos->currentLine   = pos->nextLine;         \
	pos->currentColumn = pos->nextColumn;       \
	int c;                                      \
	for (c = 0; c < yyleng; c++) {              \
		if (yytext[c] == '\n') {                \
			pos->nextLine++;                    \
			pos->nextColumn = 1;                \
		} else {                                \
			pos->nextColumn++;                  \
		}                                       \
	}                                           \
}

#define STOREAS(tKind) {                        \
	token->value     = strndup_(yytext, yyleng);\
	token->tokenKind = strdup(tKind);           \
	token->filePath  = strdup(""); /*TODO*/     \
	token->line      = pos->currentLine;        \
	token->column    = pos->currentColumn;      \
	return;                                     \
}
#define DISCARD {                               \
}
#define ERROR {                                 \
	char* filePath = strdup(""); /*TODO*/       \
	ReportLexerError(bbStringFromCString(filePath), token->line, token->column); \
	free(filePath);                             \
	return 0;                                   \
}

#define NEWLINE_CHARS \r\n
%}

/* exclusive start conditions */
%x REMCOMMENT_FIRST_LINE REMCOMMENT

NEWLINE \r\n|\r|\n
/* TLexerToken.CodeRange must be kept in sync with the token kinds newlines are used in */

%%



	/* keep these in sync with LexerTokenKind.bmx */

Include STOREAS("Include")

Strict STOREAS("Strict")
SuperStrict STOREAS("SuperStrict")

Framework STOREAS("Framework")
Import STOREAS("Import")
Module STOREAS("Module")
ModuleInfo STOREAS("ModuleInfo")

Incbin STOREAS("Incbin")
IncbinPtr STOREAS("IncbinPtr")
IncbinLen STOREAS("IncbinLen")

True STOREAS("True")
False STOREAS("False")
Pi STOREAS("Pi")
Null STOREAS("Null")
Self STOREAS("Self")

Byte STOREAS("Byte")
Short STOREAS("Short")
Int STOREAS("Int")
UInt STOREAS("UInt")
Long STOREAS("Long")
ULong STOREAS("ULong")
Size_T STOREAS("Size_T")
Float STOREAS("Float")
Double STOREAS("Double")
String STOREAS("String")
Object STOREAS("Object")
LParam STOREAS("LParam")
WParam STOREAS("WParam")
Float64 STOREAS("Float64")
Float128 STOREAS("Float128")
Double128 STOREAS("Double128")
Int128 STOREAS("Int128")

Ptr STOREAS("Ptr")
Var STOREAS("Var")

If STOREAS("If")
End[ \t]?If STOREAS("End If")
Then STOREAS("Then")
Else STOREAS("Else")
Else[ \t]?If STOREAS("Else If")
For STOREAS("For")
To STOREAS("To")
Step STOREAS("Step")
Next STOREAS("Next")
EachIn STOREAS("EachIn")
While STOREAS("While")
Wend STOREAS("Wend")
End[ \t]?While STOREAS("Wend")
Repeat STOREAS("Repeat")
Until STOREAS("Until")
Forever STOREAS("Forever")
Select STOREAS("Select")
End[ \t]?Select STOREAS("End Select")
Case STOREAS("Case")
Default STOREAS("Default")

Exit STOREAS("Exit")
Continue STOREAS("Continue")
Return STOREAS("Return")
Goto STOREAS("Goto")
Try STOREAS("Try")
End[ \t]?Try STOREAS("End Try")
Catch STOREAS("Catch")
Finally STOREAS("Finally")
Throw STOREAS("Throw")
Assert STOREAS("Assert")
End STOREAS("End")

Alias STOREAS("Alias")
Const STOREAS("Const")
Global STOREAS("Global")
Local STOREAS("Local")
Field STOREAS("Field")
Function STOREAS("Function")
End[ \t]?Function STOREAS("End Function")
Method STOREAS("Method")
End[ \t]?Method STOREAS("End Method")

Type STOREAS("Type")
End[ \t]?Type STOREAS("End Type")
Struct STOREAS("Struct")
End[ \t]?Struct STOREAS("End Struct")
Interface STOREAS("Interface")
End[ \t]?Interface STOREAS("End Interface")
Enum STOREAS("Enum")
End[ \t]?Enum STOREAS("End Enum")

Extends STOREAS("Extends")
Implements STOREAS("Implements")
Super STOREAS("Super")

ReadOnly STOREAS("ReadOnly")

Abstract STOREAS("Abstract")
Final STOREAS("Final")
Override STOREAS("Override")
Operator STOREAS("Operator")
Inline STOREAS("Inline")

Extern STOREAS("Extern")
End[ \t]?Extern STOREAS("End Extern")
Export STOREAS("Export")

New STOREAS("New")
Delete STOREAS("Delete")
Release STOREAS("Release")

Public STOREAS("Public")
Protected STOREAS("Protected")
Private STOREAS("Private")

DefData STOREAS("DefData")
ReadData STOREAS("ReadData")
RestoreData STOREAS("RestoreData")

And STOREAS("And")
Or  STOREAS("Or")
Not STOREAS("Not")
Mod STOREAS("Mod")
Shl STOREAS("Shl")
Shr STOREAS("Shr")
Sar STOREAS("Sar")
	/* Min STOREAS("Min") */
	/* Max STOREAS("Max") */
Asc STOREAS("Asc")
Chr STOREAS("Chr")
Len STOREAS("Len")
Varptr STOREAS("Varptr")
SizeOf STOREAS("SizeOf")



"(" STOREAS("LParen")
")" STOREAS("RParen")
"{" STOREAS("LBrace")
"}" STOREAS("RBrace")
"[" STOREAS("LBracket")
"]" STOREAS("RBracket")
"@" STOREAS("ByteSigil")
"%" STOREAS("IntSigil")
"#" STOREAS("FloatSigil")
"!" STOREAS("DoubleSigil")
"$" STOREAS("StringSigil")
";" STOREAS("Semicolon")
":" STOREAS("Colon")
"," STOREAS("Comma")
"." STOREAS("Dot")
".." STOREAS("DotDot")
"?" STOREAS("QuestionMark")

"&" STOREAS("BitAnd")
"|" STOREAS("BitOr")
"~" STOREAS("BitNot")
"+" STOREAS("Plus")
"-" STOREAS("Minus")
"*" STOREAS("Mul")
"/" STOREAS("Div")
"^" STOREAS("Pow")
"<>" STOREAS("Neq")
"<=" STOREAS("Leq")
">=" STOREAS("Geq")
"<" STOREAS("Lt")
">" STOREAS("Gt")
"=" STOREAS("Eq")
":=" STOREAS("InfEq")

":&" STOREAS("ColonBitAnd")
":|" STOREAS("ColonBitOr")
":~" STOREAS("ColonBitNot")
":+" STOREAS("ColonPlus")
":-" STOREAS("ColonMinus")
":*" STOREAS("ColonMul")
":/" STOREAS("ColonDiv")
:Mod STOREAS("ColonMod")
:Shl STOREAS("ColonShl")
:Shr STOREAS("ColonShr")
:Sar STOREAS("ColonSar")

	/* "\\" STOREAS("Lambda") */
	/* "->" STOREAS("RArrow") */



[ \t]*    STOREAS("Whitespace")
{NEWLINE} STOREAS("Linebreak")



[0-9]+                          STOREAS("IntLiteral")
\$[0-9a-fA-F]+                  STOREAS("HexIntLiteral")
\%[0-1]+                        STOREAS("BinIntLiteral")
[0-9]*\.[0-9]+([eE]-?[0-9]+)?   |
[0-9]+[eE]-?[0-9]+              STOREAS("FloatLiteral")
\"([^\r\n]{-}[\"])*\"           STOREAS("StringLiteral")

'![^\r\n]*                      STOREAS("NativeCode")

'[^\r\n]*                       STOREAS("Comment")

Rem                                                                         BEGIN(REMCOMMENT_FIRST_LINE); STOREAS("Rem")
<REMCOMMENT_FIRST_LINE>[^\r\n]*{NEWLINE}                                    BEGIN(REMCOMMENT);            STOREAS("RemComment")
<REMCOMMENT>[ \t]*                                                          BEGIN(REMCOMMENT);            STOREAS("RemComment")
<REMCOMMENT>[ \t]*End[ \t]?Rem/{NEWLINE}                                    BEGIN(INITIAL);               STOREAS("End Rem")
<REMCOMMENT>[ \t]*End[ \t]?Rem/(([^\r\n]{-}[a-zA-Z0-9_])[^\r\n]*{NEWLINE})  BEGIN(INITIAL);               STOREAS("End Rem")
<REMCOMMENT>[^\r\n]*{NEWLINE}                                               BEGIN(REMCOMMENT);            STOREAS("RemComment")
End[ \t]?Rem                                                                                              STOREAS("End Rem")



[a-zA-Z_][a-zA-Z0-9_]*          STOREAS("Identifier")



<*>. STOREAS("InvalidCode")



%%



int FlexScanFile(
	BBString* filePath,
	void* tokenList,
	//BBString* processFilename(BBString* filename),
	void* createToken(BBSTRING value, BBSTRING tokenKind, BBSTRING filePath, BBUINT line, BBUINT column),
	void* addToList(void* list, void* token)
) {
	
	char* filePathC = bbStringToCString(filePath);
	FILE* file = fopen(filePathC, "r");
	if (file == NULL) {
		bbMemFree(filePathC);
		return 0;
	}
	FilePosition filePosition;
	resetFilePosition(&filePosition);
	
	yyin = file;
	TokenC tokenC;
	while (yylex(&tokenC, &filePosition) != 0) {
		void* token = createToken(bbStringFromCString(tokenC.value), bbStringFromCString(tokenC.tokenKind), /*bbStringFromCString(tokenC.filePath)*/ filePath, tokenC.line, tokenC.column);
		addToList(tokenList, token);
		TokenC_freeStrings(&tokenC);
	};
	
	fclose(file);
	bbMemFree(filePathC);
	return 1;
}


