SuperStrict
Import "ParseError.bmx"
Import "../syntax/Syntax.bmx"



Interface IParser
	Method ParseCompilationUnit:TCompilationUnitSyntax()
	Method Errors:TList()'<TParseError>
End Interface

