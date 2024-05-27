SuperStrict
Import "../syntax/SyntaxBase.bmx"
Import BRL.LinkedList
Import BRL.StandardIO



Private

Const ThrowOnSemanticError:Int = False

Public

' TODO: this does not belong here
Global SemanticErrors:TList = New TList'<TSemanticError>

Type TSemanticError Final
	Field ReadOnly message:String
	Field ReadOnly syntax:ISyntax
	
	Private
	Method New() End Method
	
	Public
	Method New(message:String, syntax:ISyntax)
		Self.message = message
		Self.syntax = syntax
	End Method
	
	Method ToString:String() Override
		Return message + " (at " + syntax.CodeRange().ToString() + ")"
	End Method
End Type



Function ReportError(message:String, syntax:ISyntax) ' TODO: this should be part of a compilation, not global
	SemanticErrors.AddLast New TSemanticError(message, syntax)
	If ThrowOnSemanticError Then Throw message
End Function