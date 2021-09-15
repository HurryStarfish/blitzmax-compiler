SuperStrict



Type TTokenKind Final ' TODO: turn into struct once reflection is less broken
	
	Field ReadOnly name:String
	Field ReadOnly canonicalValue:String {nullable} ' null if there is no reasonablecanonical value, e.g. for identifiers
	
	Private
	Method New(name:String)
		New(name, name)
	End Method
	
	Method New(name:String, canonicalValue:String)
		Self.name = name
		Self.canonicalValue = canonicalValue
	End Method
	
	Public
	Method Operator =:Int(other:TTokenKind)
		Return other And Self.name = other.name
	End Method
	
	Method Operator <>:Int(other:TTokenKind)
		Return Not (Self = other)
	End Method
	
	Method ToString:String() Override
		Return name
	End Method
	
	Method ToCode:String() ' to generate missing tokens
		If Not canonicalValue Then RuntimeError "Token of kind " + name + " has no canonical value"
		Return canonicalValue
	End Method
	
		
	
	' keep these in sync with Lexer.flex
	
	Global Include_:TTokenKind = New TTokenKind("Include")
	
	Global Strict_:TTokenKind = New TTokenKind("Strict")
	Global SuperStrict_:TTokenKind = New TTokenKind("SuperStrict")
	
	Global Framework_:TTokenKind = New TTokenKind("Framework")
	Global Import_:TTokenKind = New TTokenKind("Import")
	Global Module_:TTokenKind = New TTokenKind("Module")
	Global ModuleInfo_:TTokenKind = New TTokenKind("ModuleInfo")
	
	Global Incbin_:TTokenKind = New TTokenKind("Incbin")
	Global IncbinPtr_:TTokenKind = New TTokenKind("IncbinPtr")
	Global IncbinLen_:TTokenKind = New TTokenKind("IncbinLen")
	
	Global True_:TTokenKind = New TTokenKind("True")
	Global False_:TTokenKind = New TTokenKind("False")
	Global Pi_:TTokenKind = New TTokenKind("Pi")
	Global Null_:TTokenKind = New TTokenKind("Null")
	Global Self_:TTokenKind = New TTokenKind("Self")
	
	Global Byte_:TTokenKind = New TTokenKind("Byte")
	Global Short_:TTokenKind = New TTokenKind("Short")
	Global Int_:TTokenKind = New TTokenKind("Int")
	Global UInt_:TTokenKind = New TTokenKind("UInt")
	Global Long_:TTokenKind = New TTokenKind("Long")
	Global ULong_:TTokenKind = New TTokenKind("ULong")
	Global Size_T_:TTokenKind = New TTokenKind("Size_T")
	Global Float_:TTokenKind = New TTokenKind("Float")
	Global Double_:TTokenKind = New TTokenKind("Double")
	Global String_:TTokenKind = New TTokenKind("String")
	Global Object_:TTokenKind = New TTokenKind("Object")
	Global LParam_:TTokenKind = New TTokenKind("LParam")
	Global WParam_:TTokenKind = New TTokenKind("WParam")
	Global Float64_:TTokenKind = New TTokenKind("Float64")
	Global Float128_:TTokenKind = New TTokenKind("Float128")
	Global Double128_:TTokenKind = New TTokenKind("Double128")
	Global Int128_:TTokenKind = New TTokenKind("Int128")
	
	Global Ptr_:TTokenKind = New TTokenKind("Ptr")
	Global Var_:TTokenKind = New TTokenKind("Var")
	
	Global If_:TTokenKind = New TTokenKind("If")
	Global EndIf_:TTokenKind = New TTokenKind("End If")
	Global Then_:TTokenKind = New TTokenKind("Then")
	Global Else_:TTokenKind = New TTokenKind("Else")
	Global ElseIf_:TTokenKind = New TTokenKind("Else If")
	Global For_:TTokenKind = New TTokenKind("For")
	Global To_:TTokenKind = New TTokenKind("To")
	Global Step_:TTokenKind = New TTokenKind("Step")
	Global Next_:TTokenKind = New TTokenKind("Next")
	Global EachIn_:TTokenKind = New TTokenKind("EachIn")
	Global While_:TTokenKind = New TTokenKind("While")
	Global Wend_:TTokenKind = New TTokenKind("Wend")
	Global EndWhile_:TTokenKind = New TTokenKind("Wend")
	Global Repeat_:TTokenKind = New TTokenKind("Repeat")
	Global Until_:TTokenKind = New TTokenKind("Until")
	Global Forever_:TTokenKind = New TTokenKind("Forever")
	Global Select_:TTokenKind = New TTokenKind("Select")
	Global EndSelect_:TTokenKind = New TTokenKind("End Select")
	Global Case_:TTokenKind = New TTokenKind("Case")
	Global Default_:TTokenKind = New TTokenKind("Default")
	
	Global Exit_:TTokenKind = New TTokenKind("Exit")
	Global Continue_:TTokenKind = New TTokenKind("Continue")
	Global Return_:TTokenKind = New TTokenKind("Return")
	Global Goto_:TTokenKind = New TTokenKind("Goto")
	Global Try_:TTokenKind = New TTokenKind("Try")
	Global EndTry_:TTokenKind = New TTokenKind("End Try")
	Global Catch_:TTokenKind = New TTokenKind("Catch")
	Global Finally_:TTokenKind = New TTokenKind("Finally")
	Global Throw_:TTokenKind = New TTokenKind("Throw")
	Global Assert_:TTokenKind = New TTokenKind("Assert")
	Global End_:TTokenKind = New TTokenKind("End")
	
	Global Alias_:TTokenKind = New TTokenKind("Alias")
	Global Const_:TTokenKind = New TTokenKind("Const")
	Global Global_:TTokenKind = New TTokenKind("Global")
	Global Local_:TTokenKind = New TTokenKind("Local")
	Global Field_:TTokenKind = New TTokenKind("Field")
	Global Function_:TTokenKind = New TTokenKind("Function")
	Global EndFunction_:TTokenKind = New TTokenKind("End Function")
	Global Method_:TTokenKind = New TTokenKind("Method")
	Global EndMethod_:TTokenKind = New TTokenKind("End Method")
	
	Global Type_:TTokenKind = New TTokenKind("Type")
	Global EndType_:TTokenKind = New TTokenKind("End Type")
	Global Struct_:TTokenKind = New TTokenKind("Struct")
	Global EndStruct_:TTokenKind = New TTokenKind("End Struct")
	Global Interface_:TTokenKind = New TTokenKind("Interface")
	Global EndInterface_:TTokenKind = New TTokenKind("End Interface")
	Global Enum_:TTokenKind = New TTokenKind("Enum")
	Global EndEnum_:TTokenKind = New TTokenKind("End Enum")
	
	Global Extends_:TTokenKind = New TTokenKind("Extends")
	Global Implements_:TTokenKind = New TTokenKind("Implements")
	Global Super_:TTokenKind = New TTokenKind("Super")
	
	Global ReadOnly_:TTokenKind = New TTokenKind("ReadOnly")
	
	Global Abstract_:TTokenKind = New TTokenKind("Abstract")
	Global Final_:TTokenKind = New TTokenKind("Final")
	Global Override_:TTokenKind = New TTokenKind("Override")
	Global Operator_:TTokenKind = New TTokenKind("Operator")
	Global Inline_:TTokenKind = New TTokenKind("Inline")
 
	Global Extern_:TTokenKind = New TTokenKind("Extern")
	Global EndExtern_:TTokenKind = New TTokenKind("End Extern")
	Global Export_:TTokenKind = New TTokenKind("Export")
	
	Global New_:TTokenKind = New TTokenKind("New")
	Global Delete_:TTokenKind = New TTokenKind("Delete")
	Global Release_:TTokenKind = New TTokenKind("Release")
	
	Global Public_:TTokenKind = New TTokenKind("Public")
	Global Protected_:TTokenKind = New TTokenKind("Protected")
	Global Private_:TTokenKind = New TTokenKind("Private")
	
	Global DefData_:TTokenKind = New TTokenKind("DefData")
	Global ReadData_:TTokenKind = New TTokenKind("ReadData")
	Global RestoreData_:TTokenKind = New TTokenKind("RestoreData")
	
	Global And_:TTokenKind = New TTokenKind("And")
	Global Or_:TTokenKind = New TTokenKind("Or")
	Global Not_:TTokenKind = New TTokenKind("Not")
	Global Shl_:TTokenKind = New TTokenKind("Shl")
	Global Shr_:TTokenKind = New TTokenKind("Shr")
	Global Sar_:TTokenKind = New TTokenKind("Sar")
	Global Mod_:TTokenKind = New TTokenKind("Mod")
	Global Asc_:TTokenKind = New TTokenKind("Asc")
	Global Chr_:TTokenKind = New TTokenKind("Chr")
	Global Len_:TTokenKind = New TTokenKind("Len")
	Global Varptr_:TTokenKind = New TTokenKind("Varptr")
	Global SizeOf_:TTokenKind = New TTokenKind("SizeOf")


	
	Global LParen:TTokenKind = New TTokenKind("LParen", "(")
	Global RParen:TTokenKind = New TTokenKind("RParen", ")")
	Global LBrace:TTokenKind = New TTokenKind("LBrace", "{")
	Global RBrace:TTokenKind = New TTokenKind("RBrace", "}")
	Global LBracket:TTokenKind = New TTokenKind("LBracket", "[")
	Global RBracket:TTokenKind = New TTokenKind("RBracket", "]")
	Global ByteSigil:TTokenKind = New TTokenKind("ByteSigil", "@")
	Global IntSigil:TTokenKind = New TTokenKind("IntSigil", "%")
	Global FloatSigil:TTokenKind = New TTokenKind("FloatSigil", "#")
	Global DoubleSigil:TTokenKind = New TTokenKind("DoubleSigil", "!")
	Global StringSigil:TTokenKind = New TTokenKind("StringSigil", "$")
	Global Semicolon:TTokenKind = New TTokenKind("Semicolon", ";")
	Global Colon:TTokenKind = New TTokenKind("Colon", ":")
	Global Comma:TTokenKind = New TTokenKind("Comma", ",")
	Global Dot:TTokenKind = New TTokenKind("Dot", ".")
	Global DotDot:TTokenKind = New TTokenKind("DotDot", "..")
	Global QuestionMark:TTokenKind = New TTokenKind("QuestionMark", "?")
	
	Global Plus:TTokenKind = New TTokenKind("Plus", "+")
	Global Minus:TTokenKind = New TTokenKind("Minus", "-")
	Global Mul:TTokenKind = New TTokenKind("Mul", "*")
	Global Div:TTokenKind = New TTokenKind("Div", "/")
	Global Pow:TTokenKind = New TTokenKind("Pow", "^")
	Global Neq:TTokenKind = New TTokenKind("Neq", "<>")
	Global Leq:TTokenKind = New TTokenKind("Leq", "<=")
	Global Geq:TTokenKind = New TTokenKind("Geq", ">=")
	Global Lt:TTokenKind = New TTokenKind("Lt", "<")
	Global Gt:TTokenKind = New TTokenKind("Gt", ">")
	Global Eq:TTokenKind = New TTokenKind("Eq", "=")
	Global InfEq:TTokenKind = New TTokenKind("InfEq", ":=")
	Global BitAnd:TTokenKind = New TTokenKind("BitAnd", "&")
	Global BitOr:TTokenKind = New TTokenKind("BitOr", "|")
	Global BitNot:TTokenKind = New TTokenKind("BitNot", "~~")
	
	
	
	Global Whitespace:TTokenKind = New TTokenKind("Whitespace", Null)
	Global Linebreak:TTokenKind = New TTokenKind("Linebreak", "~n")
	
	
	
	Global IntLiteral:TTokenKind = New TTokenKind("IntLiteral", Null)
	Global HexIntLiteral:TTokenKind = New TTokenKind("HexIntLiteral", Null)
	Global BinIntLiteral:TTokenKind = New TTokenKind("BinIntLiteral", Null)
	Global FloatLiteral:TTokenKind = New TTokenKind("FloatLiteral", Null)
	Global StringLiteral:TTokenKind = New TTokenKind("StringLiteral", Null)
	
	Global NativeCode:TTokenKind = New TTokenKind("NativeCode", Null)
	
	Global Comment:TTokenKind = New TTokenKind("Comment", Null)
	
	Global Rem_:TTokenKind = New TTokenKind("Rem")
	Global RemComment:TTokenKind = New TTokenKind("RemComment", Null)
	Global EndRem_:TTokenKind = New TTokenKind("End Rem")

	Global Identifier:TTokenKind = New TTokenKind("Identifier", Null)
	
	
	
	Global Eof:TTokenKind = New TTokenKind("EOF", Null) ' no equivalent in the flex file
	
	Global InvalidCode:TTokenKind = New TTokenKind("InvalidCode", Null) ' no equivalent in the flex file
	
End Type

