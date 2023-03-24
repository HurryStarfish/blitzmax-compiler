SuperStrict
Import "Symbols.bmx"
Import BRL.Map



Type TScope Final
	Private
	Field ReadOnly symbols:TMap = New TMap'<String, IDeclaration> ' key is not the same as name
	Field ReadOnly parent:TScope
	
	Method New() End Method
	
	Public
	Method New(parent:TScope)
		Self.parent = parent
	End Method
	
	Method AddSymbol(symbol:IDeclaration)
		Assert Not symbols.Contains(New SSymbolKey(symbol).ToString()) Else "Scope already contains a symbol with this name"
		symbols[New SSymbolKey(symbol).ToString()] = symbol
	End Method
	
	'Method GetSymbolForName:TNamedSymbol(name:String) ' returns null if not found
	'	Return TNamedSymbol(symbols[(name)])
	'End Method
	
	Method ToString:String() Override
		Local str:String
		For Local symbol:IDeclaration = EachIn symbols.Values()
			If str Then str :+ ", "
			str :+ symbol.ToString()
		Next
		Return "Scope [" + str + "]"
	End Method
End Type



Struct SSymbolKey
	Private
	Field ReadOnly str:String
	
	Public
	Method New(declaration:IDeclaration)
		' TODO: support overloads
		If declaration.IsReferrableByName() Then str = declaration.GetName().ToLower() Else str = "~~" + declaration.GetName().ToLower()
	End Method
	
	Method ToString:String()
		Return str
	End Method
End Struct
