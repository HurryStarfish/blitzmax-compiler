SuperStrict
Import "Symbols.bmx"
Import BRL.Map



Type TScope Final
	Private
	Field ReadOnly symbols:TMap = New TMap'<String, TNamedSymbol> ' key is not the same as name
	Field ReadOnly parent:TScope
	
	Method New() End Method
	
	Public
	Method New(parent:TScope)
		Self.parent = parent
	End Method
	
	Private
	Function NameToKey:String(name:String)
		Return name.ToLower()
	End Function
	'Function Key:String(symbol:?)
	'	Return symbol.name.ToLower()
	'End Function
	' TODO: add nameless symbols (e.g. constructors) with a special key prefix
	' TODO: support overloads
	
	Public
	Method AddSymbol(symbol:TNamedSymbol)
		Local key:String = NameToKey(symbol.name)
		Assert Not symbols.Contains(key) Else "Scope already contains a symbol with this name"
		symbols[key] = symbol
	End Method
	'Method AddSymbol(symbol:TVariableSymbol)
	'Method AddSymbol(symbol:TCallableSymbol) ' support overloads
	'Method AddSymbol(symbol:TTypeSymbol)
	
	Method GetSymbolForName:TNamedSymbol(name:String) ' returns null if not found
		Return TNamedSymbol(symbols[NameToKey(name)])
	End Method
	
	Method ToString:String() Override
		Local str:String
		For Local symbol:TNamedSymbol = EachIn symbols.Values()
			If str Then str :+ ", "
			str :+ symbol.ToString()
		Next
		Return "Scope [" + str + "]"
	End Method
End Type
