SuperStrict

Rem
Local f:Int(a:Float, b:String)
localsymbol
	name=f
	type=int(float,string)
	paramnames=[a,b]
	
localsymbol
	name=f
	type=*t
*t=type
	int(float,string)
	paramnames=[a,b]
	
localsymbol
	name=f
	type=*t
*t=functiontypesymbol
	paramnames=[a,b]
	type=int(float,string)
End Rem

'Local p:Int(a:String) Ptr
' whats the type of the ptr referent field? does it have param names?
' yes it should, they are needed for e.g. Local f := p[0]
' * Ptr scope has Method Operator []:referent()


' scopes should be attached to syntax nodes
' parents and children can be found by walking the tree
' 
' scopes contain symbols
' 
Rem
Local x:Int = y
syntax: variabledeclaration <- operation: variabledeclaration
	syntax: variabledeclarator <- operation: variabledeclarator
		syntax: assignment <- operation: variableinitializer
			syntax: name <- operation: local/field/...reference, or invalid (if name cant be resolved)
				localreferenceoperation.local returns a localsymbol
		variabledeclaratoroperation.symbol returns a localsymbol
		(variabledeclaratoroperation.declarators returns an array ... is this needed?)
		(getting the type could be done by inspecting the symbol being declared)		
(every operation has a type symbol, parent and child operations, syntax and optional constant value)
(every symbol has getmembers (or getscope?))

methoddeclarationsyntax has a methodbodyoperation (declared symbol where?) which has a blockoperation
End Rem



Rem
typesymbol: any appearance of a type in code, with name for udts, param names for callables, tuple element names etc.
a symbol is roughly anything that can be declared somewhere (type, local, field, module, label...)
	also includes compound types like Ptrs
an operation is roughly any statement/expression or any component of a declaration that is not
part of a symbol (declarator, initializer, block...)
End Rem



Rem
Type TOperation Abstract
	Protected
	Method New() End Method
End Type



Type TBlock Extends TOperation Final
	Field ReadOnly scope:TScope
	
	Method New(scope:TScope)
		Self.scope = scope
	End Method
End Type
End Rem



Type TSymbol Abstract
	Protected
	Method New() End Method
	
	Public
	Method ToString:String() Override Abstract
End Type



Type TNamedSymbol Extends TSymbol Abstract ' TODO: symbol visibility
	Field ReadOnly name:String 'todo: 'take syntax as parameter instead, name can be a method
End Type



Type TVariableSymbol Extends TNamedSymbol Abstract
End Type



Type TConstSymbol Extends TVariableSymbol Final
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Const " + name
	End Method
End Type



Type TGlobalSymbol Extends TVariableSymbol Final
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Global " + name
	End Method
End Type



Type TLocalSymbol Extends TVariableSymbol Final
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Local " + name
	End Method
End Type



Type TFieldSymbol Extends TVariableSymbol Final
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Field " + name
	End Method
End Type



'Type TCallableSymbol Extends TNamedSymbol
'End Type



Type TConstructorSymbol Extends TSymbol Final 'TCallableSymbol
	Method New()
	End Method
	
	Method ToString:String() Override
		Return "New"
	End Method
End Type



Type TFinalizerSymbol Extends TSymbol Final 'TCallableSymbol
	Method New()
	End Method
	
	Method ToString:String() Override
		Return "Delete"
	End Method
End Type



Type TFunctionSymbol Extends TNamedSymbol
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Function " + name
	End Method
End Type



Type TMethodSymbol Extends TNamedSymbol
	'Field ReadOnly body:TBlock
	
	Method New(name:String)', body:TBlock)
		Self.name = name
		'Self.body = body
	End Method
	
	Method ToString:String() Override
		Return "Method " + name
	End Method
End Type



TTypeSymbol.Byte_    = New TStructTypeSymbol("Byte")
TTypeSymbol.Short_   = New TStructTypeSymbol("Short")
TTypeSymbol.Int_     = New TStructTypeSymbol("Int")
TTypeSymbol.UInt_    = New TStructTypeSymbol("UInt")
TTypeSymbol.Long_    = New TStructTypeSymbol("Long")
TTypeSymbol.ULong_   = New TStructTypeSymbol("ULong")
TTypeSymbol.Float_   = New TStructTypeSymbol("Float")
TTypeSymbol.Double_  = New TStructTypeSymbol("Double")
TTypeSymbol.Object_  = New TClassTypeSymbol("Object", Null, Null)
TTypeSymbol.String_  = New TClassTypeSymbol("String", TTypeSymbol.Object_, Null)
TTypeSymbol.Void     = New TVoidTypeSymbol
TTypeSymbol.Null_    = New TNullTypeSymbol
Type TTypeSymbol Extends TNamedSymbol Abstract
	Global Byte_:TStructTypeSymbol
	Global Short_:TStructTypeSymbol
	Global Int_:TStructTypeSymbol
	Global UInt_:TStructTypeSymbol
	Global Long_:TStructTypeSymbol
	Global ULong_:TStructTypeSymbol
	Global Float_:TStructTypeSymbol
	Global Double_:TStructTypeSymbol
	Global Object_:TClassTypeSymbol
	Global String_:TClassTypeSymbol
	Global Void:TVoidTypeSymbol
	Global Null_:TNullTypeSymbol
	'Global Unknown:TType
	
	Private
	Field ptrType:TPtrTypeSymbol = Null
	Field varType:TVarTypeSymbol = Null
	Field arrayTypes:TArrayTypeSymbol[] = Null ' index = dimension count - 1
	'Field callableTypes:TCallableType[][] = Null ' callable types with this type as the return type and no parameter names; 1st index = parameter count
	'Field genericTypes:TType[][] = Null ' all the fully constructed types from this generic type; 1st index = type parameter count
	
	Public
	Method ToPtr:TPtrTypeSymbol()
		If Not ptrType Then ptrType = New TPtrTypeSymbol(Self)
		Return ptrType
	End Method
	
	Method ToVar:TVarTypeSymbol()
		If Not varType Then varType = New TVarTypeSymbol(Self)
		Return varType
	End Method
	
	Method ToArray:TArrayTypeSymbol(dimensions:Int)
		If Not arrayTypes[dimensions - 1] Then arrayTypes[dimensions - 1] = New TArrayTypeSymbol(Self, dimensions)
		Return arrayTypes[dimensions - 1]
	End Method
	
	'Method ToString:String() Override Abstract
	
	' TODO: ToCode(withColon, scope)
End Type



Type TValueTypeSymbol Extends TTypeSymbol Abstract
End Type

Type TReferenceTypeSymbol Extends TTypeSymbol Abstract
End Type



Type TStructTypeSymbol Extends TValueTypeSymbol Final
	Field ReadOnly name:String
	
	Method New(name:String)
		Self.name = name
	End Method
	
	Method ToString:String() Override
		Return "Struct " + name
	End Method
End Type

Type TClassTypeSymbol Extends TReferenceTypeSymbol Final
	Field ReadOnly superClass:TClassTypeSymbol
	Field ReadOnly superInterfaces:TInterfaceTypeSymbol[]
	
	Method New(name:String, superClass:TClassTypeSymbol, superInterfaces:TInterfaceTypeSymbol[])
		Self.name = name
		Self.superClass = superClass
		Self.superInterfaces = superInterfaces
	End Method

	Method ToString:String() Override
		Return "Class " + name
	End Method
End Type

Type TInterfaceTypeSymbol Extends TReferenceTypeSymbol Final
	Field ReadOnly superInterfaces:TInterfaceTypeSymbol[]
	
	Method New(name:String, superInterfaces:TInterfaceTypeSymbol[])
		Self.name = name
		Self.superInterfaces = superInterfaces
	End Method
	
	Method ToString:String() Override
		Return "Interface " + name
	End Method
End Type

Type TEnumTypeSymbol Extends TValueTypeSymbol Final
	Field ReadOnly baseType:TStructTypeSymbol
	Field ReadOnly isFlags:Int
	
	Method New(name:String, baseType:TStructTypeSymbol, isFlags:Int)
		Self.name = name
		Self.baseType = baseType
		Self.isFlags = isFlags
	End Method

	Method ToString:String() Override
		Return "Enum " + name
	End Method
End Type



Type TPtrTypeSymbol Extends TValueTypeSymbol Final
	Field ReadOnly referentType:TTypeSymbol
	
	Method New(referentType:TTypeSymbol)
		Self.referentType = referentType
	End Method

	Method ToString:String() Override
		Return referentType.ToString() + " Ptr"
	End Method
End Type

Type TVarTypeSymbol Extends TValueTypeSymbol Final
	Field ReadOnly referentType:TTypeSymbol
	
	Method New(referentType:TTypeSymbol)
		Self.referentType = referentType
	End Method

	Method ToString:String() Override
		Return referentType.ToString() + " Var"
	End Method
End Type

Type TArrayTypeSymbol Extends TReferenceTypeSymbol Final
	Field ReadOnly elementType:TTypeSymbol
	Field ReadOnly dimensions:Int
	
	Method New(elementType:TTypeSymbol, dimensions:Int)
		Assert dimensions >= 1 Else "Array type must have positive number of dimensions"
		Self.elementType = elementType
		Self.dimensions = dimensions
	End Method
	
	Method ToString:String() Override
		Return elementType.ToString() + "[" + ",".Join(New String[dimensions]) + "]"
	End Method
End Type



Type TVoidTypeSymbol Extends TTypeSymbol Final
	Method New()
	End Method
	
	Method ToString:String() Override
		Return "Void"
	End Method
End Type

Type TNullTypeSymbol Extends TTypeSymbol Final
	Method New()
	End Method
	
	Method ToString:String() Override
		Return "Null"
	End Method
End Type

