SuperStrict
Import "../syntax/Syntax.bmx"

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


' TODO: distinguish between declaration and use
Interface ISymbol
	Method GetSyntax:ISyntax() ' returns nulls for imported symbols
	'Method ToString:String() Override
End Interface


Rem
Type TSymbol Implements ISymbol Abstract
	Private
	Method New() End Method
	
	Protected
	Method New(syntax:ISyntax)
		Self.syntax = syntax
	End Method
End Type
End Rem


Interface IDeclaration Extends ISymbol
	' TODO: visibility
	Method GetName:String() ' does not need to be unique, can but should not be null
	Method IsReferrableByName:Int() ' whether the name can be used in code (as an identifier) to refer to this declaration
End Interface



Interface IOverloadableDeclaration Extends IDeclaration
End Interface



Type TVariableDeclaration Implements IDeclaration Abstract
	Private
	Field ReadOnly name:String
	Field ReadOnly syntax:TVariableDeclaratorSyntax
	
	Protected
	Method New() End Method
	
	Public
	Method New(syntax:TVariableDeclaratorSyntax)
		Self.name = syntax.name.identifier.lexerToken.value 
		Self.syntax = syntax
	End Method
	
	Method New(name:String) ' for imported symbols
		Self.name = name
		Self.syntax = Null
	End Method
	
	Method GetSyntax:TVariableDeclaratorSyntax() Override
		Return syntax
	End Method
	
	Method GetName:String() Override
		Return name
	End Method
	
	Method IsReferrableByName:Int() Override
		Return True
	End Method
End Type



Type TConstDeclaration Extends TVariableDeclaration Final
	Method ToString:String() Override
		Return "Const " + GetName()
	End Method
End Type



Type TGlobalDeclaration Extends TVariableDeclaration Final	
	Method ToString:String() Override
		Return "Global " + GetName()
	End Method
End Type



Type TLocalDeclaration Extends TVariableDeclaration Final
	Method ToString:String() Override
		Return "Local " + GetName()
	End Method
End Type



Type TFieldDeclaration Extends TVariableDeclaration Final
	Method ToString:String() Override
		Return "Field " + GetName()
	End Method
End Type



Type TCallableDeclaration Implements IDeclaration Abstract
	Private
	Field ReadOnly name:String
	Field ReadOnly syntax:TCallableDeclarationSyntax
	
	Protected
	Method New() End Method
	
	Public
	Method New(syntax:TCallableDeclarationSyntax)
		If syntax.name.identifierName Then
			Self.name = syntax.name.identifierName.identifier.lexerToken.value
		Else If syntax.name.keywordName Then
		DebugStop
			Self.name = syntax.name.keywordName.lexerToken.value
		Else If syntax.name.operatorName Then
			For Local t:TSyntaxToken = EachIn syntax.name.operatorName.tokens
				Self.name :+ t.lexerToken.value
			Next
		Else
			RuntimeError "Missing case"
		End If
		Self.syntax = syntax
	End Method
	
	Method New(name:String) ' for imported symbols
		Self.name = name
		Self.syntax = Null
	End Method
	
	Method GetSyntax:TCallableDeclarationSyntax() Override
		Return syntax
	End Method
	
	Method GetName:String() Override
		Return name
	End Method
End Type



Type TFunctionDeclaration Extends TCallableDeclaration Implements IOverloadableDeclaration Final
	Method ToString:String() Override
		Return "Function " + GetName()
	End Method
	
	Method IsReferrableByName:Int() Override
		Return True
	End Method
End Type



Type TMethodDeclaration Extends TCallableDeclaration Implements IOverloadableDeclaration Final
	Method ToString:String() Override
		Return "Method " + GetName()
	End Method
	
	Method IsReferrableByName:Int() Override
		Return True
	End Method
End Type



Type TConstructorDeclaration Extends TCallableDeclaration Implements IOverloadableDeclaration Final
	Method ToString:String() Override
		Return GetName()
	End Method
	
	Method IsReferrableByName:Int() Override
		Return False
	End Method
End Type



Type TFinalizerDeclaration Extends TCallableDeclaration Final
	Method ToString:String() Override
		Return GetName()
	End Method
	
	Method IsReferrableByName:Int() Override
		Return False
	End Method
End Type



TTypeDeclaration.Byte_    = New TStructDeclaration("Byte")
TTypeDeclaration.Short_   = New TStructDeclaration("Short")
TTypeDeclaration.Int_     = New TStructDeclaration("Int")
TTypeDeclaration.UInt_    = New TStructDeclaration("UInt")
TTypeDeclaration.Long_    = New TStructDeclaration("Long")
TTypeDeclaration.ULong_   = New TStructDeclaration("ULong")
TTypeDeclaration.Float_   = New TStructDeclaration("Float")
TTypeDeclaration.Double_  = New TStructDeclaration("Double")
TTypeDeclaration.Object_  = New TClassDeclaration("Object", Null, Null)
TTypeDeclaration.String_  = New TClassDeclaration("String", Null, Null) ' TODO 'TTypeSymbol.Object_, Null)
'TTypeDeclaration.Void     = New TVoidTypeDeclaration
'TTypeDeclaration.Null_    = New TNullTypeDeclaration
Type TTypeDeclaration Implements IDeclaration Abstract
	Global Byte_:TStructDeclaration
	Global Short_:TStructDeclaration
	Global Int_:TStructDeclaration
	Global UInt_:TStructDeclaration
	Global Long_:TStructDeclaration
	Global ULong_:TStructDeclaration
	Global Float_:TStructDeclaration
	Global Double_:TStructDeclaration
	Global Object_:TClassDeclaration
	Global String_:TClassDeclaration
	'Global Void:TVoidTypeDeclaration
	'Global Null_:TNullTypeDeclaration
	'Global Unknown:TType
	
	Private
	'Field ptrType:TPtrTypeSymbol = Null
	'Field varType:TVarTypeSymbol = Null
	'Field arrayTypes:TArrayTypeSymbol[] = Null ' index = dimension count - 1
	'Field callableTypes:TCallableType[][] = Null ' callable types with this type as the return type and no parameter names; 1st index = parameter count
	'Field genericTypes:TType[][] = Null ' all the fully constructed types from this generic type; 1st index = type parameter count
	
	Rem
	Protected
	Method New() End Method
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
	End Rem
	' TODO: ToCode(withColon, scope)
	
	
	Private
	Field ReadOnly name:String
	'Field ReadOnly syntax:TTypeDeclarationSyntax
	
	Protected
	Method New() End Method
	
	Public	
	Method GetSyntax:TTypeDeclarationSyntax() Override Abstract
	
	Method GetName:String() Override
		Return name
	End Method

	Method IsReferrableByName:Int() Override
		Return True
	End Method
End Type



Type TValueTypeDeclaration Extends TTypeDeclaration Abstract
End Type



Type TReferenceTypeDeclaration Extends TTypeDeclaration Abstract
End Type



Type TStructDeclaration Extends TValueTypeDeclaration Final
	Private
	Field ReadOnly syntax:TStructDeclarationSyntax
		
	Method New() End Method
	
	Public
	Method New(syntax:TStructDeclarationSyntax)
		Self.name = syntax.name.identifier.lexerToken.value
		Self.syntax = syntax
	End Method
	
	Method New(name:String) ' for imported symbols
		Self.name = name
		Self.syntax = Null
	End Method
	
	Method GetSyntax:TStructDeclarationSyntax() Override
		Return syntax
	End Method
	
	Method ToString:String() Override
		Return "Struct " + name
	End Method
End Type



Type TClassDeclaration Extends TReferenceTypeDeclaration Final
	Private
	Field ReadOnly syntax:TClassDeclarationSyntax
	Field ReadOnly superClass:TClass
	Field ReadOnly superInterfaces:TInterface[]
		
	Method New() End Method
	
	Public
	Method New(syntax:TClassDeclarationSyntax, superClass:TClass, superInterfaces:TInterface[])
		Self.name = syntax.name.identifier.lexerToken.value
		Self.syntax = syntax
		Self.superClass = superClass
		Self.superInterfaces = superInterfaces
	End Method
	
	Method New(name:String, superClass:TClass, superInterfaces:TInterface[]) ' for imported symbols
		Self.name = name
		Self.syntax = Null
		Self.superClass = superClass
		Self.superInterfaces = superInterfaces
	End Method
	
	Method GetSyntax:TClassDeclarationSyntax() Override
		Return syntax
	End Method
	
	Method ToString:String() Override
		Return "Class " + name
	End Method
End Type



Type TInterfaceDeclaration Extends TReferenceTypeDeclaration Final
	Private
	Field ReadOnly syntax:TInterfaceDeclarationSyntax
	Field ReadOnly superInterfaces:TInterface[]
		
	Method New() End Method
	
	Public
	Method New(syntax:TInterfaceDeclarationSyntax, superInterfaces:TInterface[])
		Self.name = syntax.name.identifier.lexerToken.value
		Self.syntax = syntax
		Self.superInterfaces = superInterfaces
	End Method
	
	Method New(name:String, superInterfaces:TInterface[]) ' for imported symbols
		Self.name = name
		Self.syntax = Null
		Self.superInterfaces = superInterfaces
	End Method
	
	Method GetSyntax:TInterfaceDeclarationSyntax() Override
		Return syntax
	End Method
	
	Method ToString:String() Override
		Return "Interface " + name
	End Method
End Type



Type TEnumDeclaration Extends TValueTypeDeclaration Final
	Private
	Field ReadOnly syntax:TEnumDeclarationSyntax
	Field ReadOnly baseType:TStruct
	
	Method New() End Method
	
	Public
	Method New(syntax:TEnumDeclarationSyntax, baseType:TStruct, isFlags:Int)
		Self.name = syntax.name.identifier.lexerToken.value
		Self.syntax = syntax
		Self.baseType = baseType
	End Method
	
	Method New(name:String, baseType:TStruct, isFlags:Int) ' for imported symbols
		Self.name = name
		Self.syntax = Null
		Self.baseType = baseType
	End Method
	
	Method GetSyntax:TEnumDeclarationSyntax() Override
		Return syntax
	End Method
	
	Method ToString:String() Override
		Return "Enum " + name
	End Method
End Type



Type TType Implements ISymbol Abstract
End Type



Type TValueType Extends TType Abstract
End Type



Type TReferenceType Extends TType Abstract
End Type



Type TStruct Extends TValueType Final
	Private
	Field ReadOnly syntax:TTypeBaseSyntax
	Field ReadOnly declaration:TStructDeclaration
	
	Public
	Method New(syntax:TTypeBaseSyntax, declaration:TStructDeclaration)
		Self.syntax = syntax
		Self.declaration = declaration
	End Method
	
	Method GetSyntax:TTypeBaseSyntax() Override
		Return syntax
	End Method
	
	Method GetDeclaration:TStructDeclaration()
		Return declaration
	End Method
	
	Method ToString:String() Override
		Return declaration.GetName()
	End Method
End Type



Type TClass Extends TReferenceType Final
	Private
	Field ReadOnly syntax:TTypeBaseSyntax
	Field ReadOnly declaration:TClassDeclaration
	
	Public
	Method New(syntax:TTypeBaseSyntax, declaration:TClassDeclaration)
		Self.syntax = syntax
		Self.declaration = declaration
	End Method
	
	Method GetSyntax:TTypeBaseSyntax() Override
		Return syntax
	End Method
	
	Method GetDeclaration:TClassDeclaration()
		Return declaration
	End Method
	
	Method ToString:String() Override
		Return declaration.GetName()
	End Method
End Type



Type TInterface Extends TReferenceType Final
	Private
	Field ReadOnly syntax:TTypeBaseSyntax
	Field ReadOnly declaration:TInterfaceDeclaration
	
	Public
	Method New(syntax:TTypeBaseSyntax, declaration:TInterfaceDeclaration)
		Self.syntax = syntax
		Self.declaration = declaration
	End Method
	
	Method GetSyntax:TTypeBaseSyntax() Override
		Return syntax
	End Method
	
	Method GetDeclaration:TInterfaceDeclaration()
		Return declaration
	End Method
	
	Method ToString:String() Override
		Return declaration.GetName()
	End Method
End Type



Type TEnum Extends TValueType Final
	Private
	Field ReadOnly syntax:TTypeBaseSyntax
	Field ReadOnly declaration:TEnumDeclaration
	
	Public
	Method New(syntax:TTypeBaseSyntax, declaration:TEnumDeclaration)
		Self.syntax = syntax
		Self.declaration = declaration
	End Method
	
	Method GetSyntax:TTypeBaseSyntax() Override
		Return syntax
	End Method
	
	Method GetDeclaration:TEnumDeclaration()
		Return declaration
	End Method
	
	Method ToString:String() Override
		Return declaration.GetName()
	End Method
End Type



Type TPtrType Extends TValueType Final
	Field ReadOnly referentType:TType
	
	Method New(referentType:TType)
		Self.referentType = referentType
	End Method

	Method ToString:String() Override
		Return referentType.ToString() + " Ptr"
	End Method
End Type

Type TVarType Extends TValueType Final
	Field ReadOnly referentType:TType
	
	Method New(referentType:TType)
		Self.referentType = referentType
	End Method

	Method ToString:String() Override
		Return referentType.ToString() + " Var"
	End Method
End Type

Type TArrayType Extends TReferenceType Final
	Field ReadOnly elementType:TType
	Field ReadOnly dimensions:Int
	
	Method New(elementType:TType, dimensions:Int)
		Assert dimensions >= 1 Else "Array type must have positive number of dimensions"
		Self.elementType = elementType
		Self.dimensions = dimensions
	End Method
	
	Method ToString:String() Override
		Return elementType.ToString() + "[" + ",".Join(New String[dimensions]) + "]"
	End Method
End Type



Type TVoidType Extends TType Final
	Method New()
	End Method
	
	Method GetSyntax:ISyntax() Override
		Return Null
	End Method
	
	Method ToString:String() Override
		Return "Void"
	End Method
End Type



Type TNullType Extends TType Final
	Method New()
	End Method
	
	Method GetSyntax:ISyntax() Override
		Return Null
	End Method
	
	Method ToString:String() Override
		Return "Null"
	End Method
End Type

