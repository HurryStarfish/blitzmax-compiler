SuperStrict
Import "Symbols.bmx"



' type declarations have references to each other; creating a declaration would normally require
' the declarations for all its dependencies (such as super types) to be created first
' however when type declarations are nested, declarations need to be present in scopes for the
' required name lookups to work correctly; this can create cyclic dependencies where creating a
' declaration would require looking up their super type declarations first, but said lookups
' would require the super types to already be known
' to solve this, type declarations are first created in an unfinished state and then fully
' initialized using builder types; ones a builder is no longer used, the corresponding
' declaration becomes effectively immutable



'Private ' TODO: does not work due to bcc bug
Type TTypeDeclarationSymbol Implements ITypeDeclarationSymbol Abstract
	Field _name:String
	Field _isReferrableByName:Int
	
	Method Name:String() Override Return _name End Method
	
	Method IsReferrableByName:Int() Override Return _isReferrableByName End Method
	
	Method NameWithTypeParameters:String() Override
		Local str:String
		For Local typeParameter:ITypeParameterDeclarationSymbol = EachIn TypeParameters()
			If str Then str :+ ", "
			str :+ typeParameter.Name()
		Next
		If str Then Return Name() + "<" + str + ">" Else Return Name()
	End Method
	
	Method Arity:Int() Override Return TypeParameters().length End Method
End Type

Public
Type TTypeDeclarationSymbolBuilder Abstract
	Method Declaration:ITypeDeclarationSymbol() Abstract
End Type



Private
Type TClassDeclarationSymbol Extends TTypeDeclarationSymbol Implements IClassDeclarationSymbol Final
	Field ReadOnly _syntax:TClassDeclarationSyntax
	Field _superClass:IClassSymbol
	Field _superInterfaces:IInterfaceSymbol[]
	Field _typeParameters:ITypeParameterDeclarationSymbol[]
	
	Method New(syntax:TClassDeclarationSyntax)
		Self._syntax = syntax
		Self._name = syntax.Name().Identifier().lexerToken.value
		Self._isReferrableByName = True
	End Method
	
	Method KindName:String() Override Return "Class" End Method
	
	Method Syntax:TClassDeclarationSyntax() Override Return _syntax End Method
	
	Method SuperTypes:ITypeSymbol[]() Override Return ITypeSymbol[]([_superClass] + _superInterfaces) End Method
	
	Method SuperClass:IClassSymbol() Override Return _superClass End Method
	
	Method SuperInterfaces:IInterfaceSymbol[]() Override Return _superInterfaces End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return _typeParameters End Method
	
	Method BodySyntax:ISyntax() Override Return _syntax.Body() End Method
End Type

Public
Type TClassDeclarationSymbolBuilder Extends TTypeDeclarationSymbolBuilder Final
	Private
	Field ReadOnly _declaration:TClassDeclarationSymbol
	
	Public
	Method New(syntax:TClassDeclarationSyntax)
		Self._declaration = New TClassDeclarationSymbol(syntax)
	End Method
	
	Method Declaration:IClassDeclarationSymbol() Override Return _declaration End Method
	
	Method SetSuperClass(superClass:IClassSymbol) _declaration._superClass = superClass End Method
	
	Method SetSuperInterfaces(superInterfaces:IInterfaceSymbol[]) _declaration._superInterfaces = superInterfaces End Method
	Method SetSuperInterface(index:Int, superInterface:IInterfaceSymbol) _declaration._superInterfaces[index] = superInterface End Method
	
	Method SetTypeParameters(typeParameters:ITypeParameterDeclarationSymbol[]) _declaration._typeParameters = typeParameters End Method
End Type



Private
Type TStructDeclarationSymbol Extends TTypeDeclarationSymbol Implements IStructDeclarationSymbol Final
	Field ReadOnly _syntax:TStructDeclarationSyntax
	Field _superInterfaces:IInterfaceSymbol[]
	Field _typeParameters:ITypeParameterDeclarationSymbol[]
	
	Method New(syntax:TStructDeclarationSyntax)
		Self._syntax = syntax
		Self._name = syntax.Name().Identifier().lexerToken.value
		Self._isReferrableByName = True
	End Method
	
	Method KindName:String() Override Return "Struct" End Method
	
	Method Syntax:TStructDeclarationSyntax() Override Return _syntax End Method
	
	Method SuperTypes:ITypeSymbol[]() Override Return _superInterfaces End Method
	
	Method SuperInterfaces:IInterfaceSymbol[]() Override Return _superInterfaces End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return _typeParameters End Method

	Method BodySyntax:ISyntax() Override Return _syntax.Body() End Method
End Type

Public
Type TStructDeclarationSymbolBuilder Extends TTypeDeclarationSymbolBuilder Final
	Private
	Field ReadOnly _declaration:TStructDeclarationSymbol
	
	Public
	Method New(syntax:TStructDeclarationSyntax)
		Self._declaration = New TStructDeclarationSymbol(syntax)
	End Method
	
	Method Declaration:IStructDeclarationSymbol() Override Return _declaration End Method
	
	Method SetSuperInterfaces(superInterfaces:IInterfaceSymbol[]) _declaration._superInterfaces = superInterfaces End Method
	Method SetSuperInterface(index:Int, superInterface:IInterfaceSymbol) _declaration._superInterfaces[index] = superInterface End Method
	
	Method SetTypeParameters(typeParameters:ITypeParameterDeclarationSymbol[]) _declaration._typeParameters = typeParameters End Method
End Type



Private
Type TInterfaceDeclarationSymbol Extends TTypeDeclarationSymbol Implements IInterfaceDeclarationSymbol Final
	Field ReadOnly _syntax:TInterfaceDeclarationSyntax
	Field _superInterfaces:IInterfaceSymbol[]
	Field _typeParameters:ITypeParameterDeclarationSymbol[]
	
	Method New(syntax:TInterfaceDeclarationSyntax)
		Self._syntax = syntax
		Self._name = syntax.Name().Identifier().lexerToken.value
		Self._isReferrableByName = True
	End Method
	
	Method KindName:String() Override Return "Interface" End Method
	
	Method Syntax:TInterfaceDeclarationSyntax() Override Return _syntax End Method
	
	Method SuperTypes:ITypeSymbol[]() Override Return _superInterfaces End Method
	
	Method SuperInterfaces:IInterfaceSymbol[]() Override Return _superInterfaces End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return _typeParameters End Method

	Method BodySyntax:ISyntax() Override Return _syntax.Body() End Method
End Type

Public
Type TInterfaceDeclarationSymbolBuilder Extends TTypeDeclarationSymbolBuilder Final
	Private
	Field ReadOnly _declaration:TInterfaceDeclarationSymbol
	
	Public
	Method New(syntax:TInterfaceDeclarationSyntax)
		Self._declaration = New TInterfaceDeclarationSymbol(syntax)
	End Method
	
	Method Declaration:IInterfaceDeclarationSymbol() Override Return _declaration End Method
	
	Method SetSuperInterfaces(superInterfaces:IInterfaceSymbol[]) _declaration._superInterfaces = superInterfaces End Method
	Method SetSuperInterface(index:Int, superInterface:IInterfaceSymbol) _declaration._superInterfaces[index] = superInterface End Method
	
	Method SetTypeParameters(typeParameters:ITypeParameterDeclarationSymbol[]) _declaration._typeParameters = typeParameters End Method
End Type



Private
Type TEnumDeclarationSymbol Extends TTypeDeclarationSymbol Implements IEnumDeclarationSymbol Final
	Field ReadOnly _syntax:TEnumDeclarationSyntax
	Field _baseType:IStructSymbol
	
	Method New(syntax:TEnumDeclarationSyntax)
		Self._syntax = syntax
		Self._name = syntax.Name().Identifier().lexerToken.value
		Self._isReferrableByName = True
	End Method
	
	Method KindName:String() Override Return "Enum" End Method
	
	Method Syntax:TEnumDeclarationSyntax() Override Return _syntax End Method
	
	Method SuperTypes:ITypeSymbol[]() Override Return [] End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return [] End Method
	
	Method BaseType:IStructSymbol() Override Return _baseType End Method
	
	Method BodySyntax:ISyntax() Override Return _syntax End Method
End Type

Public
Type TEnumDeclarationSymbolBuilder Extends TTypeDeclarationSymbolBuilder Final
	Private
	Field ReadOnly _declaration:TEnumDeclarationSymbol
	
	Public
	Method New(syntax:TEnumDeclarationSyntax)
		Self._declaration = New TEnumDeclarationSymbol(syntax)
	End Method
	
	Method Declaration:IEnumDeclarationSymbol() Override Return _declaration End Method
	
	Method SetBaseType(baseType:IStructSymbol) _declaration._baseType = baseType End Method
End Type



Type TTypeParameterDeclarationSymbol Extends TTypeDeclarationSymbol Implements ITypeParameterDeclarationSymbol Final
	Field ReadOnly _syntax:TTypeParameterDeclaratorSyntax
	
	Method New(syntax:TTypeParameterDeclaratorSyntax)
		Self._syntax = syntax
		Self._name = syntax.Name().Identifier().lexerToken.value
		Self._isReferrableByName = True
	End Method
	
	Method KindName:String() Override Return "Type parameter" End Method
	
	Method Syntax:TTypeParameterDeclaratorSyntax() Override Return _syntax End Method
	
	Method SuperTypes:ITypeSymbol[]() Override RuntimeError "TODO" End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return [] End Method
	
	Method BodySyntax:ISyntax() Override RuntimeError "TODO" End Method
End Type



Type TErrorTypeDeclarationSymbol Extends TTypeDeclarationSymbol Implements IErrorTypeDeclarationSymbol
	Global _baseType:TErrorTypeSymbol = New TErrorTypeSymbol(Null)
	
	Method New()
		Self._name = "<error type>"
		Self._isReferrableByName = False
	End Method
	
	Method KindName:String() Override Return "Error type" End Method
	
	Method Syntax:ISyntax() Override Return Null End Method
	
	Method SuperTypes:ITypeSymbol[]() Override Return [] End Method
	
	Method SuperClass:IClassSymbol() Override Return Null End Method
	
	Method SuperInterfaces:IInterfaceSymbol[]() Override Return [] End Method
	
	Method TypeParameters:ITypeParameterDeclarationSymbol[]() Override Return [] End Method
	
	Method BaseType:IStructSymbol() Override Return _baseType End Method
	
	Method BodySyntax:ISyntax() Override Return Null End Method
End Type

Global ErrorTypeDeclarationSymbol:TErrorTypeDeclarationSymbol = New TErrorTypeDeclarationSymbol



Type TClassSymbol Implements IClassSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	Field ReadOnly _declaration:IClassDeclarationSymbol
	
	Method New(syntax:TTypeSyntax, declaration:IClassDeclarationSymbol)
		Self._syntax = syntax
		Self._declaration = declaration
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:IClassDeclarationSymbol() Override Return _declaration End Method
End Type

Type TStructSymbol Implements IStructSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	Field ReadOnly _declaration:IStructDeclarationSymbol
	
	Method New(syntax:TTypeSyntax, declaration:IStructDeclarationSymbol)
		Self._syntax = syntax
		Self._declaration = declaration
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:IStructDeclarationSymbol() Override Return _declaration End Method
End Type

Type TInterfaceSymbol Implements IInterfaceSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	Field ReadOnly _declaration:IInterfaceDeclarationSymbol
	
	Method New(syntax:TTypeSyntax, declaration:IInterfaceDeclarationSymbol)
		Self._syntax = syntax
		Self._declaration = declaration
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:IInterfaceDeclarationSymbol() Override Return _declaration End Method
End Type

Type TEnumSymbol Implements IEnumSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	Field ReadOnly _declaration:IEnumDeclarationSymbol
	
	Method New(syntax:TTypeSyntax, declaration:IEnumDeclarationSymbol)
		Self._syntax = syntax
		Self._declaration = declaration
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:IEnumDeclarationSymbol() Override Return _declaration End Method
End Type

Type TTypeParameterSymbol Implements ITypeParameterSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	Field ReadOnly _declaration:ITypeParameterDeclarationSymbol
	
	Method New(syntax:TTypeSyntax, declaration:ITypeParameterDeclarationSymbol)
		Self._syntax = syntax
		Self._declaration = declaration
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:ITypeParameterDeclarationSymbol() Override Return _declaration End Method
End Type

Type TErrorTypeSymbol Implements IErrorTypeSymbol Final
	Field ReadOnly _syntax:TTypeSyntax
	
	Method New(syntax:TTypeSyntax)
		Self._syntax = syntax
	End Method
	
	Method Syntax:TTypeSyntax() Override Return _syntax End Method
	
	Method Declaration:IErrorTypeDeclarationSymbol() Override Return ErrorTypeDeclarationSymbol End Method
End Type

