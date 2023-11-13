SuperStrict
Import "../syntax/Syntax.bmx"


Interface ISymbol
	Method Syntax:ISyntax() ' null for imported or generated symbols
	' TODO: this will not work long-term - red nodes have to be discarded and rebuilt after every
	'       syntax change - symbols should be able to survive changes that don't affect them and
	'       should not keep the entire syntax tree alive, so a syntax reference type is needed
End Interface


Interface IDeclarationSymbol Extends ISymbol
	' TODO: visibility
	Method KindName:String() ' TODO: this should be a function (not possible due to bcc bug)
	Method Name:String() ' does not need to be unique, should not be null if IsReferrableByName is true
	Method IsReferrableByName:Int() ' whether the name can be used in code (as an identifier) to refer to this declaration
End Interface

Interface IDeclarationWithMembersSymbol Extends IDeclarationSymbol End Interface

Interface ITypeDeclarationSymbol Extends IDeclarationWithMembersSymbol ' corresponds to any name that represents a type
	Method KindName:String() Override ' bugfix; doesn't inherit properly for some reason otherwise
	Method Syntax:ISyntax() Override
	Method Name:String() Override ' bugfix; doesn't inherit properly for some reason otherwise
	Method NameWithTypeParameters:String()
	Method SuperTypes:ITypeSymbol[]()
	Method Arity:Int()
	Method TypeParameters:ITypeParameterDeclarationSymbol[]()
	Method BodySyntax:ISyntax()
End Interface
Interface IVariableDeclarationSymbol Extends IDeclarationSymbol End Interface ' any name that represents a value, including constants

Interface ICallableDeclarationSymbol Extends IVariableDeclarationSymbol Method Syntax:TCallableDeclarationSyntax() Override End Interface
Interface IOverloadableDeclarationSymbol Extends IDeclarationSymbol End Interface

Interface IConstDeclarationSymbol Extends IVariableDeclarationSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
Interface IGlobalDeclarationSymbol Extends IVariableDeclarationSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
Interface IFieldDeclarationSymbol Extends IVariableDeclarationSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
Interface ILocalDeclarationSymbol Extends IVariableDeclarationSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface

Interface IFunctionDeclarationSymbol Extends ICallableDeclarationSymbol, IOverloadableDeclarationSymbol End Interface
Interface IMethodDeclarationSymbol Extends ICallableDeclarationSymbol, IOverloadableDeclarationSymbol End Interface
Interface IConstructorDeclarationSymbol Extends ICallableDeclarationSymbol, IOverloadableDeclarationSymbol End Interface
Interface IFinalizerDeclarationSymbol Extends ICallableDeclarationSymbol End Interface
'Interface IClosureDeclarationSymbol Extends ICallableDeclarationSymbol End Interface

Interface IClassDeclarationSymbol Extends ITypeDeclarationSymbol
	'Method Syntax:TClassDeclarationSyntax() Override
	Method SuperClass:IClassSymbol()
	Method SuperInterfaces:IInterfaceSymbol[]()
End Interface

Interface IStructDeclarationSymbol Extends ITypeDeclarationSymbol
	'Method Syntax:TStructDeclarationSyntax() Override
	Method SuperInterfaces:IInterfaceSymbol[]()
End Interface

Interface IInterfaceDeclarationSymbol Extends ITypeDeclarationSymbol
	'Method Syntax:TInterfaceDeclarationSyntax() Override
	Method SuperInterfaces:IInterfaceSymbol[]()
End Interface

Interface IEnumDeclarationSymbol Extends ITypeDeclarationSymbol
	'Method Syntax:TEnumDeclarationSyntax() Override
	Method BaseType:IStructSymbol()
End Interface

Interface ITypeParameterDeclarationSymbol Extends ITypeDeclarationSymbol
End Interface

'Interface IAliasDeclarationSymbol Extends ITypeDeclarationSymbol End Interface

Interface IErrorTypeDeclarationSymbol Extends IClassDeclarationSymbol, IStructDeclarationSymbol, IInterfaceDeclarationSymbol, IEnumDeclarationSymbol, ITypeParameterDeclarationSymbol
End Interface



Interface ITypeSymbol Extends ISymbol
	Method Declaration:ITypeDeclarationSymbol()
End Interface
'Interface IVariableSymbol Extends ISymbol End Interface

'Interface ICallableSymbol Extends IVariableSymbol Method Syntax:TCallableSyntax() Override End Interface

'Interface IConstSymbol Extends IVariableSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
'Interface IGlobalSymbol Extends IVariableSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
'Interface IFieldSymbol Extends IVariableSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface
'Interface ILocalSymbol Extends IVariableSymbol Method Syntax:TVariableDeclaratorSyntax() Override End Interface

'Interface IFunctionSymbol Extends ICallableSymbol, IOverloadableSymbol End Interface
'Interface IMethodSymbol Extends ICallableSymbol, IOverloadableSymbol End Interface
'Interface IConstructorSymbol Extends ICallableSymbol, IOverloadableSymbol End Interface
'Interface IFinalizerSymbol Extends ICallableSymbol End Interface
'Interface IClosureSymbol Extends ICallableSymbol, IOverloadableSymbol End Interface

Interface IClassSymbol Extends ITypeSymbol Method Declaration:IClassDeclarationSymbol() Override End Interface
Interface IStructSymbol Extends ITypeSymbol Method Declaration:IStructDeclarationSymbol() Override End Interface
Interface IInterfaceSymbol Extends ITypeSymbol Method Declaration:IInterfaceDeclarationSymbol() Override End Interface
Interface IEnumSymbol Extends ITypeSymbol Method Declaration:IEnumDeclarationSymbol() Override End Interface
Interface ITypeParameterSymbol Extends ITypeSymbol Method Declaration:ITypeParameterDeclarationSymbol() Override End Interface
Interface IErrorTypeSymbol Extends IClassSymbol, IStructSymbol, IInterfaceSymbol, IEnumSymbol, ITypeParameterSymbol Method Declaration:IErrorTypeDeclarationSymbol() Override End Interface
