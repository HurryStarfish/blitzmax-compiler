SuperStrict
Import "ISyntax.bmx"
Import "SyntaxToken.bmx"
Import "SyntaxUtils.bmx"
? Debug
Import BRL.Reflection
?

' TODO: replace Assert in TExpressionStatementSyntax with marker interface, do the same whereever else applicable
' TODO: verify union-like types or replace with subtyping
' TODO: metadata to validate token types in TSyntaxToken-type fields?

Public

Type TSyntax Implements ISyntax Abstract
	Method CodeRange:SCodeRange() Override Final
		Local children:ISyntaxOrSyntaxToken[] = GetChildren()
		If children.length = 0 Then
			Return Null
		Else
			If children.length = 1 Then
				Return children[0].CodeRange()
			Else
				' return combined range of children
				Local firstValidChildRange:SCodeRange
				Local lastValidChildRange:SCodeRange
				Local c:Int
				For c = 0 To children.length - 1
					Local r:SCodeRange = children[c].CodeRange()
					If r.IsValid() Then firstValidChildRange = r; Exit
				Next
				If c = children.length Then
					Return Null ' no child with a valid range
				Else
					For c = children.length - 1 To c Step -1
						Local r:SCodeRange = children[c].CodeRange()
						If r.IsValid() Then lastValidChildRange = r; Exit
					Next
					Return New SCodeRange(firstValidChildRange.startLocation, lastValidChildRange.endLocation)
				End If
			End If
		End If
	End Method
	
	' TODO: ToString
	
	' TODO: ToCode
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Top-Level ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Type TCompilationUnitSyntax Extends TSyntax Final
	Field ReadOnly header:TCodeHeaderSyntax
	Field ReadOnly body:TCodeBodySyntax
	Field ReadOnly eofToken:TSyntaxToken {minor}
	
	Method New(header:TCodeHeaderSyntax, body:TCodeBodySyntax, eofToken:TSyntaxToken)
		Self.header = header
		Self.body = body
		Self.eofToken = eofToken
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			header, ..
			body, ..
			eofToken ..
		)
	End Method
End Type



Type TCodeHeaderSyntax Extends TSyntax Final
	Field ReadOnly elements:ICodeHeaderElementSyntax[]
	
	Method New(elements:ICodeHeaderElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Interface ICodeHeaderElementSyntax Extends ISyntax
End Interface



Type TCodeBodySyntax Extends TSyntax Final
	Field ReadOnly block:TCodeBlockSyntax
	
	Method New(block:TCodeBlockSyntax)
		Self.block = block
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			block ..
		)
	End Method
End Type



Type TCodeBlockSyntax Extends TSyntax Final
	Field ReadOnly elements:ICodeBlockElementSyntax[]
	
	Method New(elements:ICodeBlockElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Interface ICodeBlockElementSyntax Extends ISyntax
End Interface




' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Header Directives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




Interface IHeaderDirectiveSyntax Extends ICodeHeaderElementSyntax
End Interface



Type TStrictnessDirectiveSyntax Extends TSyntax Implements IHeaderDirectiveSyntax Final
	Field ReadOnly strictness:TSyntaxToken
	
	Method New(strictness:TSyntaxToken)
		Self.strictness = strictness
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			strictness ..
		)
	End Method
End Type



Type TModuleDirectiveSyntax Extends TSyntax Implements IHeaderDirectiveSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly moduleName:TQualifiedNameSyntax
	
	Method New(keyword:TSyntaxToken, moduleName:TQualifiedNameSyntax)
		Self.keyword = keyword
		Self.moduleName = moduleName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			moduleName ..
		)
	End Method
End Type



Type TModuleInfoDirectiveSyntax Extends TSyntax Implements IHeaderDirectiveSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly info:TStringLiteralExpressionSyntax
	
	Method New(keyword:TSyntaxToken, info:TStringLiteralExpressionSyntax)
		Self.keyword = keyword
		Self.info = info
	End Method

	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			info ..
		)
	End Method
End Type



Type TFrameworkDirectiveSyntax Extends TSyntax Implements IHeaderDirectiveSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly moduleName:TQualifiedNameSyntax
	
	Method New(keyword:TSyntaxToken, moduleName:TQualifiedNameSyntax)
		Self.keyword = keyword
		Self.moduleName = moduleName
	End Method

	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			moduleName ..
		)
	End Method
End Type



Type TImportDirectiveSyntax Extends TSyntax Implements IHeaderDirectiveSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly importSource:TImportSourceSyntax
	
	Method New(keyword:TSyntaxToken, importSource:TImportSourceSyntax)
		Self.keyword = keyword
		Self.importSource = importSource
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			importSource ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Include Directive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



'Interface IDirectiveSyntax Extends ICodeBlockElementSyntax, IExternBlockElementSyntax, IEnumMemberSyntax
'End Interface



'Type TIncludeDirectiveSyntax Extends TSyntax Implements IDirectiveSyntax Final
Type TIncludeDirectiveSyntax Extends TSyntax Implements ICodeBlockElementSyntax, IExternBlockElementSyntax, IEnumMemberSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly filePath:TStringLiteralExpressionSyntax
	Field ReadOnly body:TCodeBodySyntax
	Field ReadOnly eofToken:TSyntaxToken {minor}
		
	Method New(keyword:TSyntaxToken, filePath:TStringLiteralExpressionSyntax, body:TCodeBodySyntax, eofToken:TSyntaxToken)
		Self.keyword = keyword
		Self.filePath = filePath
		Self.body = body
		Self.eofToken = eofToken
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			filePath, ..
			body, ..
			eofToken ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Interface IDeclarationSyntax Extends ICodeBlockElementSyntax
End Interface



Type TExternBlockSyntax Extends TSyntax Implements ICodeBlockElementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly callingConvention:TSyntaxToken {nullable}
	Field ReadOnly elements:IExternBlockElementSyntax[]
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}
	
	Method New(initiatorKeyword:TSyntaxToken, callingConvention:TSyntaxToken, elements:IExternBlockElementSyntax[], terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.callingConvention = callingConvention
		Self.elements = elements
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			callingConvention, ..
			elements, ..
			terminatorKeyword ..
		)
	End Method
End Type



Interface IExternBlockElementSyntax Extends ISyntax
End Interface



Type TExternDeclarationSyntax Extends TSyntax Implements IExternBlockElementSyntax, IDeclarationSyntax Abstract
End Type



Type TExternTypeDeclarationSyntax Extends TExternDeclarationSyntax Abstract
End Type



Type TExternClassDeclarationSyntax Extends TExternTypeDeclarationSyntax Final
End Type



Type TExternFunctionDeclarationSyntax Extends TExternDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TCallableDeclarationNameSyntax
	Field ReadOnly type_:TTypeSyntax
	Field ReadOnly externSignatureAssignment:TExternSignatureAssignmentSyntax
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	
	Method New(initiatorKeyword:TSyntaxToken, name:TCallableDeclarationNameSyntax, type_:TTypeSyntax, externSignatureAssignment:TExternSignatureAssignmentSyntax, metaData:TMetaDataSyntax)
		Self.initiatorKeyword = initiatorKeyword
		Self.name = name
		Self.type_ = type_
		Self.externSignatureAssignment = externSignatureAssignment
		Self.metaData = metaData
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			name, ..
			type_, ..
			externSignatureAssignment, ..
			metaData ..
		)
	End Method
End Type



Type TExternVariableDeclarationSyntax Extends TExternDeclarationSyntax Final
End Type



Type TTypeDeclarationSyntax Extends TSyntax Implements IDeclarationSyntax Abstract
End Type



Type TClassDeclarationSyntax Extends TTypeDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TNameSyntax
	Field ReadOnly typeParameters:TTypeParameterListSyntax {nullable}
	Field ReadOnly extendsKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly superClass:TTypeSyntax {nullable}
	Field ReadOnly implementsKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly superInterfaces:TTypeListSyntax {nullable}
	Field ReadOnly modifiers:TTypeModifierSyntax[]
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	Field ReadOnly body:TCodeBlockSyntax ' TODO: this type seems a bit unfitting - only declarations and visibility directives are allowed here; see enums
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor} ' TODO: move stuff to TTypeDeclarationSyntax?
	
	Method New(initiatorKeyword:TSyntaxToken, name:TNameSyntax, typeParameters:TTypeParameterListSyntax, extendsKeyword:TSyntaxToken, superClass:TTypeSyntax, implementsKeyword:TSyntaxToken, superInterfaces:TTypeListSyntax, modifiers:TTypeModifierSyntax[], metaData:TMetaDataSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.name = name
		Self.typeParameters = typeParameters
		Self.extendsKeyword = extendsKeyword
		Self.superClass = superClass
		Self.implementsKeyword = implementsKeyword
		Self.superInterfaces = superInterfaces
		Self.modifiers = modifiers
		Self.metaData = metaData
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			name, ..
			typeParameters, ..
			extendsKeyword, ..
			superClass, ..
			implementsKeyword, ..
			superInterfaces, ..
			modifiers, ..
			metaData, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TStructDeclarationSyntax Extends TTypeDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TNameSyntax
	Field ReadOnly typeParameters:TTypeParameterListSyntax {nullable}
	Field ReadOnly modifiers:TTypeModifierSyntax[]
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	Field ReadOnly body:TCodeBlockSyntax ' TODO: this type seems a bit unfitting - only declarations and visibility directives are allowed here; see enums
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor} ' TODO: move stuff to TTypeDeclarationSyntax?
	
	Method New(initiatorKeyword:TSyntaxToken, name:TNameSyntax, typeParameters:TTypeParameterListSyntax, modifiers:TTypeModifierSyntax[], metaData:TMetaDataSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.name = name
		Self.typeParameters = typeParameters
		Self.modifiers = modifiers
		Self.metaData = metaData
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			name, ..
			typeParameters, ..
			modifiers, ..
			metaData, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TInterfaceDeclarationSyntax Extends TTypeDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TNameSyntax
	Field ReadOnly typeParameters:TTypeParameterListSyntax {nullable}
	Field ReadOnly extendsKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly superInterfaces:TTypeListSyntax {nullable}
	Field ReadOnly modifiers:TTypeModifierSyntax[]
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	Field ReadOnly body:TCodeBlockSyntax ' TODO: this type seems a bit unfitting - only declarations and visibility directives are allowed here; see enums
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor} ' TODO: move stuff to TTypeDeclarationSyntax?
	
	Method New(initiatorKeyword:TSyntaxToken, name:TNameSyntax, typeParameters:TTypeParameterListSyntax, extendsKeyword:TSyntaxToken, superInterfaces:TTypeListSyntax, modifiers:TTypeModifierSyntax[], metaData:TMetaDataSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.name = name
		Self.typeParameters = typeParameters
		Self.extendsKeyword = extendsKeyword
		Self.superInterfaces = superInterfaces
		Self.modifiers = modifiers
		Self.metaData = metaData
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			name, ..
			typeParameters, ..
			extendsKeyword, ..
			superInterfaces, ..
			modifiers, ..
			metaData, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TEnumDeclarationSyntax Extends TTypeDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TNameSyntax
	Field ReadOnly baseType:TTypeSyntax {nullable}
	Field ReadOnly flagsKeyword:TContextualKeywordSyntax {nullable}
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	Field ReadOnly members:IEnumMemberSyntax[]
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}
	
	Method New(initiatorKeyword:TSyntaxToken, name:TNameSyntax, baseType:TTypeSyntax, flagsKeyword:TContextualKeywordSyntax, metaData:TMetaDataSyntax, members:IEnumMemberSyntax[], terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.name = name
		Self.baseType = baseType
		Self.flagsKeyword = flagsKeyword
		Self.metaData = metaData
		Self.members = members
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			name, ..
			baseType, ..
			flagsKeyword, ..
			metaData, ..
			members, ..
			terminatorKeyword ..
		)
	End Method
End Type



Interface IEnumMemberSyntax Extends ISyntax
End Interface



Type TEnumMemberDeclarationSyntax Extends TSyntax Implements IEnumMemberSyntax, IDeclarationSyntax Final
	Field ReadOnly name:TNameSyntax
	Field ReadOnly assignment:TAssignmentSyntax {nullable}
	
	Method New(name:TNameSyntax, assignment:TAssignmentSyntax)
		Self.name = name
		Self.assignment = assignment
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			name, ..
			assignment ..
		)
	End Method
End Type



Type TVisibilityDirectiveSyntax Extends TSyntax Implements ICodeBlockElementSyntax Final
	Field ReadOnly visibility:TSyntaxToken
	
	Method New(visibility:TSyntaxToken)
		Self.visibility = visibility
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			visibility ..
		)
	End Method
End Type



Type TCallableDeclarationSyntax Extends TSyntax Implements IDeclarationSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {nullable}
	Field ReadOnly operatorKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly name:TCallableDeclarationNameSyntax
	Field ReadOnly typeParameters:TTypeParameterListSyntax {nullable}
	Field ReadOnly type_:TTypeSyntax
	Field ReadOnly modifiers:TCallableModifierSyntax[]
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	Field ReadOnly body:TCodeBlockSyntax {nullable}
	Field ReadOnly terminatorKeyword:TSyntaxToken {nullable minor}
	
	Method New(initiatorKeyword:TSyntaxToken, operatorKeyword:TSyntaxToken, name:TCallableDeclarationNameSyntax, typeParameters:TTypeParameterListSyntax, type_:TTypeSyntax, modifiers:TCallableModifierSyntax[], metaData:TMetaDataSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.operatorKeyword = operatorKeyword
		Self.name = name
		Self.typeParameters = typeParameters
		Self.type_ = type_
		Self.modifiers = modifiers
		Self.metaData = metaData
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			operatorKeyword, ..
			name, ..
			typeParameters, ..
			type_, ..
			modifiers, ..
			metaData, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TVariableDeclarationSyntax Extends TSyntax Implements IDeclarationSyntax Final
	Field ReadOnly declarationKeyword:TSyntaxToken {nullable}
	Field ReadOnly modifiers:TVariableModifierSyntax[]
	Field ReadOnly declarators:TVariableDeclaratorListSyntax
	Field ReadOnly metaData:TMetaDataSyntax {nullable}
	
	Method New(declarationKeyword:TSyntaxToken, modifiers:TVariableModifierSyntax[], declarators:TVariableDeclaratorListSyntax, metaData:TMetaDataSyntax)
		Self.declarationKeyword = declarationKeyword
		Self.modifiers = modifiers
		Self.declarators = declarators
		Self.metaData = metaData
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			declarationKeyword, ..
			modifiers, ..
			declarators, ..
			metaData ..
		)
	End Method
End Type



Type TVariableDeclaratorSyntax Extends TSyntax Final
	Field ReadOnly name:TNameSyntax
	Field ReadOnly type_:TTypeSyntax {nullable}
	Field ReadOnly initializer:TAssignmentSyntax {nullable}
	
	Method New(name:TNameSyntax, type_:TTypeSyntax, initializer:TAssignmentSyntax)
		Self.name = name
		Self.type_ = type_
		Self.initializer = initializer
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			name, ..
			type_, ..
			initializer ..
		)
	End Method
End Type



Type TModifierSyntax Extends TSyntax Abstract
	Field ReadOnly token:TSyntaxToken
	
	Method New(token:TSyntaxToken)
		Self.token = token
	End Method
End Type



Type TTypeModifierSyntax Extends TModifierSyntax Final
	Method New(token:TSyntaxToken)
		Super.New(token)
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TVariableModifierSyntax Extends TModifierSyntax Final
	Method New(token:TSyntaxToken)
		Super.New(token)
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TCallableModifierSyntax Extends TModifierSyntax Final
	Method New(token:TSyntaxToken)
		Super.New(token)
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TTypeParameterDeclaratorSyntax Extends TSyntax Final
	Field ReadOnly name:TNameSyntax
	
	Method New(name:TNameSyntax)
		Self.name = name
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			name ..
		)
	End Method
End Type



Type TLabelDeclarationSyntax Extends TSyntax Implements IDeclarationSyntax Final
	Field ReadOnly hash:TSyntaxToken {minor}
	Field ReadOnly name:TNameSyntax
	
	Method New(hash:TSyntaxToken, name:TNameSyntax)
		Self.hash = hash
		Self.name = name
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			hash,
			name ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Statements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Interface IStatementSyntax Extends ICodeBlockElementSyntax
End Interface



Type TIfStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly condition:IExpressionSyntax
	Field ReadOnly branches:TIfBranchSyntax[] ' always contains a Then branch
	Field ReadOnly terminatorKeyword:TSyntaxToken {nullable minor} ' Null for single-line If
	
	Method New(initiatorKeyword:TSyntaxToken, condition:IExpressionSyntax, branches:TIfBranchSyntax[], terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.condition = condition
		Self.branches = branches
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			condition, ..
			branches, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TIfBranchSyntax Extends TSyntax Abstract
End Type



Type TThenIfBranchSyntax Extends TIfBranchSyntax Final
	Field ReadOnly thenKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(thenKeyword:TSyntaxToken, body:TCodeBlockSyntax)
		Self.thenKeyword = thenKeyword
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			thenKeyword, ..
			body ..
		)
	End Method
End Type



Type TElseIfIfBranchSyntax Extends TIfBranchSyntax Final
	Field ReadOnly elseIfKeyword:TSyntaxToken {minor}
	Field ReadOnly condition:IExpressionSyntax
	Field ReadOnly thenKeyword:TSyntaxToken {nullable minor}
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(elseIfKeyword:TSyntaxToken, condition:IExpressionSyntax, thenKeyword:TSyntaxToken, body:TCodeBlockSyntax)
		Self.elseIfKeyword = elseIfKeyword
		Self.condition = condition
		Self.thenKeyword = thenKeyword
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elseIfKeyword, ..
			condition, ..
			thenKeyword, ..
			body ..
		)
	End Method
End Type



Type TElseIfBranchSyntax Extends TIfBranchSyntax Final
	Field ReadOnly elseKeyword:TSyntaxToken {minor}
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(elseKeyword:TSyntaxToken, body:TCodeBlockSyntax)
		Self.elseKeyword = elseKeyword
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elseKeyword, ..
			body ..
		)
	End Method
End Type



Type TSelectStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly expression:IExpressionSyntax
	Field ReadOnly statementSeparators:TStatementSeparatorSyntax[]
	Field ReadOnly branches:TSelectBranchSyntax[]
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}
	
	Method New(initiatorKeyword:TSyntaxToken, expression:IExpressionSyntax, statementSeparators:TStatementSeparatorSyntax[], branches:TSelectBranchSyntax[], terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.expression = expression
		Self.statementSeparators = statementSeparators
		Self.branches = branches
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			expression, ..
			statementSeparators, ..
			branches, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TSelectBranchSyntax Extends TSyntax Abstract
End Type



Type TCaseSelectBranchSyntax Extends TSelectBranchSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expressionList:TExpressionListSyntax
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(keyword:TSyntaxToken, expressionList:TExpressionListSyntax, body:TCodeBlockSyntax)
		Self.keyword = keyword
		Self.expressionList = expressionList
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expressionList, ..
			body ..
		)
	End Method
End Type



Type TDefaultSelectBranchSyntax Extends TSelectBranchSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(keyword:TSyntaxToken, body:TCodeBlockSyntax)
		Self.keyword = keyword
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			body ..
		)
	End Method
End Type



Type TForStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly counter:TForCounterSyntax
	Field ReadOnly eq:TSyntaxToken {minor}
	Field ReadOnly valueSequence:TForValueSequenceSyntax
	Field ReadOnly body:TCodeBlockSyntax
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}
	
	Method New(initiatorKeyword:TSyntaxToken, counter:TForCounterSyntax, eq:TSyntaxToken, valueSequence:TForValueSequenceSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.counter = counter
		Self.eq = eq
		Self.valueSequence = valueSequence
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			counter, ..
			eq, ..
			valueSequence, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TForCounterSyntax Extends TSyntax Abstract
End Type



Type TForCounterDeclarationSyntax Extends TForCounterSyntax Final
	Field ReadOnly declaration:TVariableDeclarationSyntax
	
	Method New(declaration:TVariableDeclarationSyntax)
		Self.declaration = declaration
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			declaration ..
		)
	End Method
End Type



Type TForCounterExpressionSyntax Extends TForCounterSyntax Final
	Field ReadOnly expression:IExpressionSyntax
	
	Method New(expression:IExpressionSyntax)
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression ..
		)
	End Method
End Type



Type TForValueSequenceSyntax Extends TSyntax Abstract
End Type



Type TForToValueSequenceSyntax Extends TForValueSequenceSyntax Final
	Field ReadOnly startExpression:IExpressionSyntax
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly endExpression:IExpressionSyntax
	
	Method New(startExpression:IExpressionSyntax, keyword:TSyntaxToken, endExpression:IExpressionSyntax)
		Self.startExpression = startExpression
		Self.keyword = keyword
		Self.endExpression = endExpression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			startExpression, ..
			keyword, ..
			endExpression ..
		)
	End Method
End Type



Type TForUntilValueSequenceSyntax Extends TForValueSequenceSyntax Final
	Field ReadOnly startExpression:IExpressionSyntax
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly endExpression:IExpressionSyntax
	
	Method New(startExpression:IExpressionSyntax, keyword:TSyntaxToken, endExpression:IExpressionSyntax)
		Self.startExpression = startExpression
		Self.keyword = keyword
		Self.endExpression = endExpression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			startExpression, ..
			keyword, ..
			endExpression ..
		)
	End Method
End Type



Type TForEachInValueSequenceSyntax Extends TForValueSequenceSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly iterableExpression:IExpressionSyntax
	
	Method New(keyword:TSyntaxToken, iterableExpression:IExpressionSyntax)
		Self.keyword = keyword
		Self.iterableExpression = iterableExpression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			iterableExpression ..
		)
	End Method
End Type



Type TWhileStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly condition:IExpressionSyntax
	Field ReadOnly body:TCodeBlockSyntax
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}
		
	Method New(initiatorKeyword:TSyntaxToken, condition:IExpressionSyntax, body:TCodeBlockSyntax, terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.condition = condition
		Self.body = body
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			condition, ..
			body, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TRepeatStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly body:TCodeBlockSyntax
	Field ReadOnly terminator:TRepeatTerminatorSyntax
		
	Method New(initiatorKeyword:TSyntaxToken, body:TCodeBlockSyntax, terminator:TRepeatTerminatorSyntax)
		Self.initiatorKeyword = initiatorKeyword
		Self.body = body
		Self.terminator = terminator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			body, ..
			terminator ..
		)
	End Method
End Type



Type TRepeatTerminatorSyntax Extends TSyntax Abstract
End Type



Type TRepeatUntilTerminatorSyntax Extends TRepeatTerminatorSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly condition:IExpressionSyntax
	
	Method New(keyword:TSyntaxToken, condition:IExpressionSyntax)
		Self.keyword = keyword
		Self.condition = condition
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			condition ..
		)
	End Method
End Type



Type TRepeatForeverTerminatorSyntax Extends TRepeatTerminatorSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	
	Method New(keyword:TSyntaxToken)
		Self.keyword = keyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword ..
		)
	End Method
End Type



Type TExitStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly labelName:TNameSyntax {nullable}
	
	Method New(keyword:TSyntaxToken, labelName:TNameSyntax)
		Self.keyword = keyword
		Self.labelName = labelName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			labelName ..
		)
	End Method
End Type



Type TContinueStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly labelName:TNameSyntax {nullable}
	
	Method New(keyword:TSyntaxToken, labelName:TNameSyntax)
		Self.keyword = keyword
		Self.labelName = labelName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			labelName ..
		)
	End Method
End Type



Type TGotoStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly labelName:TNameSyntax
	
	Method New(keyword:TSyntaxToken, labelName:TNameSyntax)
		Self.keyword = keyword
		Self.labelName = labelName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			labelName ..
		)
	End Method
End Type



Type TReturnStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expression:IExpressionSyntax {nullable}
	
	Method New(keyword:TSyntaxToken, expression:IExpressionSyntax)
		Self.keyword = keyword
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expression ..
		)
	End Method
End Type



Type TTryStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly initiatorKeyword:TSyntaxToken {minor}
	Field ReadOnly body:TCodeBlockSyntax
	Field ReadOnly branches:TTryBranchSyntax[]
	Field ReadOnly terminatorKeyword:TSyntaxToken {minor}

	Method New(initiatorKeyword:TSyntaxToken, body:TCodeBlockSyntax, branches:TTryBranchSyntax[], terminatorKeyword:TSyntaxToken)
		Self.initiatorKeyword = initiatorKeyword
		Self.body = body
		Self.branches = branches
		Self.terminatorKeyword = terminatorKeyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			initiatorKeyword, ..
			body, ..
			branches, ..
			terminatorKeyword ..
		)
	End Method
End Type



Type TTryBranchSyntax Extends TSyntax Abstract
End Type



Type TCatchTryBranchSyntax Extends TTryBranchSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly declaration:TVariableDeclarationSyntax
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(keyword:TSyntaxToken, declaration:TVariableDeclarationSyntax, body:TCodeBlockSyntax)
		Self.keyword = keyword
		Self.declaration = declaration
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			declaration, ..
			body ..
		)
	End Method
End Type



Type TFinallyTryBranchSyntax Extends TTryBranchSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly body:TCodeBlockSyntax
	
	Method New(keyword:TSyntaxToken, body:TCodeBlockSyntax)
		Self.keyword = keyword
		Self.body = body
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			body ..
		)
	End Method
End Type



Type TThrowStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expression:IExpressionSyntax
	
	Method New(keyword:TSyntaxToken, expression:IExpressionSyntax)
		Self.keyword = keyword
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expression ..
		)
	End Method
End Type



Type TAssertStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expression:IExpressionSyntax
	Field ReadOnly commaOrElse:TSyntaxToken {minor}
	Field ReadOnly message:TStringLiteralExpressionSyntax
	
	Method New(keyword:TSyntaxToken, expression:IExpressionSyntax, commaOrElse:TSyntaxToken, message:TStringLiteralExpressionSyntax)
		Self.keyword = keyword
		Self.expression = expression
		Self.commaOrElse = commaOrElse
		Self.message = message
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expression, ..
			commaOrElse, ..
			message ..
		)
	End Method
End Type



Type TEndStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	
	Method New(keyword:TSyntaxToken)
		Self.keyword = keyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword ..
		)
	End Method
End Type



Type TNativeCodeStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly token:TSyntaxToken
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TDefDataStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expressionList:TExpressionListSyntax
	
	Method New(keyword:TSyntaxToken, expressionList:TExpressionListSyntax)
		Self.keyword = keyword
		Self.expressionList = expressionList
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expressionList ..
		)
	End Method
End Type



Type TReadDataStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly expressionList:TExpressionListSyntax
	
	Method New(keyword:TSyntaxToken, expressionList:TExpressionListSyntax)
		Self.keyword = keyword
		Self.expressionList = expressionList
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			expressionList ..
		)
	End Method
End Type



Type TRestoreDataStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly labelName:TNameSyntax
	
	Method New(keyword:TSyntaxToken, labelName:TNameSyntax)
		Self.keyword = keyword
		Self.labelName = labelName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			labelName ..
		)
	End Method
End Type



Type TReleaseStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly handleExpression:IExpressionSyntax
	
	Method New(keyword:TSyntaxToken, handleExpression:IExpressionSyntax)
		Self.keyword = keyword
		Self.handleExpression = handleExpression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			handleExpression ..
		)
	End Method
End Type



Type TAssignmentStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly target:IExpressionSyntax ' TODO: constrain to lvalue?
	Field ReadOnly assignment:TAssignmentSyntax
	
	Method New(target:IExpressionSyntax, assignment:TAssignmentSyntax)
		Self.target = target
		Self.assignment = assignment
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			target, ..
			assignment ..
		)
	End Method
End Type



Type TParenlessCallStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly expression:IExpressionSyntax
	Field ReadOnly callOperator:TExpressionListSyntax
	
	Method New(expression:IExpressionSyntax, callOperator:TExpressionListSyntax)
		Self.expression = expression
		Self.callOperator = callOperator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			callOperator ..
		)
	End Method
End Type



Type TExpressionStatementSyntax Extends TSyntax Implements IStatementSyntax Final
	Field ReadOnly expression:IExpressionSyntax
	
	Method New(expression:IExpressionSyntax)
		Assert TCallExpressionSyntax(expression) Or TNewExpressionSyntax(expression) Or TParenExpressionSyntax(expression) Else "Invalid expression type"
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Expressions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Interface IExpressionSyntax Extends ISyntax
End Interface



' I*CompatibleExpressionSyntax interfaces inherit from each other and form a type hierarchy
' representing the way different kinds of expressions can be nested inside each other
' T*ExpressionSyntax classes implement these interfaces, but do not inherit from each other,
' since different kinds of expressions do not necessarily share any concrete members



Interface IRangeCompatibleExpressionSyntax Extends IExpressionSyntax
End Interface



Type TRangeExpressionSyntax Extends TSyntax Implements IRangeCompatibleExpressionSyntax Final
	' the range operator is not a regular binary operator; its arguments are optional
	' it can be used as binary, unary (postfix or prefix), or nullary operator
	Field ReadOnly lhs:IRangeCompatibleExpressionSyntax {nullable}
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IOrCompatibleExpressionSyntax {nullable}
	
	Method New(lhs:IRangeCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IOrCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IOrCompatibleExpressionSyntax Extends IRangeCompatibleExpressionSyntax
End Interface



Type TOrExpressionSyntax Extends TSyntax Implements IOrCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IOrCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IAndCompatibleExpressionSyntax
	
	Method New(lhs:IOrCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IAndCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IAndCompatibleExpressionSyntax Extends IOrCompatibleExpressionSyntax
End Interface



Type TAndExpressionSyntax Extends TSyntax Implements IAndCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IAndCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IRelationalCompatibleExpressionSyntax
	
	Method New(lhs:IAndCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IRelationalCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IRelationalCompatibleExpressionSyntax Extends IAndCompatibleExpressionSyntax
End Interface



Type TRelationalExpressionSyntax Extends TSyntax Implements IRelationalCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IRelationalCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IUnionCompatibleExpressionSyntax
	
	Method New(lhs:IRelationalCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IUnionCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IUnionCompatibleExpressionSyntax Extends IRelationalCompatibleExpressionSyntax
End Interface



Type TUnionExpressionSyntax Extends TSyntax Implements IUnionCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IUnionCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IIntersectionCompatibleExpressionSyntax
	
	Method New(lhs:IUnionCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IIntersectionCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type
	


Interface IIntersectionCompatibleExpressionSyntax Extends IUnionCompatibleExpressionSyntax
End Interface



Type TIntersectionExpressionSyntax Extends TSyntax Implements IIntersectionCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IIntersectionCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:ISumCompatibleExpressionSyntax
	
	Method New(lhs:IIntersectionCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:ISumCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface ISumCompatibleExpressionSyntax Extends IIntersectionCompatibleExpressionSyntax
End Interface



Type TSumExpressionSyntax Extends TSyntax Implements ISumCompatibleExpressionSyntax Final
	Field ReadOnly lhs:ISumCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IProductCompatibleExpressionSyntax
	
	Method New(lhs:ISumCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IProductCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IProductCompatibleExpressionSyntax Extends ISumCompatibleExpressionSyntax
End Interface



Type TProductExpressionSyntax Extends TSyntax Implements IProductCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IProductCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IExponentialCompatibleExpressionSyntax
	
	Method New(lhs:IProductCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IExponentialCompatibleExpressionSyntax)
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IExponentialCompatibleExpressionSyntax Extends IProductCompatibleExpressionSyntax
End Interface



Type TExponentialExpressionSyntax Extends TSyntax Implements IExponentialCompatibleExpressionSyntax Final
	Field ReadOnly lhs:IExponentialCompatibleExpressionSyntax
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly rhs:IPrefixCompatibleExpressionSyntax
	
	Method New(lhs:IExponentialCompatibleExpressionSyntax, op:TOperatorSyntax, rhs:IPrefixCompatibleExpressionSyntax) ' TODO: lhs type
		Self.lhs = lhs
		Self.op = op
		Self.rhs = rhs
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lhs, ..
			op, ..
			rhs ..
		)
	End Method
End Type



Interface IPrefixCompatibleExpressionSyntax Extends IExponentialCompatibleExpressionSyntax
End Interface



Type TTypeCastExpressionSyntax Extends TSyntax Implements IPrefixCompatibleExpressionSyntax Final
	Field ReadOnly targetType:TTypeSyntax
	Field ReadOnly expression:IPrefixCompatibleExpressionSyntax
	
	Method New(targetType:TTypeSyntax, expression:IPrefixCompatibleExpressionSyntax)
		Self.targetType = targetType
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			targetType, ..
			expression ..
		)
	End Method
End Type



Type TPrefixOperatorExpressionSyntax Extends TSyntax Implements IPrefixCompatibleExpressionSyntax Final
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly expression:IPrefixCompatibleExpressionSyntax
	
	Method New(op:TOperatorSyntax, expression:IPrefixCompatibleExpressionSyntax)
		Self.op = op
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			op, ..
			expression ..
		)
	End Method
End Type



Interface IPostfixCompatibleExpressionSyntax Extends IPrefixCompatibleExpressionSyntax
End Interface



Type TMemberAccessExpressionSyntax Extends TSyntax Implements IPostfixCompatibleExpressionSyntax Final
	Field ReadOnly expression:IPostfixCompatibleExpressionSyntax {nullable}
	Field ReadOnly dot:TSyntaxToken {minor}
	Field ReadOnly memberName:TNameSyntax
	
	Method New(expression:IPostfixCompatibleExpressionSyntax, dot:TSyntaxToken, memberName:TNameSyntax)
		Self.expression = expression
		Self.dot = dot
		Self.memberName = memberName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			dot, ..
			memberName ..
		)
	End Method
End Type



Type TIndexExpressionSyntax Extends TSyntax Implements IPostfixCompatibleExpressionSyntax Final
	Field ReadOnly expression:IPostfixCompatibleExpressionSyntax
	Field ReadOnly indexOperator:TBracketExpressionListSyntax
	
	Method New(expression:IPostfixCompatibleExpressionSyntax, indexOperator:TBracketExpressionListSyntax)
		Self.expression = expression
		Self.indexOperator = indexOperator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			indexOperator ..
		)
	End Method
End Type



Type TCallExpressionSyntax Extends TSyntax Implements IPostfixCompatibleExpressionSyntax Final
	Field ReadOnly expression:IPostfixCompatibleExpressionSyntax
	Field ReadOnly callOperator:TParenExpressionListSyntax
	
	Method New(expression:IPostfixCompatibleExpressionSyntax, callOperator:TParenExpressionListSyntax)
		Self.expression = expression
		Self.callOperator = callOperator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			callOperator ..
		)
	End Method
End Type



Type TTypeApplicationExpressionSyntax Extends TSyntax Implements IPostfixCompatibleExpressionSyntax Final
	Field ReadOnly expression:IPostfixCompatibleExpressionSyntax
	Field ReadOnly typeApplicationOperator:TTypeArgumentListSyntax
	
	Method New(expression:IPostfixCompatibleExpressionSyntax, typeApplicationOperator:TTypeArgumentListSyntax)
		Self.expression = expression
		Self.typeApplicationOperator = typeApplicationOperator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			typeApplicationOperator ..
		)
	End Method
End Type



Type TTypeAssertionExpressionSyntax Extends TSyntax Implements IPostfixCompatibleExpressionSyntax Final
	Field ReadOnly expression:IPostfixCompatibleExpressionSyntax
	Field ReadOnly type_:TTypeSyntax
	
	Method New(expression:IPostfixCompatibleExpressionSyntax, type_:TTypeSyntax)
		Self.expression = expression
		Self.type_ = type_
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			expression, ..
			type_ ..
		)
	End Method
End Type



Interface IPrimaryExpressionSyntax Extends IPostfixCompatibleExpressionSyntax
End Interface



Type TParenExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly lparen:TSyntaxToken {minor}
	Field ReadOnly expression:IExpressionSyntax
	Field ReadOnly rparen:TSyntaxToken {minor}
	
	Method New(lparen:TSyntaxToken, expression:IExpressionSyntax, rparen:TSyntaxToken)
		Self.lparen = lparen
		Self.expression = expression
		Self.rparen = rparen
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lparen, ..
			expression, ..
			rparen ..
		)
	End Method
End Type



Type TNewExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly keyword:TSyntaxToken {minor}
	Field ReadOnly type_:TTypeSyntax ' TODO: separate brackets with dimensions from the type? then TArrayTypeModifierSyntax doesn't need to have a TBracketExpressionList
	Field ReadOnly callOperator:TParenExpressionListSyntax {nullable}
	
	Method New(keyword:TSyntaxToken, type_:TTypeSyntax, callOperator:TParenExpressionListSyntax)
		Self.keyword = keyword
		Self.type_ = type_
		Self.callOperator = callOperator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword, ..
			type_, ..
			callOperator ..
		)
	End Method
End Type



Type TSelfExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TSuperExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TNullExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TTrueExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TFalseExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TPiExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TNameExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Final
	Field ReadOnly name:TNameSyntax
	
	Method New(name:TNameSyntax)
		Self.name = name
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			name ..
		)
	End Method
End Type



Type TLiteralExpressionSyntax Extends TSyntax Implements IPrimaryExpressionSyntax Abstract
End Type



Type TNumericLiteralExpressionSyntax Extends TLiteralExpressionSyntax Final
	Field ReadOnly value:TSyntaxToken
	Field ReadOnly type_:TTypeSyntax {nullable}
	
	Method New(value:TSyntaxToken, type_:TTypeSyntax)
		Self.value = value
		Self.type_ = type_
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			value, ..
			type_ ..
		)
	End Method
End Type



Type TStringLiteralExpressionSyntax Extends TLiteralExpressionSyntax Final
	Field ReadOnly value:TSyntaxToken
	Field ReadOnly type_:TTypeSyntax {nullable}
	
	Method New(value:TSyntaxToken, type_:TTypeSyntax)
		Self.value = value
		Self.type_ = type_
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			value, ..
			type_ ..
		)
	End Method
End Type



Type TArrayLiteralExpressionSyntax Extends TLiteralExpressionSyntax Final
	Field ReadOnly elementList:TBracketExpressionListSyntax
	
	Method New(elementList:TBracketExpressionListSyntax)
		Self.elementList = elementList
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elementList ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Types ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Type TTypeSyntax Extends TSyntax Final
	Field ReadOnly colon:TSyntaxToken {nullable minor}
	Field ReadOnly base:TTypeBaseSyntax {nullable} ' should only be null in the case of a void-returning callable type
	Field ReadOnly marshallingModifier:TTypeMarshallingModifierSyntax {nullable}
	Field ReadOnly typeArguments:TTypeArgumentListSyntax {nullable}
	Field ReadOnly suffixes:TTypeSuffixSyntax[]
	
	Method New(colon:TSyntaxToken, base:TTypeBaseSyntax, marshallingModifier:TTypeMarshallingModifierSyntax, typeArguments:TTypeArgumentListSyntax, suffixes:TTypeSuffixSyntax[])
		Self.colon = colon
		Self.base = base
		Self.marshallingModifier = marshallingModifier
		Self.typeArguments = typeArguments
		Self.suffixes = suffixes
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			colon, ..
			base, ..
			marshallingModifier, ..
			typeArguments, ..
			suffixes ..
		)
	End Method
End Type



Type TTypeBaseSyntax Extends TSyntax Abstract
End Type



Type TKeywordTypeBaseSyntax Extends TTypeBaseSyntax Final
	Field ReadOnly keyword:TSyntaxToken
	
	Method New(keyword:TSyntaxToken)
		Self.keyword = keyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword ..
		)
	End Method
End Type



Type TSigilTypeBaseSyntax Extends TTypeBaseSyntax Final
	Field ReadOnly sigil:TSyntaxToken
	
	Method New(sigil:TSyntaxToken)
		Self.sigil = sigil
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			sigil ..
		)
	End Method
End Type



Type TQualifiedNameTypeBaseSyntax Extends TTypeBaseSyntax Final
	Field ReadOnly name:TQualifiedNameSyntax
	
	Method New(name:TQualifiedNameSyntax)
		Self.name = name
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			name ..
		)
	End Method
End Type



Type TTypeMarshallingModifierSyntax Extends TSyntax Abstract
End Type



Type TCStringTypeMarshallingModifierSyntax Extends TTypeMarshallingModifierSyntax Final
	Field ReadOnly keyword:TContextualKeywordSyntax {minor}
	
	Method New(keyword:TContextualKeywordSyntax)
		Self.keyword = keyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword ..
		)
	End Method
End Type



Type TWStringTypeMarshallingModifierSyntax Extends TTypeMarshallingModifierSyntax Final
	Field ReadOnly keyword:TContextualKeywordSyntax {minor}
	
	Method New(keyword:TContextualKeywordSyntax)
		Self.keyword = keyword
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			keyword ..
		)
	End Method
End Type



Type TTypeSuffixSyntax Extends TSyntax Abstract
End Type



Type TPtrTypeSuffixSyntax Extends TTypeSuffixSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TVarTypeSuffixSyntax Extends TTypeSuffixSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TArrayTypeSuffixSyntax Extends TTypeSuffixSyntax Final
	Field ReadOnly lbracket:TSyntaxToken {minor}
	Field ReadOnly dimensionsList:TExpressionListSyntax ' TODO: this can contain non-empty elements only if it appears after a New
	Field ReadOnly rbracket:TSyntaxToken {minor}
	
	Method New(lbracket:TSyntaxToken, dimensionsList:TExpressionListSyntax, rbracket:TSyntaxToken)
		Self.lbracket = lbracket
		Self.dimensionsList = dimensionsList
		Self.rbracket = rbracket
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lbracket, ..
			dimensionsList, ..
			rbracket ..
		)
	End Method
End Type



Type TCallableTypeSuffixSyntax Extends TTypeSuffixSyntax Final
	Field ReadOnly lparen:TSyntaxToken {minor}
	Field ReadOnly parameterDeclaration:TVariableDeclarationSyntax
	Field ReadOnly rparen:TSyntaxToken {minor}
	
	Method New(lparen:TSyntaxToken, parameterDeclaration:TVariableDeclarationSyntax, rparen:TSyntaxToken)
		Self.lparen = lparen
		Self.parameterDeclaration = parameterDeclaration
		Self.rparen = rparen
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lparen, ..
			parameterDeclaration, ..
			rparen ..
		)
	End Method
End Type



' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Auxiliary Constructs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Type TVariableDeclaratorListSyntax Extends TSyntax Final
	Field ReadOnly elements:TVariableDeclaratorListElementSyntax[]
	
	Method New(elements:TVariableDeclaratorListElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Type TVariableDeclaratorListElementSyntax Extends TSyntax Final
	Field ReadOnly comma:TSyntaxToken {nullable minor}
	Field ReadOnly declarator:TVariableDeclaratorSyntax {nullable}
	
	Method New(comma:TSyntaxToken, declarator:TVariableDeclaratorSyntax)
		Self.comma = comma
		Self.declarator = declarator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			comma, ..
			declarator ..
		)
	End Method
End Type



Type TTypeParameterDeclaratorListSyntax Extends TSyntax Final
	Field ReadOnly elements:TTypeParameterDeclaratorListElementSyntax[]
	
	Method New(elements:TTypeParameterDeclaratorListElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Type TTypeParameterDeclaratorListElementSyntax Extends TSyntax Final
	Field ReadOnly comma:TSyntaxToken {nullable minor}
	Field ReadOnly declarator:TTypeParameterDeclaratorSyntax {nullable}
	
	Method New(comma:TSyntaxToken, declarator:TTypeParameterDeclaratorSyntax)
		Self.comma = comma
		Self.declarator = declarator
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			comma, ..
			declarator ..
		)
	End Method
End Type



Type TParenExpressionListSyntax Extends TSyntax Final
	Field ReadOnly lparen:TSyntaxToken {minor}
	Field ReadOnly expressionList:TExpressionListSyntax
	Field ReadOnly rparen:TSyntaxToken {minor}
	
	Method New(lparen:TSyntaxToken, expressionList:TExpressionListSyntax, rparen:TSyntaxToken)
		Self.lparen = lparen
		Self.expressionList = expressionList
		Self.rparen = rparen
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lparen, ..
			expressionList, ..
			rparen ..
		)
	End Method
End Type



Type TBracketExpressionListSyntax Extends TSyntax Final
	Field ReadOnly lbracket:TSyntaxToken {minor}
	Field ReadOnly expressionList:TExpressionListSyntax
	Field ReadOnly rbracket:TSyntaxToken {minor}
	
	Method New(lbracket:TSyntaxToken, expressionList:TExpressionListSyntax, rbracket:TSyntaxToken)
		Self.lbracket = lbracket
		Self.expressionList = expressionList
		Self.rbracket = rbracket
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lbracket, ..
			expressionList, ..
			rbracket ..
		)
	End Method
End Type



Type TExpressionListSyntax Extends TSyntax Final
	Field ReadOnly elements:TExpressionListElementSyntax[]
	
	Method New(elements:TExpressionListElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Type TExpressionListElementSyntax Extends TSyntax Final
	Field ReadOnly comma:TSyntaxToken {nullable minor}
	Field ReadOnly expression:IExpressionSyntax {nullable}
	
	Method New(comma:TSyntaxToken, expression:IExpressionSyntax)
		Self.comma = comma
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			comma, ..
			expression ..
		)
	End Method
End Type



Type TTypeParameterListSyntax Extends TSyntax Final
	Field ReadOnly lchevron:TSyntaxToken {minor}
	Field ReadOnly list:TTypeParameterDeclaratorListSyntax
	Field ReadOnly rchevron:TSyntaxToken {minor}
	
	Method New(lchevron:TSyntaxToken, list:TTypeParameterDeclaratorListSyntax, rchevron:TSyntaxToken)
		Self.lchevron = lchevron
		Self.list = list
		Self.rchevron = rchevron
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lchevron, ..
			list, ..
			rchevron ..
		)
	End Method
End Type



Type TTypeArgumentListSyntax Extends TSyntax Final
	Field ReadOnly lchevron:TSyntaxToken {minor}
	Field ReadOnly list:TTypeListSyntax
	Field ReadOnly rchevron:TSyntaxToken {minor}
	
	Method New(lchevron:TSyntaxToken, list:TTypeListSyntax, rchevron:TSyntaxToken)
		Self.lchevron = lchevron
		Self.list = list
		Self.rchevron = rchevron
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lchevron, ..
			list, ..
			rchevron ..
		)
	End Method
End Type



Type TTypeListSyntax Extends TSyntax Final
	Field ReadOnly elements:TTypeListElementSyntax[]
	
	Method New(elements:TTypeListElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Type TTypeListElementSyntax Extends TSyntax Final
	Field ReadOnly comma:TSyntaxToken {nullable minor}
	Field ReadOnly type_:TTypeSyntax {nullable}
	
	Method New(comma:TSyntaxToken, type_:TTypeSyntax)
		Self.comma = comma
		Self.type_ = type_
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			comma, ..
			type_ ..
		)
	End Method
End Type



Type TQualifiedNameListSyntax Extends TSyntax Final
	Field ReadOnly elements:TQualifiedNameListElementSyntax[]
	
	Method New(elements:TQualifiedNameListElementSyntax[])
		Self.elements = elements
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			elements ..
		)
	End Method
End Type



Type TQualifiedNameListElementSyntax Extends TSyntax Final
	Field ReadOnly comma:TSyntaxToken {nullable minor}
	Field ReadOnly name:TQualifiedNameSyntax {nullable}
	
	Method New(comma:TSyntaxToken, name:TQualifiedNameSyntax)
		Self.comma = comma
		Self.name = name
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			comma, ..
			name ..
		)
	End Method
End Type



Type TMetaDataSyntax Extends TSyntax Final
	Field ReadOnly lbrace:TSyntaxToken {minor}
	Field ReadOnly elements:TMetaDataElementSyntax[]
	Field ReadOnly rbrace:TSyntaxToken {minor}
	
	Method New(lbrace:TSyntaxToken, elements:TMetaDataElementSyntax[], rbrace:TSyntaxToken)
		Self.lbrace = lbrace
		Self.elements = elements
		Self.rbrace = rbrace
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			lbrace, ..
			elements, ..
			rbrace ..
		)
	End Method
End Type



Type TMetaDataElementSyntax Extends TSyntax Final
	Field ReadOnly key:TNameSyntax
	Field ReadOnly eq:TSyntaxToken {nullable minor}
	Field ReadOnly value:IExpressionSyntax {nullable}
	
	Method New(key:TNameSyntax, eq:TSyntaxToken, value:IExpressionSyntax)
		Self.key = key
		Self.eq = eq
		Self.value = value
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			key, ..
			eq, ..
			value ..
		)
	End Method
End Type



Type TNameSyntax Extends TSyntax Final
	Field ReadOnly identifier:TSyntaxToken
	
	Method New(identifier:TSyntaxToken)
		Self.identifier = identifier
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			identifier ..
		)
	End Method
End Type



Type TQualifiedNameSyntax Extends TSyntax Final
	Field ReadOnly parts:TQualifiedNamePartSyntax[]
	
	Method New(parts:TQualifiedNamePartSyntax[])
		Self.parts = parts
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			parts ..
		)
	End Method
End Type



Type TQualifiedNamePartSyntax Extends TSyntax Final
	Field ReadOnly dot:TSyntaxToken {nullable minor}
	Field ReadOnly identifier:TSyntaxToken {nullable}
	
	Method New(dot:TSyntaxToken, identifier:TSyntaxToken)
		Self.dot = dot
		Self.identifier = identifier
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			dot, ..
			identifier ..
		)
	End Method
End Type



Type TImportSourceSyntax Extends TSyntax Final
	' exactly one field must be non-null
	Field ReadOnly moduleName:TQualifiedNameSyntax {nullable}
	Field ReadOnly filePath:TStringLiteralExpressionSyntax {nullable}
	
	Method New(moduleName:TQualifiedNameSyntax)
		Self.moduleName = moduleName
	End Method
	
	Method New(filePath:TStringLiteralExpressionSyntax)
		Self.filePath = filePath
	End Method

	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			moduleName, ..
			filePath ..
		)
	End Method
End Type



Type TCallableDeclarationNameSyntax Extends TSyntax Final
	' exactly one field must be non-null
	Field ReadOnly identifierName:TNameSyntax {nullable}
	Field ReadOnly keywordName:TSyntaxToken {nullable}
	Field ReadOnly operatorName:TOperatorSyntax {nullable}
	
	Method New(identifierName:TNameSyntax)
		Self.identifierName = identifierName
	End Method
	
	Method New(keywordName:TSyntaxToken)
		Self.keywordName = keywordName
	End Method
	
	Method New(operatorName:TOperatorSyntax)
		Self.operatorName = operatorName
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			identifierName, ..
			keywordName, ..
			operatorName ..
		)
	End Method
End Type



Type TOperatorSyntax Extends TSyntax Final
	Field ReadOnly tokens:TSyntaxToken[] ' this should not be empty, will usually contain one element
	
	Method New(tokens:TSyntaxToken[])
		Self.tokens = tokens
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			tokens ..
		)
	End Method
End Type



Type TContextualKeywordSyntax Extends TSyntax Final
	Field ReadOnly canonicalValue:String
	Field ReadOnly identifier:TSyntaxToken {minor}
		
	Method New(canonicalValue:String, identifier:TSyntaxToken)
		Self.canonicalValue = canonicalValue
		Self.identifier = identifier
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			identifier ..
		)
	End Method
End Type



Type TAssignmentSyntax Extends TSyntax Final
	Field ReadOnly op:TOperatorSyntax
	Field ReadOnly expression:IExpressionSyntax
	
	Method New(op:TOperatorSyntax, expression:IExpressionSyntax)
		Self.op = op
		Self.expression = expression
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			op, ..
			expression ..
		)
	End Method
End Type



Type TExternSignatureAssignmentSyntax Extends TSyntax Final
	Field ReadOnly op:TOperatorSyntax {minor}
	Field ReadOnly externSignature:TSyntaxToken
	
	Method New(op:TOperatorSyntax, externSignature:TSyntaxToken)
		Self.op = op
		Self.externSignature = externSignature
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			op, ..
			externSignature ..
		)
	End Method
End Type



Type TStatementSeparatorSyntax Extends TSyntax Implements ICodeHeaderElementSyntax, ICodeBlockElementSyntax, IExternBlockElementSyntax, IEnumMemberSyntax Final
	Field ReadOnly token:TSyntaxToken {minor}
	
	Method New(token:TSyntaxToken)
		Self.token = token
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			token ..
		)
	End Method
End Type



Type TErrorSyntax Extends TSyntax Implements ICodeHeaderElementSyntax, ICodeBlockElementSyntax, IExternBlockElementSyntax, IEnumMemberSyntax Final
	Field ReadOnly tokens:TSyntaxToken[] {minor}
	
	Method New(tokens:TSyntaxToken[])
		Self.tokens = tokens
		Verify Self
	End Method
	
	Method GetChildren:ISyntaxOrSyntaxToken[]() Override
		Return ChildrenToArray( ..
			tokens ..
		)
	End Method
End Type



Private

Function Verify(syntax:ISyntax)
	? Debug
	Local t:TTypeId = TTypeId.ForObject(syntax)
	For Local f:TField = EachIn GetAllFields(t)
		Local fValue:Object = f.Get(syntax)
		If Not fValue And Not f.TypeId().ExtendsType(ArrayTypeId) And Not TTypeId.ForObject(fValue) And Not f.MetaData("nullable") Then
			' check for {nullable} violations
			RuntimeError "Non-nullable field " + f.Name() + " in " + t.Name() + " instance contains Null value"
		Else If fValue And f.TypeId().ExtendsType(ArrayTypeId) And TTypeId.ForObject(fValue).Name() <> "Null[]" Then
			' check for null objects in arrays
			For Local i:Int = 0 Until f.TypeId().ArrayLength(fValue)
				Local element:Object = f.TypeId().GetArrayElement(fValue, i)
				If Not element And Not TTypeId.ForObject(element) Then
					RuntimeError "Array in field " + f.Name() + " in " + t.Name() + " instance contains Null object"
				End If
			Next
		End If
	Next
	Local hasChildren:Int = False
	For Local c:ISyntaxOrSyntaxToken = EachIn syntax.GetChildren()
		hasChildren = True
		Exit
	Next
	'If Not hasChildren Then RuntimeError t.Name() + " instance has no non-null children"
	?
End Function

Function ChildrenToArray:ISyntaxOrSyntaxToken[](c1:Object = Null, c2:Object = Null, c3:Object = Null, c4:Object = Null, c5:Object = Null, c6:Object = Null, c7:Object = Null, c8:Object = Null, c9:Object = Null, c10:Object = Null, c11:Object = Null, c12:Object = Null)
	' filters out null objects and flattens arrays; returns array of the results
	
	Function GetCount:Int(c:Object)
		Local cAsArray:ISyntaxOrSyntaxToken[] = ISyntaxOrSyntaxToken[](c)
		If cAsArray Then
			Local nonNullElements:Int = 0
			For Local i:Int = 0 Until cAsArray.length
				If cAsArray[i] Then nonNullElements :+ 1
			Next
			Return nonNullElements
		Else If ISyntaxOrSyntaxToken(c) Then ' need this cast to filter out empty arrays
			Return 1
		Else
			Return 0
		End If
	End Function
	
	Function AddToArray(a:ISyntaxOrSyntaxToken[], currentIndex:Int Var, c:Object)
		Local cAsArray:ISyntaxOrSyntaxToken[] = ISyntaxOrSyntaxToken[](c)
		Local cAsSyntaxOrSyntaxToken:ISyntaxOrSyntaxToken = ISyntaxOrSyntaxToken(c)
		If cAsArray Then
			For Local i:Int = 0 Until cAsArray.length
				If cAsArray[i] Then
					a[currentIndex] = cAsArray[i]
					currentIndex :+ 1
				End If
			Next
		Else If cAsSyntaxOrSyntaxToken Then
			a[currentIndex] = cAsSyntaxOrSyntaxToken
			currentIndex :+ 1
		Else
			? Debug
			If c And TTypeId.ForObject(c).Name() <> "Null[]" Then RuntimeError "Invalid child element type"
			?
		End If
	End Function
	
	Local elementCount:Int = ..
		GetCount(c1) + ..
		GetCount(c2) + ..
		GetCount(c3) + ..
		GetCount(c4) + ..
		GetCount(c5) + ..
		GetCount(c6) + ..
		GetCount(c7) + ..
		GetCount(c8) + ..
		GetCount(c9) + ..
		GetCount(c10) + ..
		GetCount(c11) + ..
		GetCount(c12)
	
	Local children:ISyntaxOrSyntaxToken[elementCount]
	Local currentIndex:Int = 0
	AddToArray children, currentIndex, c1
	AddToArray children, currentIndex, c2
	AddToArray children, currentIndex, c3
	AddToArray children, currentIndex, c4
	AddToArray children, currentIndex, c5
	AddToArray children, currentIndex, c6
	AddToArray children, currentIndex, c7
	AddToArray children, currentIndex, c8
	AddToArray children, currentIndex, c9
	AddToArray children, currentIndex, c10
	AddToArray children, currentIndex, c11
	AddToArray children, currentIndex, c12
	
	Return children
End Function
