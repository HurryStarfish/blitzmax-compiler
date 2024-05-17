-- TODO: replace Assert in TExpressionStatementSyntax with marker interface, do the same whereever else applicable
-- TODO: verify union-like types or replace with subtyping
-- TODO: metadata to validate token types in TSyntaxToken-type fields?

-- special types
Token = "TSyntaxToken"
String = "String"

fileheader = [[
SuperStrict
Import "SyntaxBase.bmx"
Import "SyntaxToken.bmx"
Import "../util/WeakReference.bmx"
? Debug
Import BRL.Reflection ' for Verify
Import "../util/ReflectionUtils.bmx"
?
Private
Include "SyntaxUtilsPrivate.bmx"
Public
]]

nodetypes = {
	{
		category = "Top-Level",
		{
			name = "TCompilationUnit", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "header", type = "TCodeHeader" },
				{ name = "body", type = "TCodeBody" },
				{ name = "eofToken", type = Token, minor = true }
			}
		},
		{
			name = "TCodeHeader", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "ICodeHeaderElement[]" }
			}
		},
		{
			name = "ICodeHeaderElement", extends = {"I"},
			kind = "Interface"
		},
		{
			name = "TCodeBody", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "block", type = "TCodeBlock" }
			}
		},
		{
			name = "TCodeBlock", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "ICodeBlockElement[]" }
			}
		},
		{
			name = "ICodeBlockElement", extends = {"I"},
			kind = "Interface"
		}
	},
	----------------------------------------------------------------------------------------
	{
		category = "Header Directives",
		{
			name = "IHeaderDirective", extends = {"ICodeHeaderElement"},
			kind = "Interface"
		},
		{
			name = "TStrictnessDirective", extends = {"T"}, implements = {"IHeaderDirective"},
			kind = "Type",
			fields = {
				{ name = "strictness", type = Token }
			}
		},
		{
			name = "TModuleDirective", extends = {"T"}, implements = {"IHeaderDirective"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "moduleName", type = "TQualifiedName" }
			}
		},
		{
			name = "TModuleInfoDirective", extends = {"T"}, implements = {"IHeaderDirective"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "info", type = "TStringLiteralExpression" }
			}
		},
		{
			name = "TFrameworkDirective", extends = {"T"}, implements = {"IHeaderDirective"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "moduleName", type = "TQualifiedName" }
			}
		},
		{
			name = "TImportDirective", extends = {"T"}, implements = {"IHeaderDirective"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "importSource", type = "TImportSource" }
			}
		}
	},
	----------------------------------------------------------------------------------------
	{
		category = "Include Directive",
		{
			name = "TIncludeDirective", extends = {"T"}, implements = {"ICodeBlockElement", "IExternBlockElement", "IEnumMember"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "filePath", type = "TStringLiteralExpression" },
				{ name = "body", type = "TCodeBody" },
				{ name = "eofToken", type = Token, minor = true }
			}
		}
	},
	----------------------------------------------------------------------------------------
	{
		category = "Declarations",
		{
			name = "IDeclaration", extends = {"ICodeBlockElement"},
			kind = "Interface"
		},
		{
			name = "TExternBlock", extends = {"T"}, implements = {"ICodeBlockElement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "callingConvention", type = Token, nullable = true },
				{ name = "elements", type = "IExternBlockElement[]" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "IExternBlockElement", extends = {"I"},
			kind = "Interface"
		},
		{
			name = "TExternDeclaration", extends = {"T"}, implements = {"IExternBlockElement", "IDeclaration"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TExternTypeDeclaration", extends = {"TExternDeclaration"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TExternClassDeclaration", extends = {"TExternTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				-- TODO
				-- { name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TExternStructDeclaration", extends = {"TExternTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				-- TODO
				-- { name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TExternFunctionDeclaration", extends = {"TExternDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TCallableDeclarationName" },
				{ name = "type_", type = "TType" },
				{ name = "externSignatureAssignment", type = "TExternSignatureAssignment" },
				{ name = "metaData", type = "TMetaData", nullable = true }
			}
		},
		{
			name = "TExternVariableDeclaration", extends = {"TExternDeclaration"},
			kind = "Type",
			fields = {
				{ name = "declarationKeyword", type = Token, nullable = true }, -- null for parameters
				--{ name = "modifiers", type = "TVariableModifier[]" },
				-- TODO
				{ name = "declarators", type = "TVariableDeclaratorList" },
				{ name = "metaData", type = "TMetaData", nullable = true }
			}
		},
		{
			name = "TTypeDeclaration", extends = {"T"}, implements = {"IDeclaration"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TClassDeclaration", extends = {"TTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				{ name = "typeParameters", type = "TTypeParameterList", nullable = true },
				{ name = "extendsKeyword", type = Token, nullable = true, minor = true },
				{ name = "superClass", type = "TType", nullable = true },
				{ name = "implementsKeyword", type = Token, nullable = true, minor = true },
				{ name = "superInterfaces", type = "TTypeList", nullable = true },
				{ name = "modifiers", type = "TTypeModifier[]" },
				{ name = "metaData", type = "TMetaData", nullable = true },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TStructDeclaration", extends = {"TTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				{ name = "typeParameters", type = "TTypeParameterList", nullable = true },
				{ name = "implementsKeyword", type = Token, nullable = true, minor = true },
				{ name = "superInterfaces", type = "TTypeList", nullable = true },
				{ name = "modifiers", type = "TTypeModifier[]" },
				{ name = "metaData", type = "TMetaData", nullable = true },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TInterfaceDeclaration", extends = {"TTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				{ name = "typeParameters", type = "TTypeParameterList", nullable = true },
				{ name = "extendsKeyword", type = Token, nullable = true, minor = true },
				{ name = "superInterfaces", type = "TTypeList", nullable = true },
				{ name = "modifiers", type = "TTypeModifier[]" },
				{ name = "metaData", type = "TMetaData", nullable = true },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TEnumDeclaration", extends = {"TTypeDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "name", type = "TName" },
				{ name = "baseType", type = "TType", nullable = true },
				{ name = "flagsKeyword", type = "TContextualKeyword", nullable = true },
				{ name = "metaData", type = "TMetaData", nullable = true },
				{ name = "members", type = "IEnumMember[]" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "IEnumMember", extends = {"I"},
			kind = "Interface"
		},
		{
			name = "TEnumMemberDeclaration", extends = {"T"}, implements = {"IEnumMember", "IDeclaration"},
			kind = "Type",
			fields = {
				{ name = "name", type = "TName" },
				{ name = "assignment", type = "TAssignment", nullable = true }
			}
		},
		{
			name = "TVisibilityDirective", extends = {"T"}, implements = {"ICodeBlockElement"},
			kind = "Type",
			fields = {
				{ name = "visibility", type = Token }
			}
		},
		{
			name = "TCallableDeclaration", extends = {"T"}, implements = {"IDeclaration"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token},
				{ name = "operatorKeyword", type = Token, nullable = true, minor = true },
				{ name = "name", type = "TCallableDeclarationName" },
				{ name = "typeParameters", type = "TTypeParameterList", nullable = true },
				{ name = "type_", type = "TType" },
				{ name = "modifiers", type = "TCallableModifier[]" },
				{ name = "metaData", type = "TMetaData", nullable = true },
				{ name = "body", type = "TCodeBlock", nullable = true },
				{ name = "terminatorKeyword", type = Token, nullable = true, minor = true }
			}
		},
		{
			name = "TVariableDeclaration", extends = {"T"}, implements = {"IDeclaration"},
			kind = "Type",
			fields = {
				{ name = "declarationKeyword", type = Token, nullable = true }, -- null for parameters
				{ name = "modifiers", type = "TVariableModifier[]" },
				{ name = "declarators", type = "TVariableDeclaratorList" },
				{ name = "metaData", type = "TMetaData", nullable = true }
			}
		},
		{
			name = "TVariableDeclarator", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "name", type = "TName" },
				{ name = "type_", type = "TType" },
				{ name = "initializer", type = "TAssignment", nullable = true }
			}
		},
		{
			name = "TModifier", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TTypeModifier", extends = {"TModifier"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token }
			}
		},
		{
			name = "TVariableModifier", extends = {"TModifier"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token }
			}
		},
		{
			name = "TCallableModifier", extends = {"TModifier"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token }
			}
		},
		{
			name = "TTypeParameterDeclarator", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "name", type = "TName" }
			}
		},
		{
			name = "TLabelDeclaration", extends = {"T"}, implements = {"IDeclaration"},
			kind = "Type",
			fields = {
				{ name = "hash", type = Token, minor = true },
				{ name = "name", type = "TName" }
			}
		}
	},
	----------------------------------------------------------------------------------------	
	{
		category = "Statements",
		{
			name = "IStatement", extends = {"ICodeBlockElement"},
			kind = "Interface"
		},
		{
			name = "TIfStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "condition", type = "IExpression" },
				{ name = "branches", type = "TIfBranch[]" }, -- always contains at least a Then branch
				{ name = "terminatorKeyword", type = Token, nullable = true, minor = true } -- null for single-line If
			}
		},
		{
			name = "TIfBranch", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TThenIfBranch", extends = {"TIfBranch"},
			kind = "Type",
			fields = {
				{ name = "thenKeyword", type = Token, nullable = true, minor = true },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TElseIfIfBranch", extends = {"TIfBranch"},
			kind = "Type",
			fields = {
				{ name = "elseIfKeyword", type = Token, minor = true },
				{ name = "condition", type = "IExpression" },
				{ name = "thenKeyword", type = Token, nullable = true, minor = true },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TElseIfBranch", extends = {"TIfBranch"},
			kind = "Type",
			fields = {
				{ name = "elseKeyword", type = Token, minor = true },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TSelectStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "expression", type = "IExpression" },
				{ name = "statementSeparators", type = "TStatementSeparator[]" },
				{ name = "branches", type = "TSelectBranch[]" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TSelectBranch", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TCaseSelectBranch", extends = {"TSelectBranch"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expressionList", type = "TExpressionList" },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TDefaultSelectBranch", extends = {"TSelectBranch"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TForStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "counter", type = "TForCounter" },
				{ name = "eq", type = Token, minor = true },
				{ name = "valueSequence", type = "TForValueSequence" },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TForCounter", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TForCounterDeclaration", extends = {"TForCounter"},
			kind = "Type",
			fields = {
				{ name = "declaration", type = "TVariableDeclaration" }
			}
		},
		{
			name = "TForCounterExpression", extends = {"TForCounter"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IExpression" }
			}
		},
		{
			name = "TForValueSequence", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TForToValueSequence", extends = {"TForValueSequence"},
			kind = "Type",
			fields = {
				{ name = "startExpression", type = "IExpression" },
				{ name = "keyword", type = Token, minor = true },
				{ name = "endExpression", type = "IExpression" }
			}
		},
		{
			name = "TForUntilValueSequence", extends = {"TForValueSequence"},
			kind = "Type",
			fields = {
				{ name = "startExpression", type = "IExpression" },
				{ name = "keyword", type = Token, minor = true },
				{ name = "endExpression", type = "IExpression" }
			}
		},
		{
			name = "TForEachInValueSequence", extends = {"TForValueSequence"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "iterableExpression", type = "IExpression" }
			}
		},
		{
			name = "TWhileStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "condition", type = "IExpression" },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TRepeatStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "body", type = "TCodeBlock" },
				{ name = "terminator", type = "TRepeatTerminator" }
			}
		},
		{
			name = "TRepeatTerminator", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TRepeatUntilTerminator", extends = {"TRepeatTerminator"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "condition", type = "IExpression" }
			}
		},
		{
			name = "TRepeatForeverTerminator", extends = {"TRepeatTerminator"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true }
			}
		},
		{
			name = "TExitStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "labelName", type = "TName", nullable = true }
			}
		},
		{
			name = "TContinueStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "labelName", type = "TName", nullable = true }
			}
		},
		{
			name = "TGotoStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "labelName", type = "TName" }
			}
		},
		{
			name = "TReturnStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expression", type = "IExpression", nullable = true }
			}
		},
		{
			name = "TTryStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "initiatorKeyword", type = Token, minor = true },
				{ name = "body", type = "TCodeBlock" },
				{ name = "branches", type = "TTryBranch[]" },
				{ name = "terminatorKeyword", type = Token, minor = true }
			}
		},
		{
			name = "TTryBranch", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TCatchTryBranch", extends = {"TTryBranch"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "declaration", type = "TVariableDeclaration" },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TFinallyTryBranch", extends = {"TTryBranch"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "body", type = "TCodeBlock" }
			}
		},
		{
			name = "TThrowStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expression", type = "IExpression" }
			}
		},
		{
			name = "TAssertStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expression", type = "IExpression" },
				{ name = "commaOrElse", type = Token, minor = true },
				{ name = "message", type = "TStringLiteralExpression" }
			}
		},
		{
			name = "TEndStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true }
			}
		},
		{
			name = "TNativeCodeStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token }
			}
		},
		{
			name = "TDefDataStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expressionList", type = "TExpressionList" }
			}
		},
		{
			name = "TReadDataStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "expressionList", type = "TExpressionList" }
			}
		},
		{
			name = "TRestoreDataStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "labelName", type = "TName" }
			}
		},
		{
			name = "TReleaseStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "handleExpression", type = "IExpression" }
			}
		},
		{
			name = "TAssignmentStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "target", type = "IExpression" }, -- TODO: constrain to lvalue?
				{ name = "assignment", type = "TAssignment" }
			}
		},
		{
			name = "TParenlessCallStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IExpression" },
				{ name = "callOperator", type = "TExpressionList" }
			}
		},
		{
			name = "TExpressionStatement", extends = {"T"}, implements = {"IStatement"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IExpression" }
			}
		}
	},
	----------------------------------------------------------------------------------------	
	{
		category = "Expressions",
		{
			name = "IExpression", extends = {"I"},
			kind = "Interface"
		},
		--[[
		I*CompatibleExpression interfaces inherit from each other and form a type hierarchy
		that represents the way different kinds of expressions can be nested inside each other;
		T*Expression classes implement these interfaces, but do not inherit from each other,
		since different kinds of expressions do not necessarily share any concrete data members
		]]
		{
			name = "IRangeCompatibleExpression", extends = {"IExpression"},
			kind = "Interface"
		},
		{
			-- the range operator is not a regular binary operator; both its operands are optional
			-- it can be used as binary, unary (postfix or prefix), or nullary operator
			name = "TRangeExpression", extends = {"T"}, implements = {"IRangeCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IRangeCompatibleExpression", nullable = true },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IOrCompatibleExpression", nullable = true }
			}
		},
		{
			name = "IOrCompatibleExpression", extends = {"IRangeCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TOrExpression", extends = {"T"}, implements = {"IOrCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IOrCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IAndCompatibleExpression" }
			}
		},
		{
			name = "IAndCompatibleExpression", extends = {"IOrCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TAndExpression", extends = {"T"}, implements = {"IAndCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IAndCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IRelationalCompatibleExpression" }
			}
		},
		{
			name = "IRelationalCompatibleExpression", extends = {"IAndCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TRelationalExpression", extends = {"T"}, implements = {"IRelationalCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IRelationalCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IUnionCompatibleExpression" }
			}
		},
		{
			name = "IUnionCompatibleExpression", extends = {"IRelationalCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TUnionExpression", extends = {"T"}, implements = {"IUnionCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IUnionCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IIntersectionCompatibleExpression" }
			}
		},
		{
			name = "IIntersectionCompatibleExpression", extends = {"IUnionCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TIntersectionExpression", extends = {"T"}, implements = {"IIntersectionCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IIntersectionCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "ISumCompatibleExpression" }
			}
		},
		{
			name = "ISumCompatibleExpression", extends = {"IIntersectionCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TSumExpression", extends = {"T"}, implements = {"ISumCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "ISumCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IProductCompatibleExpression" }
			}
		},
		{
			name = "IProductCompatibleExpression", extends = {"ISumCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TProductExpression", extends = {"T"}, implements = {"IProductCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IProductCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IExponentialCompatibleExpression" }
			}
		},
		{
			name = "IExponentialCompatibleExpression", extends = {"IProductCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TExponentialExpression", extends = {"T"}, implements = {"IExponentialCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "lhs", type = "IExponentialCompatibleExpression" },
				{ name = "op", type = "TOperator" },
				{ name = "rhs", type = "IPrefixCompatibleExpression" }
			}
		},
		{
			name = "IPrefixCompatibleExpression", extends = {"IExponentialCompatibleExpression"},
			kind = "Interface"
		},
		{
			-- not every type cast is parsed as this, some are parsed as calls instead
			name = "TTypeCastExpression", extends = {"T"}, implements = {"IPrefixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "targetType", type = "TType" },
				{ name = "expression", type = "IPrefixCompatibleExpression" }
			}
		},
		{
			name = "TPrefixOperatorExpression", extends = {"T"}, implements = {"IPrefixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "op", type = "TOperator" },
				{ name = "expression", type = "IPrefixCompatibleExpression" }
			}
		},
		{
			name = "IPostfixCompatibleExpression", extends = {"IPrefixCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TMemberAccessExpression", extends = {"T"}, implements = {"IPostfixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IPostfixCompatibleExpression", nullable = true },
				{ name = "dot", type = Token, minor = true },
				{ name = "memberName", type = "TName" }
			}
		},
		{
			name = "TIndexExpression", extends = {"T"}, implements = {"IPostfixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IPostfixCompatibleExpression" },
				{ name = "indexOperator", type = "TBracketExpressionList" }
			}
		},
		{
			-- may also be a type cast that is syntactically indistinguishable from a call
			name = "TCallExpression", extends = {"T"}, implements = {"IPostfixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IPostfixCompatibleExpression" },
				{ name = "callOperator", type = "TParenExpressionList" }
			}
		},
		{
			name = "TTypeApplicationExpression", extends = {"T"}, implements = {"IPostfixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IPostfixCompatibleExpression" },
				{ name = "typeApplicationOperator", type = "TTypeArgumentList" }
			}
		},
		{
			name = "TTypeAssertionExpression", extends = {"T"}, implements = {"IPostfixCompatibleExpression"},
			kind = "Type",
			fields = {
				{ name = "expression", type = "IPostfixCompatibleExpression" },
				{ name = "type_", type = "TType" }
			}
		},
		{
			name = "IPrimaryExpression", extends = {"IPostfixCompatibleExpression"},
			kind = "Interface"
		},
		{
			name = "TParenExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "lparen", type = Token, minor = true },
				{ name = "expression", type = "IExpression" },
				{ name = "rparen", type = Token, minor = true }
			}
		},
		{
			name = "TNewExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token, minor = true },
				{ name = "type_", type = "TType" }, -- TODO: separate brackets with dimensions from the type? then TArrayTypeModifier doesn't need to have a TBracketExpressionList
				{ name = "callOperator", type = "TParenExpressionList", nullable = true }
			}
		},
		{
			name = "TSelfExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TSuperExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TNullExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TTrueExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TFalseExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TPiExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TNameExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			fields = {
				{ name = "name", type = "TName" }
			}
		},
		{
			name = "TLiteralExpression", extends = {"T"}, implements = {"IPrimaryExpression"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TNumericLiteralExpression", extends = {"TLiteralExpression"},
			kind = "Type",
			fields = {
				{ name = "value", type = Token },
				{ name = "type_", type = "TType", nullable = true }
			}
		},
		{
			name = "TStringLiteralExpression", extends = {"TLiteralExpression"},
			kind = "Type",
			fields = {
				{ name = "value", type = Token },
				{ name = "type_", type = "TType", nullable = true }
			}
		},
		{
			name = "TArrayLiteralExpression", extends = {"TLiteralExpression"},
			kind = "Type",
			fields = {
				{ name = "elementList", type = "TBracketExpressionList" }
			}
		}
	},
	----------------------------------------------------------------------------------------	
	{
		category = "Types",
		{
			-- TODO: restructure to allow member access after type argument list
			name = "TType", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "colon", type = Token, nullable = true, minor = true },
				{ name = "base", type = "TTypeBase", nullable = true }, -- should only be null in a void-returning callable type
				{ name = "marshallingModifier", type = "TTypeMarshallingModifier", nullable = true },
				{ name = "typeArguments", type = "TTypeArgumentList", nullable = true },
				{ name = "suffixes", type = "TTypeSuffix[]" }
			}
		},
		{
			name = "TTypeBase", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TKeywordTypeBase", extends = {"TTypeBase"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = Token }
			}
		},
		{
			name = "TSigilTypeBase", extends = {"TTypeBase"},
			kind = "Type",
			fields = {
				{ name = "sigil", type = Token }
			}
		},
		{
			name = "TQualifiedNameTypeBase", extends = {"TTypeBase"},
			kind = "Type",
			fields = {
				{ name = "name", type = "TQualifiedName" }
			}
		},
		{
			name = "TTypeMarshallingModifier", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TCStringTypeMarshallingModifier", extends = {"TTypeMarshallingModifier"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = "TContextualKeyword", minor = true }
			}
		},
		{
			name = "TWStringTypeMarshallingModifier", extends = {"TTypeMarshallingModifier"},
			kind = "Type",
			fields = {
				{ name = "keyword", type = "TContextualKeyword", minor = true }
			}
		},
		{
			name = "TTypeSuffix", extends = {"T"},
			kind = "Type",
			abstract = true
		},
		{
			name = "TPtrTypeSuffix", extends = {"TTypeSuffix"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TVarTypeSuffix", extends = {"TTypeSuffix"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TArrayTypeSuffix", extends = {"TTypeSuffix"},
			kind = "Type",
			fields = {
				{ name = "lbracket", type = Token, minor = true },
				{ name = "dimensionsList", type = "TExpressionList" }, -- TODO: this can contain non-empty elements only if it appears after a New
				{ name = "rbracket", type = Token, minor = true }
			}
		},
		{
			name = "TCallableTypeSuffix", extends = {"TTypeSuffix"},
			kind = "Type",
			fields = {
				{ name = "lparen", type = Token, minor = true },
				{ name = "parameterDeclaration", type = "TVariableDeclaration" },
				{ name = "rparen", type = Token, minor = true }
			}
		}
	},
	----------------------------------------------------------------------------------------	
	{
		category = "Auxiliary Constructs",
		{
			name = "TVariableDeclaratorList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "TVariableDeclaratorListElement[]" }
			}
		},
		{
			name = "TVariableDeclaratorListElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "comma", type = Token, nullable = true, minor = true },
				{ name = "declarator", type = "TVariableDeclarator", nullable = true }
			}
		},
		{
			name = "TTypeParameterDeclaratorList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "TTypeParameterDeclaratorListElement[]" }
			}
		},
		{
			name = "TTypeParameterDeclaratorListElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "comma", type = Token, nullable = true, minor = true },
				{ name = "declarator", type = "TTypeParameterDeclarator", nullable = true }
			}
		},
		{
			name = "TParenExpressionList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "lparen", type = Token, minor = true },
				{ name = "expressionList", type = "TExpressionList" },
				{ name = "rparen", type = Token, minor = true }
			}
		},
		{
			name = "TBracketExpressionList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "lbracket", type = Token, minor = true },
				{ name = "expressionList", type = "TExpressionList" },
				{ name = "rbracket", type = Token, minor = true }
			}
		},
		{
			name = "TExpressionList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "TExpressionListElement[]" }
			}
		},
		{
			name = "TExpressionListElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "comma", type = Token, nullable = true, minor = true },
				{ name = "expression", type = "IExpression", nullable = true }
			}
		},
		{
			name = "TTypeParameterList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "lchevron", type = Token, minor = true },
				{ name = "typeDeclaratorList", type = "TTypeParameterDeclaratorList" },
				{ name = "rchevron", type = Token, minor = true }
			}
		},
		{
			name = "TTypeArgumentList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "lchevron", type = Token, minor = true },
				{ name = "typeList", type = "TTypeList" },
				{ name = "rchevron", type = Token, minor = true }
			}
		},
		{
			name = "TTypeList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "TTypeListElement[]" }
			}
		},
		{
			name = "TTypeListElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "comma", type = Token, nullable = true, minor = true },
				{ name = "type_", type = "TType", nullable = true }
			}
		},
		{
			name = "TQualifiedNameList", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "elements", type = "TQualifiedNameListElement[]" }
			}
		},
		{
			name = "TQualifiedNameListElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "comma", type = Token, nullable = true, minor = true },
				{ name = "name", type = "TQualifiedName", nullable = true }
			}
		},
		{
			name = "TMetaData", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "lbrace", type = Token, minor = true },
				{ name = "elements", type = "TMetaDataElement[]" },
				{ name = "rbrace", type = Token, minor = true }
			}
		},
		{
			name = "TMetaDataElement", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "key", type = "TName" },
				{ name = "eq", type = Token, nullable = true, minor = true },
				{ name = "value", type = "IExpression", nullable = true }
			}
		},
		{
			name = "TName", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "identifier", type = Token }
			}
		},
		{
			name = "TQualifiedName", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "parts", type = "TQualifiedNamePart[]" }
			}
		},
		{
			name = "TQualifiedNamePart", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "dot", type = Token, nullable = true, minor = true },
				{ name = "identifier", type = Token, nullable = true } -- null for leading dot
			}
		},
		{
			name = "TImportSource", extends = {"T"},
			kind = "Type",
			fields = {
				-- exactly one field must be non-null
				{ name = "moduleName", type = "TQualifiedName", nullable = true },
				{ name = "filePath", type = "TStringLiteralExpression", nullable = true }
			}
		},
		{
			name = "TCallableDeclarationName", extends = {"T"},
			kind = "Type",
			fields = {
				-- exactly one field must be non-null
				{ name = "identifierName", type = "TName", nullable = true },
				{ name = "keywordName", type = Token, nullable = true },
				{ name = "operatorName", type = "TOperator", nullable = true }
			}
		},
		{
			name = "TOperator", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "tokens", type = Token .. "[]" } -- this should not be empty, will usually contain one element
			}
		},
		{
			name = "TContextualKeyword", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "canonicalValue", type = String },
				{ name = "identifier", type = Token, minor = true }
			}
		},
		{
			name = "TAssignment", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "op", type = "TOperator" },
				{ name = "expression", type = "IExpression" }
			}
		},
		{
			name = "TExternSignatureAssignment", extends = {"T"},
			kind = "Type",
			fields = {
				{ name = "op", type = "TOperator", minor = true },
				{ name = "externSignature", type = Token }
			}
		},
		{
			name = "TStatementSeparator", extends = {"T"}, implements = {"ICodeHeaderElement", "ICodeBlockElement", "IExternBlockElement", "IEnumMember"},
			kind = "Type",
			fields = {
				{ name = "token", type = Token, minor = true }
			}
		},
		{
			name = "TError", extends = {"T"}, implements = {"ICodeHeaderElement", "ICodeBlockElement", "IExternBlockElement", "IEnumMember"},
			kind = "Type",
			fields = {
				{ name = "tokens", type = Token .. "[]", minor = true }
			}
		}
	}
}
