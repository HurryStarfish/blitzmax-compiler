SuperStrict
Import BRL.LinkedList



Type TGraphNode
	Field obj:Object
	Field edges:TGraphNode[]
	
	Field algorithmData:SAlgorithmData
	
	Method New(obj:Object = Null, edges:TGraphNode[] = [])
		Self.obj = obj
		Self.edges = edges
	End Method
End Type



Private
Struct SGraphNodeStack
	Field stack:TGraphNode[8]
	Field stackSize:Int = 0
	
	Method Push(node:TGraphNode)
		If stackSize = stack.length Then stack = stack[..stack.length * 2]
		stack[stackSize] = node
		stackSize :+ 1
	End Method
	
	Method Pop:TGraphNode()
		stackSize :- 1
		Local node:TGraphNode = stack[stackSize]
		stack[stackSize] = Null
		Return node
	End Method
End Struct

Struct SAlgorithmData
	Field index:Int = -1
	Field lowlink:Int
	Field onStack:Int
End Struct



Public
Function TopologicalSort:TGraphNode[][](nodes:TGraphNode[])
	' uses Tarjan's strongly connected components algorithm to calculate a topologically sorted
	' array containing the strongly connected components of the specified directed graph
	' every SCC is an array that represents a cycle in the graph (possibly consisting of just a single
	' node) and appears before any of its successors (that is, others SCCs that it has any edges to)
	' https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
	For Local node:TGraphNode = EachIn nodes
		node.algorithmData = New SAlgorithmData
	Next
	
	Local index:Int = 0
	Local s:SGraphNodeStack = New SGraphNodeStack
	Local sccs:TGraphNode[][]
	For Local v:TGraphNode = EachIn nodes
		If v.algorithmData.index = -1 Then StrongConnect v, index, s, sccs
	Next
	Return sccs
	
	Function StrongConnect(v:TGraphNode, index:Int Var, s:SGraphNodeStack Var, sccs:TGraphNode[][] Var)
		' Set the depth index for v to the smallest unused index
		v.algorithmData.index = index
		v.algorithmData.lowLink = index
		index :+ 1
		s.Push v
		v.algorithmData.onStack = True
		
		' Consider successors of v
		For Local w:TGraphNode = EachIn v.edges
			If w.algorithmData.index = -1 Then
				' Successor w has not yet been visited; recurse on it
				StrongConnect w, index, s, sccs
				v.algorithmData.lowLink = Min(v.algorithmData.lowLink, w.algorithmData.lowLink)
			Else If w.algorithmData.onStack Then
				' Successor w is in stack s and hence in the current SCC
				' If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
				' Note: The next line may look odd - but is correct.
				' It says w.index not w.lowLink; that is deliberate and from the original paper
				v.algorithmData.lowLink = Min(v.algorithmData.lowLink, w.algorithmData.index)
			End If
		Next
		
		' If v is a root node, pop the stack and generate an SCC
		If v.algorithmData.lowLink = v.algorithmData.index Then
			Local n:Int = s.stackSize
			Repeat n :- 1 Until s.stack[n] = v
			Local scc:TGraphNode[s.stackSize - n]
			For Local n:Int = 0 Until s.stackSize - n
				scc[n] = s.Pop()
				scc[n].algorithmData.onStack = False
			Next
			sccs :+ [scc]
		End If
	End Function
End Function

Function TopologicalSort:TList(nodes:TList)'<TList<TGraphNode>>'<TGraphNode>
	' uses Tarjan's strongly connected components algorithm to calculate a topologically sorted
	' array containing the strongly connected components of the specified directed graph
	' every SCC is an array that represents a cycle in the graph (possibly consisting of just a single
	' node) and appears before any of its successors (that is, others SCCs that it has any edges to)
	' https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
	For Local node:TGraphNode = EachIn nodes
		node.algorithmData = New SAlgorithmData
	Next
	
	Local index:Int = 0
	Local s:SGraphNodeStack = New SGraphNodeStack
	Local sccs:TList = New TList'<TList<TGraphNode>>
	For Local v:TGraphNode = EachIn nodes
		If v.algorithmData.index = -1 Then StrongConnect v, index, s, sccs
	Next
	Return sccs
	
	Function StrongConnect(v:TGraphNode, index:Int Var, s:SGraphNodeStack Var, sccs:TList)
		' Set the depth index for v to the smallest unused index
		v.algorithmData.index = index
		v.algorithmData.lowLink = index
		index :+ 1
		s.Push v
		v.algorithmData.onStack = True
		
		' Consider successors of v
		For Local w:TGraphNode = EachIn v.edges
			If w.algorithmData.index = -1 Then
				' Successor w has not yet been visited; recurse on it
				StrongConnect w, index, s, sccs
				v.algorithmData.lowLink = Min(v.algorithmData.lowLink, w.algorithmData.lowLink)
			Else If w.algorithmData.onStack Then
				' Successor w is in stack s and hence in the current SCC
				' If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
				' Note: The next line may look odd - but is correct.
				' It says w.index not w.lowLink; that is deliberate and from the original paper
				v.algorithmData.lowLink = Min(v.algorithmData.lowLink, w.algorithmData.index)
			End If
		Next
		
		' If v is a root node, pop the stack and generate an SCC
		If v.algorithmData.lowLink = v.algorithmData.index Then
			Local n:Int = s.stackSize
			Repeat n :- 1 Until s.stack[n] = v
			Local scc:TList = New TList'<TGraphNode>
			For Local n:Int = 0 Until s.stackSize - n
				Local w:TGraphNode = s.Pop()
				w.algorithmData.onStack = False
				scc.AddLast w
			Next
			sccs.AddLast scc
		End If
	End Function
End Function