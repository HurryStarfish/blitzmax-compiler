SuperStrict
Framework BRL.StandardIO

Print New T.f3 ' 3
Print New T.f2 ' 1

Global G4:Int = 8

Type T
	Field f1:Int = G ' T.G - TODO: look into init order here
	Global G:Int = 1
	Field f2:Int = G ' T.G
	
	Field f3:Int = .G ' .G - TODO: look into init order here
	'Global G3:Int = G4 ' error - why?
End Type

'Local l1:Int = G ' error
Global G:Int = 3
Local l2:Int = G

Global G2:Int = 9
