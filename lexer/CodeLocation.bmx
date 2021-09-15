SuperStrict
Import BRL.FileSystem



Struct SCodeLocation
	Field ReadOnly filePath:String
	Field ReadOnly line:Int
	Field ReadOnly column:Int
	
	Method New(filePath:String, line:Int, column:Int)
		Self.filePath = filePath
		Self.line = line
		Self.column = column
	End Method
	
	Method ToString:String()
		Return "l:" + line + " c:" + column + " in " + StripDir(filePath) + " [" + filePath + "]"
	End Method
End Struct
