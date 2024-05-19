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
		Assert IsValid() Else "Invalid location" ' may only use default constructor or Null for this
	End Method
	
	Method IsValid:Int()
		Return line > 0 And column > 0 And filePath
	End Method
	
	Method ToString:String(includeFullFilePath:Int)
		Local str:String
		If includeFullFilePath Then
			str = " [" + filePath + "]"
		Else
			str = StripDir(filePath)
		End If
		str :+ " l:" + line + " c:" + column
		Return str
	End Method
	
	Method ToString:String()
		Return ToString(False)
	End Method
End Struct
