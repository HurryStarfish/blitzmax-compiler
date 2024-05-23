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
	
	Method ToString:String(filePathDisplay:ECodeLocationFilePathDisplay)
		Local str:String
		Select filePathDisplay
			Case ECodeLocationFilePathDisplay.Absolute str = filePath + " "
			Case ECodeLocationFilePathDisplay.Relative str = StripDir(filePath) + " "
			Case ECodeLocationFilePathDisplay.None str = ""
			Default RuntimeError "Missing case"
		End Select
		str :+ "l:" + line + " c:" + column
		Return str
	End Method
	
	Method ToString:String()
		Return ToString(ECodeLocationFilePathDisplay.Relative)
	End Method
End Struct

Enum ECodeLocationFilePathDisplay
	Absolute
	Relative
	None
End Enum
