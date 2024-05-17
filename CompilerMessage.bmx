SuperStrict



Type TCompilerMessage Final ' TODO: turn into struct?
	
	Field ReadOnly message:String
	
	Private
	Method New(message:String)
		Self.message = message
	End Method
	
	Public
	Method Format:String(args:String[] = [])
		Local messageStr:String = message.ToString()
		? Debug
		Local vars:Int = 0
		For Local c:Int = 0 Until messageStr.length
			If messageStr[c] = "$"[0] Then vars :+ 1
		Next
		Assert vars = args.length Else "Wrong number of arguments for message"
		?
		Local messageStrWithArgs:String = ""
		Local a:Int = 0
		Local s:Int = 0
		For Local c:Int = 0 Until messageStr.length
			If messageStr[c] = "$"[0] Then
				messageStrWithArgs :+ messageStr[s..c]
				messageStrWithArgs :+ formatArgs[a]
				s = c
				a :+ 1
			End If
		Next
		If s <> messageStr.length - 1 Then messageStrWithArgs :+ messageStr[s..]		
		Return messageStrWithArgs
	End Method
	
	Global :TCompilerMessage = New TCompilerMessage("Include")
End Type
