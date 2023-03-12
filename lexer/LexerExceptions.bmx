SuperStrict



Type TLexerFileReadException Extends TBlitzException Final
	Field ReadOnly message:String
	
	Private
	Method New() End Method
	
	Public
	Method New(message:String)
		Self.message = message
	End Method
	
	Method ToString:String() Override
		Return message
	End Method
End Type
