SuperStrict
Import "CodeLocation.bmx"



Struct SCodeRange
	Field startLocation:SCodeLocation ' TODO: make read-only
	Field endLocation:SCodeLocation ' TODO: make read-only
	
	Method New(startLocation:SCodeLocation, endLocation:SCodeLocation)
		Self.startLocation = startLocation
		Self.endLocation = endLocation
	End Method
	
	Method IsValid:Int()
		Return startLocation.IsValid()
	End Method
	
	Method ToString:String()
		Return startLocation.ToString() + " - " + endLocation.ToString()
	End Method
End Struct
