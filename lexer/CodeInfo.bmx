SuperStrict



Struct SCodeInfo
	Field ReadOnly length:Int
	Field ReadOnly linebreakCount:Int
	Field ReadOnly charsAfterLastLinebreak:Int
	
	Method New(length:Int, linebreakCount:Int, charsAfterLastLinebreak:Int)
		Self.length = length
		Self.linebreakCount = linebreakCount
		Self.charsAfterLastLinebreak = charsAfterLastLinebreak
	End Method
	
	' combines the infos corresponding to two code strings such that the resulting info
	' corresponds to the concatenation of those strings, i.e. CI(s1) + CI(s2) = CI(s1 + s2)
	Method Operator +:SCodeInfo(nextInfo:SCodeInfo)
		Local charsAfterLastLinebreak:Int
		If nextInfo.linebreakCount = 0 Then
			charsAfterLastLinebreak = Self.charsAfterLastLinebreak
		Else
			charsAfterLastLinebreak = 0
		End If
		
		Return New SCodeInfo( ..
			Self.length + nextInfo.length, ..
			Self.linebreakCount + nextInfo.linebreakCount, ..
			charsAfterLastLinebreak + nextInfo.charsAfterLastLinebreak ..
		)
	End Method
	
	Method ToString:String()
		Return "length: " + length + ..
		       ", linebreakCount: " + linebreakCount + ..
		       ", charsAfterLastLinebreak: " + charsAfterLastLinebreak
	End Method
End Struct
