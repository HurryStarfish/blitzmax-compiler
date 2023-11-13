SuperStrict



Function Pluralize:String(str:String, pluralize:Int = True)
	' very basic attempt
	If Not str Then Return ""
	If Not pluralize Then Return str
	Local last1:String = str[str.length - 1..].ToLower()
	Local last2:String = str[str.length - 2..].ToLower()
	Select True
		Case last1 = "s" Or last1 = "x" Or last1 = "z" Or last2 = "ss" Or last2 = "sh" Or last2 = "ch"
			Return str + "es"
		Case last2 = "ay" Or last2 = "ey" Or last2 = "iy" Or last2 = "oy" Or last2 = "uy"
			Return str + "s"
		Case last1 = "y"
			Return str[..str.length - 1] + "ies"
		Case last2 = "is"
			Return str[..str.length - 1] + "es"
		Case last2 = "on"
			Return str[..str.length - 1] + "a"
		Default
			Return str + "s"
	End Select
End Function

Function WithIndefiniteArticle:String(str:String)
	If Not str Then Return ""
	Function IsVowel:Int(char:Int)
		Select char
			Case "a"[0], "e"[0], "i"[0], "o"[0], "u"[0], "A"[0], "E"[0], "I"[0], "O"[0], "U"[0] Return True
			Default Return False
		End Select
	End Function
	If IsVowel(str[0]) Then Return "an " + str Else Return "a " + str
End Function
