SuperStrict
Framework BRL.Blitz
Import "WeakReference.c"



Private ' <- this doesn't work if TWeakReference is generic (bug with generics)
Extern
	Function bbRegisterWeakPtr(weakPtr:Byte Ptr Var, obj:Object)
	Function bbUnregisterWeakPtr(weakPtr:Byte Ptr Var)
	Function bbWeakPtrGetObject:Object(weakPtr:Byte Ptr Var)
End Extern

Public
Type TWeakReference Final'<T>
	Private
	Field objPtr:Byte Ptr
	
	Method New() End Method
	
	Public
	Method New(obj:Object)
		? Debug
		If Not obj Then Throw New TNullObjectException
		?
		bbRegisterWeakPtr objPtr, obj
	End Method
	
	Method Delete()
		bbUnregisterWeakPtr objPtr
	End Method
	
	Method Get:Object()'T
		Return bbWeakPtrGetObject(objPtr)
	End Method
End Type
