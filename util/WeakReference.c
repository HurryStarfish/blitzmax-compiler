#include <blitz.h>

void bbRegisterWeakPtr(BBOBJECT* weakPtr, BBOBJECT obj) {
	*weakPtr = obj;
	switch (GC_general_register_disappearing_link(weakPtr, obj)) {
		case GC_SUCCESS:
			break;
		case GC_DUPLICATE:
		case GC_NO_MEMORY:
		case GC_UNIMPLEMENTED:
		default:
			bbExThrowCString("Weak pointer creation failed");
	}
}

void bbUnregisterWeakPtr(BBOBJECT* weakPtr) {
	GC_unregister_disappearing_link(weakPtr);
}

static BBOBJECT GC_CALLBACK DerefWeakPtr(BBOBJECT* weakPtr) {
	return *weakPtr;
}

BBOBJECT bbWeakPtrGetObject(BBOBJECT* weakPtr) {
	BBOBJECT obj = GC_call_with_alloc_lock(DerefWeakPtr, weakPtr);
	return obj ? obj : &bbNullObject;
}
