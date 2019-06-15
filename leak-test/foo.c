#include <HsFFI.h>

HsStablePtr myobj;

void setMyObject(HsStablePtr ptr) {
	myobj = ptr;
}
