#include <windows.h>
#include <xlcall.h>
#include <framewrk.h>
#include <stdarg.h>

int xlAutoOpen() {
	static XLOPER12 xDLL;
	Excel12f(xlGetName, &xDLL, 0);

	XLOPER12 result;

	Excel12(xlfRegister, &result, 5,
		(LPXLOPER12)&xDLL,
		(LPXLOPER12)TempStr12(L"Square"),
		(LPXLOPER12)TempStr12(L"BB"),
		(LPXLOPER12)TempStr12(L"Square"),
		(LPXLOPER12)TempStr12(L"x"));

	return 1;
}

int xlAutoClose() {
	return 1;
}

XLOPER* xlAddInManagerInfo(XLOPER* _) {
	static XLOPER result;
	result.xltype = xltypeStr;
	result.val.str = "7FxSheet";
	return &result;
}

const double square(const double x) {
	return x * x;
}