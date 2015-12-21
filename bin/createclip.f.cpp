#include "yolib.h"

/********************INCLUDE END******************/
struct ___Sub;
struct ___Or;
struct ___Neq;
struct ___Mult;
struct ___Mod;
struct ___Less;
struct ___Leq;
struct ___Gt;
struct ___Geq;
struct ___Eq;
struct ___Div;
struct ___And;
struct ___Add;
struct _Void;
struct _String;
struct _Log;
struct _Int;
struct _Frame;
struct _Double;
struct _Clip_save;
struct _Clip;
struct _Bool;
struct _Attribute;

/********************DECLARATION END*****************/

/****************TYPE DECLARATION ENDED************/

/**************FUNCTION DECLARATION ENDED***********/
int main() {
auto c = _Clip::eval(DUMMY_SELF, "helloworld.webm");
_Clip_save::eval(c, "abc.webm");
return 0;
}