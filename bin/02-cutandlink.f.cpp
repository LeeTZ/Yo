#include "../src/yolib.h"

/********************INCLUDE END******************/
struct _log;
struct ___Sub;
struct ___Or;
struct ___Neq;
struct ___Mult;
struct ___Mod;
struct ___Less;
struct ___Leq;
struct ___Gt;
struct ___Geq;
struct ___Equal;
struct ___Div;
struct ___And;
struct ___Add;
struct _Void;
struct _String;
struct _Pixel;
struct _Int;
struct _Frame;
struct _Double;
struct _Clip_save;
struct _Clip_log;
struct _Clip;
struct _Bool;
struct _Attribute;
struct _ArrayElementT;
struct _Array;

/********************DECLARATION END*****************/

/****************TYPE DECLARATION ENDED************/

/**************FUNCTION DECLARATION ENDED***********/
int main() {
auto a = _Clip::eval(DUMMY_SELF, "helloworld.webm");
auto b = _Clip::eval(DUMMY_SELF, "helloworld2.webm");
auto e = clipRange(a, 0., (1.6 - 0.1));
auto f = clipRange(b, 0, (1 + (24 * 2)));
auto d = addClip(e, f);
_Clip_save::eval(d, "hello-combined.webm");
return 0;
}