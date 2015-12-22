#include "../src/yolib.h"

/********************INCLUDE END******************/
struct _log;
struct _gcd;
struct _compute;
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
struct _Universal;
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

/********************DECLARATION END*****************/

/****************TYPE DECLARATION ENDED************/
struct _gcd {
static int eval(tr1::shared_ptr<Universal>, int a, int b) {
if(false) {}
else if ((a == 0)) {
return b;
}

return _gcd::eval(DUMMY_SELF, (b % a), a);
}


};

struct _compute {
static int eval(tr1::shared_ptr<Universal>) {
auto a = 84;

auto b = 42;

return _gcd::eval(DUMMY_SELF, a, b);
}


};


/**************FUNCTION DECLARATION ENDED***********/
int main() {
_log::eval(DUMMY_SELF, _compute::eval(DUMMY_SELF));
return 0;
}