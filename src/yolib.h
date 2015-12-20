#include <memory>
#include <iostream>
#include <string>
using namespace std;

struct Universal {};

std::shared_ptr<Universal> DUMMY_SELF;

template <typename T>
void LOG (std::shared_ptr<Universal> obj, T str) {
	std::cout << str;
}


