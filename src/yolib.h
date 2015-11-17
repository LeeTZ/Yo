#include <memory>
#include <iostream>

struct Universal {};

std::shared_ptr<Universal> DUMMY_SELF;

template<T>
void LOG (T str) {
	std::cout << str;
}