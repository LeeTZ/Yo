#include <memory>
#include <iostream>

struct Universal {};

std::shared_ptr<Universal> DUMMY_SELF;

template <typename T>
void LOG (std::shared_ptr<Universal> obj, T str) {
	std::cout << str;
}


