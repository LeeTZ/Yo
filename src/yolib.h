#include <memory>
#include <iostream>

struct Universal {};
using std::string;
using std::tr1::shared_ptr;

shared_ptr<Universal> DUMMY_SELF;

template <typename T>
void LOG (std::shared_ptr<Universal> obj, T str) {
	std::cout << str;
}


struct Clip : Universal {

	static shared_ptr<Clip> eval(string fileName) {
		return createClip(filename);
	}

	static void save(shared_ptr<Clip> clip, string fileName) {
		return writeClips(clip, filename);
	}
};