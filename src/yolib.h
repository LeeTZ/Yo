#include "/usr/local/include/libopenshot/OpenShot.h"
#include <dirent.h>
#include <libconfig.h++>
#include <iostream>
#include <fstream>
#include <sys/types.h>
#include <errno.h>
#include <vector>
#include <string>
#include <algorithm>
#include <memory>

using namespace std;
using namespace libconfig;
/*Global output configuration*/
int V_FPS=24;
int V_WIDTH=640;
int V_HEIGHT=360;
int V_PIXEL_RATIO=1;
int V_BIT_RATE=240000;

struct _Pixel{
	int R;
	int G;
	int B;
	_Pixel(int r, int g, int b){
		R =r; G =g; B=b;
	}
};


void readConfig(){
	Config cfg;               /*Returns all parameters in this structure */
    try{
		cfg.readFile("config.ini");
	}
	catch(const FileIOException &fioex)
	{
		std::cerr << "No config found, use default." << std::endl;
		return;
	}
	catch(const ParseException &pex)
	{
		std::cerr << "Parse error at " << pex.getFile() << ":" << pex.getLine()
		          << " - " << pex.getError() << std::endl;
		return;
	}

	// Get the store name.
	try{
		int tmp = cfg.lookup("fps");
		V_FPS = tmp;
	}
	catch(const SettingNotFoundException &nfex){}
	try{
		int tmp = cfg.lookup("width");
		V_WIDTH = tmp;
	}
	catch(const SettingNotFoundException &nfex){}
	try{
		int tmp = cfg.lookup("height");
		V_HEIGHT = tmp;
	}
	catch(const SettingNotFoundException &nfex){}
	try{
		int tmp = cfg.lookup("pixel_ratio");
		V_PIXEL_RATIO = tmp;
	}
	catch(const SettingNotFoundException &nfex){}
	try{
		int tmp = cfg.lookup("bit_rate");
		V_BIT_RATE = tmp;
	}
	catch(const SettingNotFoundException &nfex){}
}

template<typename T>
void pop_front(std::vector<T>& vec)
{
    assert(!vec.empty());
    vec.erase(vec.begin());
}


int getdir (string dir, vector<string> &files)
{
    DIR *dp;
    struct dirent *dirp;
    if((dp  = opendir(dir.c_str())) == NULL) {
        cout << "Error(" << errno << ") opening " << dir << endl;
        return errno;
    }

    while ((dirp = readdir(dp)) != NULL) {
        files.push_back(string(dirp->d_name));
    }
    closedir(dp);
    // sort by filename
    sort(files.begin(),files.end());	
    //remove . and ..
    pop_front(files);
    pop_front(files);
    return 0;
}

string getextension(string filename){
	int idx = filename.rfind('.');
	string extension = "";
	if (idx != string::npos){
		extension = filename.substr(idx + 1);
	}
	transform(extension.begin(), extension.end(), extension.begin(), ::tolower);
	return extension;
}

int isVideo(string filename){
	string imagetype[5] = {"jpg","jpeg","png","bmp","gif"};
	string videotype[11] = {"webm","f4v","mov","rmvb","mp4","rm","wmv","avi","flv","3gp","mkv"};
	string extension = getextension(filename);
	for (int i = 0; i < 5; i++)
		if (extension == imagetype[i])
			return 0;
	for (int i = 0; i < 11; i++)
		if (extension == videotype[i])
			return 1;
	return -1;
}


struct Universal {};
using std::string;
struct _Clip : Universal  {
	
	tr1::shared_ptr<Timeline> __instance__;

	_Clip(tr1::shared_ptr<Timeline> tl) {
		__instance__ = tl;
	}	
	static tr1::shared_ptr<_Clip> eval(tr1::shared_ptr<Universal> obj, string fileName);

};

tr1::shared_ptr<_Clip> fromTimeline(tr1::shared_ptr<Timeline> tlptr) {
	return tr1::shared_ptr<_Clip>(new _Clip(tlptr));
}

/*create a clip from a file, 
  yo prog:
  r = Clip("output.webm") 	
  generate as:
  tr1::shared_ptr<Timeline> r = createClip("output.webm");
*/

std::string logClip(tr1::shared_ptr<_Clip> _clip){
	list<Clip*> cliplists = _clip->__instance__->Clips();
	for (std::list<Clip*>::const_iterator iterator = cliplists.begin(), end = cliplists.end(); iterator != end; ++iterator){
		return (*iterator)->Json();
	}
}

tr1::shared_ptr<_Clip> createClip(string filename){
	//check the file type, an image or a video
	int filetype = isVideo(filename);
	if (filetype == 1){
		FFmpegReader* reader = new FFmpegReader(filename);		
		// essential: Open the reader otherwise you cannot read
		reader->Open();
		Clip* clip = new Clip(reader);
		tr1::shared_ptr<Timeline> r (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));
		r->AddClip(clip);
		r->Open();
		reader->Close();
		return fromTimeline(r);
	}else if (filetype == 0){
		ImageReader* reader = new ImageReader(filename);
		reader->Open();
		Clip* clip = new Clip(reader);
		tr1::shared_ptr<Timeline> r (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));	
		r->AddClip(clip);
		r->Open();
		reader->Close();
		return fromTimeline(r);
	}else{
		exit(-1);
	}

}
/* create clips from a dir
   yo:prog:
   clips = readclips("dirname/")
   generate as:
   std::vector<tr1::shared_ptr<Timeline>> clips = createClips("dir/");
*/

tr1::shared_ptr<std::vector<tr1::shared_ptr<_Clip>>> createClips(string dirname){
	//read files into Filenames
	auto Filenames = vector<string>();
	getdir(dirname,Filenames);

    auto res = tr1::shared_ptr<vector<tr1::shared_ptr<_Clip>> >(new vector<tr1::shared_ptr<_Clip>>());
	
    //auto res = tr1::shared_ptr<std::vector<tr1::shared_ptr<_Clip>>>();
	int len = Filenames.size();
	for (int i = 0; i < len; i++){
		//std::cout << Filenames[i] << endl;	
		// form clips
		res->push_back(createClip(dirname + Filenames[i]));
	}
	return res;
}

/* clips addition
   yo:prog:
   clip = clip1 + clip2
   generate as:
   tr1::shared_ptr<Timeline> clip = addClip(clip1,clip2);
*/

tr1::shared_ptr<_Clip> addClip(tr1::shared_ptr<_Clip> lop, tr1::shared_ptr<_Clip> rop){
	list<Clip*> leftlists = lop->__instance__->Clips();
	list<Clip*> rightlists = rop->__instance__->Clips();
	tr1::shared_ptr<Timeline> res (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));

	double maxpos = 0.0;
	for (std::list<Clip*>::const_iterator iterator = leftlists.begin(), end = leftlists.end(); iterator != end; ++iterator) {
    	// directly add clip to the timeline
    	res->AddClip(*iterator);
    	// find out how much time clip2 should shift
    	if ((*iterator)->Position() + ((*iterator)->End()- (*iterator)->Start()) > maxpos){
    		maxpos = (*iterator)->Position()+ ((*iterator)->End() - (*iterator)->Start());
    	}    	
	}

	for (std::list<Clip*>::const_iterator iterator = rightlists.begin(), end = rightlists.end(); iterator != end; ++iterator) {
    	// shift clip2
    	(*iterator)->Position((*iterator)->Position() + maxpos);
   		res->AddClip(*iterator);
	}
	res->Open();
	return fromTimeline(res);
}

/* clips layering
   yo:prog:
   clip = clip1 ^ clip2 @ 1.0
   generate as:
   tr1::shared_ptr<Timeline> clip = layerClip(clip1,clip2,1.0);
*/

tr1::shared_ptr<_Clip> layerClip(tr1::shared_ptr<_Clip> bottom, tr1::shared_ptr<_Clip> top, double shifttime){
	list<Clip*>	bottomlists = bottom->__instance__->Clips();
	list<Clip*> toplists = top->__instance__->Clips();
	tr1::shared_ptr<Timeline> res(new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));
	
	int maxlayer = 0;
	for (std::list<Clip*>::const_iterator iterator = bottomlists.begin(), end = bottomlists.end(); iterator != end; ++iterator){
		//std::cout << (*iterator)->Layer() << std::endl;
		if ((*iterator)->Layer() > maxlayer){
			maxlayer = (*iterator)->Layer();
		}
		res->AddClip(*iterator);
	}
	
	for (std::list<Clip*>::const_iterator iterator = toplists.begin(), end = toplists.end(); iterator != end; ++iterator) {
    	(*iterator)->Layer((*iterator)->Layer() + maxlayer);
    	(*iterator)->Position((*iterator)->Position() + shifttime);
    	res->AddClip(*iterator);
	}
	res->Open();
	return fromTimeline(res);
}

tr1::shared_ptr<_Clip> layerClip(tr1::shared_ptr<_Clip> bottom, tr1::shared_ptr<_Clip> top, int shiftframe){
	double shifttime = double(shiftframe) / V_FPS;
	return layerClip(bottom,top,shifttime);
}
/* write clips to a file
   yo:prog:
   write(clip,"filename.mp4")
   generate as:
   writeClips(clip,"filename.mp4");
*/

void writeClips(tr1::shared_ptr<_Clip> _clip, string filename){
	auto clip = _clip->__instance__;
	//std::cout << (clip)->Json() << std::endl;
	FFmpegWriter w(filename);
	string extension = getextension(filename);
	w.SetAudioOptions(true, "libvorbis", 44100, 2, LAYOUT_STEREO, 188000);
	//if (extension == "webm")
	w.SetVideoOptions(true, "libvpx", Fraction(V_FPS,1), V_WIDTH, V_HEIGHT, Fraction(V_PIXEL_RATIO,1), false, false, V_BIT_RATE);
	
	w.Open();
	// calculate the ending time
	double totaltime = 0;
	list<Clip*> lists = clip->Clips();
	for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
    	if ((*iterator)->Position() + ((*iterator)->End() - (*iterator)->Start()) > totaltime){
    		totaltime = (*iterator)->Position()+ ((*iterator)->End() - (*iterator)->Start());
    	}   
    	//std::cout << (*iterator)->Position() << " " << (*iterator)->Start() << " " << (*iterator)->End() << std::endl;
		//std::cout << (*iterator)->Json() << endl;
	}
	int totalframe = int(V_FPS * totaltime) + 1;
	std::cout << "Rendering... Totalframe:" << totalframe << std::endl;
	w.WriteFrame(&(*clip), 1, totalframe);
	w.Close();
}

/*
	clipRange argument is double (seconds)
	yo:prog:
    a = clip[2.0:3.0]
    generate as:
    a = clipRange(clip,2.0,3.0);
*/

tr1::shared_ptr<_Clip> clipRange(tr1::shared_ptr<_Clip> _clip,double starttime, double endtime){
	assert(starttime <= endtime);
	list<Clip*>	cliplists = _clip->__instance__->Clips();
	tr1::shared_ptr<Timeline> res(new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));
	for (std::list<Clip*>::const_iterator iterator = cliplists.begin(), end = cliplists.end(); iterator != end; ++iterator){
		bool modifyhead = false;
		bool modifyend = false;
		if ((*iterator)->Position() < starttime){
			if ((*iterator)->Position() + ((*iterator)->End() - (*iterator)->Start()) < starttime){
				continue;
			}
			modifyhead = true;
		}
		if ((*iterator)->Position() < endtime){
			if ((*iterator)->Position() + ((*iterator)-> End() - (*iterator)->Start()) > endtime){
				modifyend = true;
			}
		}
		if ((*iterator)->Position() >= endtime){
			continue;
		}
		if (modifyhead){
			(*iterator)->Start(starttime - (*iterator)->Position());
		}
		if (modifyend){
			(*iterator)->End(endtime - (*iterator)->Position());
		}
		res->AddClip(*iterator);
	}
	res->Open();
	return fromTimeline(res);
}

/*
	clipRange argument is integer (frames)
	yo:prog:
    a = clip[24:48]
    generate as:
    a = clipRange(clip,24,48);
*/

tr1::shared_ptr<_Clip> clipRange(tr1::shared_ptr<_Clip> _clip,int startframe, int endframe){
	double starttime = double (startframe) / V_FPS;
	double endtime = double (endframe) / V_FPS;
	return clipRange(_clip, starttime, endtime);
}

/*
	clipIndex argument is integer (frame)
	yo:prog:
    a = clip[24]
    generate as:
    a = clipIndex(clip,24);
*/

tr1::shared_ptr<Frame> clipIndex(tr1::shared_ptr<_Clip> _clip,int frame){
	return _clip->__instance__->GetFrame(frame);
}


/*
	clipIndex argument is double (time)
	yo:prog:
    a = clip[2.4]
    generate as:
    a = clipIndex(clip,2.4);
*/

tr1::shared_ptr<Frame> clipIndex(tr1::shared_ptr<_Clip> _clip,double frametime){
	int frame = int(frametime * V_FPS);
	return _clip->__instance__->GetFrame(frame);
}


/*
return a matrix of 	
pixel : R G B{}
pixel = getpixel(clip,frame,i,j)
*/

tr1::shared_ptr<_Pixel> getPixel(tr1::shared_ptr<_Clip> _clip, int frame, int x, int y){	
	tr1::shared_ptr<Frame> f = _clip->__instance__->GetFrame(frame);
	const unsigned char* pixels = f->GetPixels(x);
	tr1::shared_ptr<_Pixel> res(new _Pixel(int(pixels[3 * y]),int(pixels[3 * y + 1]),int(pixels[3 * y + 2])));	
	return res;	
}

tr1::shared_ptr<_Pixel> getPixel(tr1::shared_ptr<_Clip> _clip, double frametime, int x, int y){
	int frame = int (frametime*V_FPS);
	return getPixel(_clip,frame,x,y);	
}

/*
set pixel
no use now
*/

template< typename T >
std::string int_to_hex( T i )
{
  std::stringstream stream;
  stream << std::setfill ('0') << std::setw(2) 
         << std::hex << i;
  return stream.str();
}

/*
void setPixel(tr1::shared_ptr<_Clip> _clip, int frame, int x, int y, pixel res){
	tr1::shared_ptr<Frame> f = _clip->__instance__->GetFrame(frame);
	const unsigned char* pixels = f->GetPixels();
	unsigned char* dest;
	strcpy(dest,pixels);
	int index = x * f->GetWidth() + y;
	dest[3*index] = char(res.R);
	dest[3*index + 1] = char(res.G);
	dest[3*index + 2] = char(res.B);
	const unsigned char* p = (const char*) dest;
	f->AddImage(f->GetWidth(), f->GetHeight(), 4, QImage::Format_RGBA8888, p);
	//string color = "#" + int_to_hex(p.R) + int_to_hex(p.G) + int_to_hex(p.B);
	//cout << color << endl;
	//f->AddColor(x,y,color);
}
*/

/* write clips to a file
   yo:prog:
   clip.alpha@1 = 255
   generate as:
   setProperty(clip,"alpha",1,255);
   
*/

void setProperty(tr1::shared_ptr<_Clip> _clip, string attname, int frame, double value){
	list<Clip*> lists = _clip->__instance__->Clips();
	if (attname == "alpha"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	//double keytime = double(frame) / V_FPS + (*iterator)->Start();
	    	(*iterator)->alpha.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	    	//std::cout << (*iterator)->Position() << " " << (*iterator)->Start() <<" " << (*iterator)->End() << " " << keytime <<" " << value << std::endl;
		}
	}if (attname == "location_x"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->location_x.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	   	}
	}if (attname == "location_y"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->location_y.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	   	}
	}if (attname == "scale_x"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->scale_x.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	   	}
	}if (attname == "scale_y"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->scale_y.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	   	}
	}if (attname == "rotate"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->rotation.AddPoint(frame + (*iterator)->Start() * V_FPS,value);
	   	}
	}

	// add more..
}

template<typename T>
tr1::shared_ptr<vector<T>> slice_array(tr1::shared_ptr<vector<T>> vec, int start, int end) {
	assert(end <= vec->size());
	tr1::shared_ptr<vector<T>> n_vec;
	while (start < end) {
		n_vec->push_back(*vec[start]);
		++start;
	}
	return n_vec;
}

template<class T, class... Tail>
auto make_array(T head, Tail... tail) -> std::array<T, 1 + sizeof...(Tail)>
{
     std::array<T, 1 + sizeof...(Tail)> a = { head, tail ... };
     return a;
}

template<typename T>
std::shared_ptr<std::vector<T>> create_array(T arr[], n) {
	auto vec = std::shared_ptr<std::vector<T>>(new std::vector<T>());
	for (int i=0; i<n; ++i)
		vec->push_back(arr[i]);
	return vec;
}

template<typename T>
std::shared_ptr<std::vector<T>> create_array() {
	auto vec = std::shared_ptr<std::vector<T>>(new std::vector<T>());
	return vec;
}



tr1::shared_ptr<Universal> DUMMY_SELF;

struct _log
{
	template <typename T>
	static void eval (tr1::shared_ptr<Universal> obj, T str) {
		std::cout << str;
	}
};



tr1::shared_ptr<_Clip> _Clip::eval(tr1::shared_ptr<Universal> obj, string fileName) {
		return createClip(fileName);
	}


struct _Clip_save : Universal {
	static void eval(tr1::shared_ptr<_Clip> _clip, string fileName) {
		writeClips(_clip, fileName);
	}
};

struct _Clip_log : Universal {
	static void eval(tr1::shared_ptr<_Clip> _clip) {
		std::cout << logClip(_clip) << std::endl;
	}

	static void eval(tr1::shared_ptr<_Clip> _clip, string fileName) {
		std::fstream fout(fileName);
		fout << logClip(_clip);
	}
};

template<typename T>
struct _Array_add {
	static tr1::shared_ptr<std::vector<T>> eval(tr1::shared_ptr<std::vector<tr1::shared_ptr<T>>> arr, tr1::shared_ptr<T> obj) {
		arr->push_back(obj);
		return arr;
	}

	static tr1::shared_ptr<std::vector<T>> eval(tr1::shared_ptr<std::vector<T>> arr, T obj) {
		arr->push_back(obj);
		return arr;
	}
};

template<typename T>
struct _Array_length {
	static int length(tr1::shared_ptr<std::vector<tr1::shared_ptr<T>>> arr) {
		return arr->size();
	}

	static int eval(tr1::shared_ptr<std::vector<T>> arr) {
		return arr->size();
	}
};

