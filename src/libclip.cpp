#include "/usr/local/include/libopenshot/OpenShot.h"
#include <dirent.h>
#include <libconfig.h++>
#include <iostream>
#include <sys/types.h>
#include <errno.h>
#include <vector>
#include <string>
#include <algorithm>
using namespace std;
using namespace libconfig;
/*Global output configuration*/
int V_FPS=24;
int V_WIDTH=1280;
int V_HEIGHT=720;
int V_PIXEL_RATIO=1;
int V_BIT_RATE=2500000;

struct pixel{
	int R;
	int G;
	int B;
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

/*create a clip from a file, 
  yo prog:
  r = Clip("output.webm") 	
  generate as:
  tr1::shared_ptr<Timeline> r = createClip("output.webm");
*/

tr1::shared_ptr<Timeline> createClip(string filename){
	//check the file type, an image or a video
	int filetype = isVideo(filename);
	if (filetype == 1){
		FFmpegReader* reader = new FFmpegReader(filename);		
		// essential: Open the reader otherwise you cannot read
		reader->Open();
		Clip* clip = new Clip(reader);
		// TODO : This infomation should be saved in an config file 
		tr1::shared_ptr<Timeline> r (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));
		r->AddClip(clip);
		r->Open();
		reader->Close();
		return r;
	}else if (filetype == 0){
		ImageReader* reader = new ImageReader(filename);
		reader->Open();
		Clip* clip = new Clip(reader);
		// TODO : This infomation should be saved in an config file 
		tr1::shared_ptr<Timeline> r (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));	
		r->AddClip(clip);
		r->Open();
		reader->Close();
		return r;
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

std::vector<tr1::shared_ptr<Timeline>> createClips(string dirname){
	//read files into Filenames
	std::vector<std::string> Filenames = vector<string>();
	getdir(dirname,Filenames);

	std::vector<tr1::shared_ptr<Timeline>> res;
	int len = Filenames.size();
	for (int i = 0; i < len; i++){
		//std::cout << Filenames[i] << endl;	
		// form clips
		res.push_back(createClip(dirname + Filenames[i]));
	}
	return res;
}

/* clips addition
   yo:prog:
   clip = clip1 + clip2
   generate as:
   tr1::shared_ptr<Timeline> clip = addClip(clip1,clip2);
*/

tr1::shared_ptr<Timeline> addClip(tr1::shared_ptr<Timeline> lop, tr1::shared_ptr<Timeline> rop){
	list<Clip*> leftlists = lop->Clips();
	list<Clip*> rightlists = rop->Clips();
	tr1::shared_ptr<Timeline> res (new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));

	double maxpos = 0.0;
	for (std::list<Clip*>::const_iterator iterator = leftlists.begin(), end = leftlists.end(); iterator != end; ++iterator) {
    	// directly add clip to the timeline
    	res->AddClip(*iterator);
    	// find out how much time clip2 should shift
    	if ((*iterator)->Position() + (*iterator)->End() > maxpos){
    		maxpos = (*iterator)->Position()+ (*iterator)->End();
    	}    	
	}

	for (std::list<Clip*>::const_iterator iterator = rightlists.begin(), end = rightlists.end(); iterator != end; ++iterator) {
    	// shift clip2
    	(*iterator)->Position((*iterator)->Position() + maxpos);
   		res->AddClip(*iterator);
	}
	res->Open();
	return res;
}

/* clips layering
   yo:prog:
   clip = clip1 ^ clip2 @ 1.0
   generate as:
   tr1::shared_ptr<Timeline> clip = layerClip(clip1,clip2,1.0);
*/

tr1::shared_ptr<Timeline> layerClip(tr1::shared_ptr<Timeline> bottom, tr1::shared_ptr<Timeline> top, double shifttime){
	list<Clip*>	bottomlists = bottom->Clips();
	list<Clip*> toplists = top->Clips();
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
	return res;
}

/* write clips to a file
   yo:prog:
   write(clip,"filename.mp4")
   generate as:
   writeClips(clip,"filename.mp4");
*/

void writeClips(tr1::shared_ptr<Timeline> clip, string filename){
	FFmpegWriter w(filename);
	string extension = getextension(filename);
	if (extension == "webm")
		w.SetVideoOptions(true, "libvpx", Fraction(V_FPS,1), V_WIDTH, V_HEIGHT, Fraction(V_PIXEL_RATIO,1), false, false, V_BIT_RATE);
	w.Open();
	std::cout << clip->info.video_length << endl;
	// calculate the ending time
	double totaltime = 0;
	list<Clip*> lists = clip->Clips();
	for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
    	if ((*iterator)->Position() + (*iterator)->End() > totaltime){
    		totaltime = (*iterator)->Position()+ (*iterator)->End();
    	}    	
	}
	int totalframe = 24 * (int(totaltime) + 1);
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

tr1::shared_ptr<Timeline> clipRange(tr1::shared_ptr<Timeline> clip,double starttime, double endtime){
	assert(starttime <= endtime);
	list<Clip*>	cliplists = clip->Clips();
	tr1::shared_ptr<Timeline> res(new Timeline(V_WIDTH, V_HEIGHT, Fraction(V_FPS, 1), 44100, 2, LAYOUT_STEREO));
	for (std::list<Clip*>::const_iterator iterator = cliplists.begin(), end = cliplists.end(); iterator != end; ++iterator){
		if ((*iterator)->Position() < starttime){
			if ((*iterator)->Position() + (*iterator)->End() < starttime){
				continue;
			}
			(*iterator)->Start(starttime - (*iterator)->Position());
		}

		if ((*iterator)->Position() >= starttime && (*iterator)->Position() < endtime){
			if ((*iterator)->Position() + (*iterator)-> End() > endtime){
				(*iterator)->End(endtime - (*iterator)->Position());
			}
		}
		if ((*iterator)->Position() >= endtime){
			continue;
		}
		res->AddClip(*iterator);
	}
	res->Open();
	return res;
}

/*
	clipRange argument is integer (frames)
	yo:prog:
    a = clip[24:48]
    generate as:
    a = clipRange(clip,24,48);
*/

tr1::shared_ptr<Timeline> clipRange(tr1::shared_ptr<Timeline> clip,int startframe, int endframe){
	double starttime = startframe / V_FPS;
	double endtime = endframe / V_FPS;
	return clipRange(clip, starttime, endtime);
}

/*
	clipIndex argument is integer (frame)
	yo:prog:
    a = clip[24]
    generate as:
    a = clipIndex(clip,24);
*/

tr1::shared_ptr<Frame> clipIndex(tr1::shared_ptr<Timeline> clip,int frame){
	return clip->GetFrame(frame);
}


/*
	clipIndex argument is double (time)
	yo:prog:
    a = clip[2.4]
    generate as:
    a = clipIndex(clip,2.4);
*/

tr1::shared_ptr<Frame> clipIndex(tr1::shared_ptr<Timeline> clip,double frametime){
	int frame = int(frametime * V_FPS);
	return clip->GetFrame(frame);
}


/*
return a matrix of 
pixel : R G B{}
pixel = getpixel(clip,frame,i,j)
*/

pixel getPixel(tr1::shared_ptr<Timeline> clip, int frame, int x, int y){
	tr1::shared_ptr<Frame> f = clip->GetFrame(frame);
	const unsigned char* pixels = f->GetPixels();
	int index = x * f->GetWidth() + y;
	pixel res;
	res.R = int(pixels[3 * index]);
	res.G = int(pixels[3 * index + 1]);
	res.B = int(pixels[3 * index + 2]);
	return res;	
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


void setPixel(tr1::shared_ptr<Timeline> clip, int frame, int x, int y, pixel p){
	tr1::shared_ptr<Frame> f = clip->GetFrame(frame);
	string color = "#" + int_to_hex(p.R) + int_to_hex(p.G) + int_to_hex(p.B);
	cout << color << endl;
	f->AddColor(x,y,color);
}

/* write clips to a file
   yo:prog:
   clip.alpha@1 = 255
   generate as:
   setProperty(clip,"alpha",1,255);
   
*/

void setProperty(tr1::shared_ptr<Timeline> clip, string attname, int second, double value){
	list<Clip*> lists = clip->Clips();
	if (attname == "alpha"){
		for (std::list<Clip*>::const_iterator iterator = lists.begin(), end = lists.end(); iterator != end; ++iterator) {
	    	(*iterator)->alpha.AddPoint(second,value);    	
		}
	}// add more..
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

/*
template<typename T>
tr1::shared_ptr<vector<T>> create_array(tr1::shared_ptr<T>[] elements)
{
	tr1::shared_ptr<vector<T>> n_vec;
	for (auto e : elements)
		n_vec.push_back(e);
	return n_vec;
}
*/
void logClip(tr1::shared_ptr<Timeline> clip){
	list<Clip*> cliplists = clip->Clips();
	for (std::list<Clip*>::const_iterator iterator = cliplists.begin(), end = cliplists.end(); iterator != end; ++iterator){
		std::cout << (*iterator)->Json() << std::endl;
	}
}


int main(){
	readConfig();
	//tr1::shared_ptr<Timeline> r = createClip("output.webm");
	//r->Open();
	std::vector<tr1::shared_ptr<Timeline>> clips = createClips("dir/");
	tr1::shared_ptr<Timeline> clip = layerClip(clips[0],clips[1],1.0);
	logClip(clips[0]);
	//writeClips(clip,"o.webm");
	/*tr1::shared_ptr<Frame> f = clip->GetFrame(30);
	pixel p;
	p.R = 255;
	p.G = 255;
	p.B = 255;
	setPixel(clip, 30, 100, 100, p);
	*/
	//pixel tmp = getPixel(clip,30,1,1);
	//std::cout << "R:" << tmp.R << " G:" << tmp.G << " B:" << tmp.B << std::endl;
	//f->Display();
	//r->Close();
}

/*
write a global error handler which process all the errors generated by each part of the program
if a python error raises, write "Preprocess error"
if a parsing error raises, write "Syntax error"
if a c++ error raises, write "Runtime error"
*/