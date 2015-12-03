#include "/usr/local/include/libopenshot/OpenShot.h"

void displayAFrame(){
	// Create a reader for a video
	FFmpegReader r("helloworld.webm");
	r.Open(); // Open the reader
	// Get frame number 1 from the video
	tr1::shared_ptr<Frame> f = r.GetFrame(1);
	f->Display(); // Display the frame on the screen
	r.Close();    // Close the reader
}

void createAVideo(){
	Clip clip_video("helloworld.webm");
	
	clip_video.Layer(0);
	clip_video.Position(0.0);
	Timeline r9(640, 480, Fraction(30, 1), 44100, 2, LAYOUT_STEREO);
	r9.debug = false;
	// Add clips
	r9.AddClip(&clip_video);
	FFmpegWriter w("output1.webm");

	w.SetVideoOptions(true, "libvpx", Fraction(24,1), 1280, 720, Fraction(1,1), false, false, 3000000);

		
	// Set options
	//w.SetAudioOptions(true, "libvorbis", 44100, 2, LAYOUT_STEREO, 188000);
	//w.SetVideoOptions(true, "libvpx", Fraction(24,1), 1280, 720, Fraction(1,1), false, false, 3000000);

	// Open writer
	w.Open();
	w.PrepareStreams();
	w.WriteHeader();
	// Write some frames
	w.WriteFrame(&r9, 24, 50);
	// Close writer & reader
	w.Close(); 
	r9.Close();
}

void createASample(){

// Create a reader for a video
	FFmpegReader r("helloworld.webm");
	r.Open(); // Open thetarget_ reader
	// Create a writer (which will create a WebM video)
	FFmpegWriter w("output.webm");
	// Set options
	w.SetAudioOptions(true, "libvorbis", 44100, 2, LAYOUT_STEREO, 128000); // Sample Rate: 44100, Channels: 2, Bitrate: 128000
	w.SetVideoOptions(true, "libvpx", openshot::Fraction(24,1), 960, 480, openshot::Fraction(1,1), false, false, 300000); // FPS: 24, Size: 720x480, Pixel Ratio: 1/1, Bitrate: 300000
	// Open the writer
	w.Open();
	// Write all frames from the reader
	w.WriteFrame(&r, 10, r.info.video_length);
	// Close the reader & writer
	
	FFmpegReader r2("helloworld2.webm");
	r2.Open(); // Open thetarget_ reader
	w.WriteFrame(&r2, 1, r2.info.video_length / 2);
	/*
	// Create a reader for a video
	ImageReader r1("subtitle.jpg");
	r1.Open(); // Open the reader
	// Get frame number 1 from the video
	tr1::shared_ptr<Frame> f = r1.GetFrame(1);
	for (int i = 0; i < 24; i ++)
		w.WriteFrame(f);
*/
	w.Close();
	r.Close();
}

void createClips(){
	// Create some clips
	Clip c1(new ImageReader("subtitle.png"));
	Clip c2(new FFmpegReader("helloworld2.webm"));
	// CLIP 1 (logo) - Set some clip properties (with Keyframes)
	c1.Position(0.0); // Set the position or location (in seconds) on the timeline
	c1.gravity = GRAVITY_LEFT; // Set the alignment / gravity of the clip (position on the screen)
	c1.scale = SCALE_CROP; // Set the scale mode (how the image is resized to fill the screen)
	c1.Layer(1); // Set the layer of the timeline (higher layers cover up images of lower layers)
	c1.Start(0.0); // Set the starting position of the video (trim the left side of the video)
	c1.End(10.0); // Set the ending position of the video (trim the right side of the video)
	c1.alpha.AddPoint(1, 0.0); // Set the alpha to transparent on frame #1
	c1.alpha.AddPoint(1, 0.0); // Keep the alpha transparent until frame #500
	c1.alpha.AddPoint(265, 1.0); // Animate the alpha from transparent to visible (between frame #501 and #565)
	// CLIP 2 (background video) - Set some clip properties (with Keyframes)
	c2.Position(1.0); // Set the position or location (in seconds) on the timeline
	c2.Start(0.0); // Set the starting position of the video (trim the left side of the video)
	c2.Layer(0); // Set the layer of the timeline (higher layers cover up images of lower layers)
	c2.alpha.AddPoint(1, 0.0); // Set the alpha to visible on frame #1
	c2.alpha.AddPoint(150, 0.0); // Animate the alpha to transparent (between frame 2 and frame #150)
	c2.alpha.AddPoint(360, 0.7, LINEAR); // Keep the alpha transparent until frame #360
	c2.alpha.AddPoint(384, 0.0); // Animate the alpha to visible (between frame #360 and frame #384)

	Timeline r9(640, 480, Fraction(30, 1), 44100, 2, LAYOUT_STEREO);
	r9.debug = false;
	r9.AddClip(&c1);
	r9.AddClip(&c2);

	// Open Timeline
	r9.Open();
	FFmpegWriter w("output1.webm");
	// Set options
	w.SetAudioOptions(true, "libvorbis", 44100, 2, LAYOUT_STEREO, 188000);
	w.SetVideoOptions(true, "libvpx", Fraction(24,1), 1280, 720, Fraction(1,1), false, false, 3000000);
	// Open writer
	w.Open();
	w.WriteFrame(&r9, 1, 500);
	// Close writer & reader
	w.Close();
}
int main(){
	//createASample();	
	createClips();
}
