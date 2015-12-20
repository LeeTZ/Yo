
/*create a clip from a file, 
  yo prog:
  r = Clip("output.webm") 	
  generate as:
  tr1::shared_ptr<Timeline> r = createClip("output.webm");
*/


/* create clips from a dir
   yo:prog:
   clips = readclips("dirname/")
   generate as:
   std::vector<tr1::shared_ptr<Timeline>> clips = createClips("dir/");
*/


/* clips addition
   yo:prog:
   clip = clip1 + clip2
   generate as:
   tr1::shared_ptr<Timeline> clip = addClip(clip1,clip2);
*/



/* clips layering
   yo:prog:
   clip = clip1 ^ clip2 @ 1.0
   generate as:
   tr1::shared_ptr<Timeline> clip = layerClip(clip1,clip2,1.0);
*/



/* write clips to a file
   yo:prog:
   write(clip,"filename.mp4")
   generate as:
   writeClips(clip,"filename.mp4");
*/


/*
	clipRange argument is double (seconds)
	yo:prog:
    a = clip[2.0:3.0]
    generate as:
    a = clipRange(clip,2.0,3.0);
*/


/*
	clipRange argument is integer (frames)
	yo:prog:
    a = clip[24:48]
    generate as:
    a = clipRange(clip,24,48);
*/



/*
	clipIndex argument is integer (frame)
	yo:prog:
    a = clip[24]
    generate as:
    a = clipIndex(clip,24);
*/



/*
	clipIndex argument is double (time)
	yo:prog:
    a = clip[2.4]
    generate as:
    a = clipIndex(clip,2.4);
*/



/*
return a matrix of 
pixel : R G B{}
pixel = getpixel(clip,frame,i,j)
*/




/* write clips to a file
   yo:prog:
   clip.alpha@1 = 255
   generate as:
   setProperty(clip,"alpha",1,255);
   
*/

