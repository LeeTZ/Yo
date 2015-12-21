#!/bin/sh

YO="./parser_test"
binaryoutput="./a.out"
preproc_path="preprocessor.py"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: test.sh [options] [.yo files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
    echo "\033[31m FAILED \033[0m"
    error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
    SignalError "$1 differs"
    echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || { 
        if [[ $5 != *fail* ]]; then
           SignalError "$1 failed on $*"
           return 1
        fi
    }
}

CheckPreprocessor() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    echo -n "$basename.................."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    Compare "../test/preprocessor/intermediate/$basename.yo" ${reffile}.out ${basename}.a.diff

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "\033[32m OK \033[0m"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi 
}


CheckParser() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    echo -n "$basename.................."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    YO="./parser_test"
    generatedfiles="$generatedfiles ${basename}.a.out" &&
    Run "$YO" "<" "../test/parser/intermediate/$basename.yo" ">" ${basename}.a.out &&
    Compare ${basename}.a.out ${reffile}.out ${basename}.a.diff

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "\033[32m OK \033[0m"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi 
}

CheckSemanticAnalysis() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    YO="./semantic_test"
    generatedfiles="$generatedfiles ${basename}.s.out"
    Run "$YO" "<" "../test/semantic/intermediate/$basename.yo" ">" ${basename}.s.out &&
    Compare ${basename}.s.out ${reffile}.out ${basename}.s.diff

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi 
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    
    echo -n "$basename..."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    # old from microc - interpreter
    # generatedfiles="$generatedfiles ${basename}.i.out" &&
    # Run "$YO" "-i" "<" $1 ">" ${basename}.i.out &&
    # Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff

    generatedfiles="$generatedfiles ${basename}.c.out" &&
    Run "$YO" "-c" $1 ">" ${basename}.c.out &&
    Compare ${basename}.c.out ${reffile}.out ${basename}.c.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}
CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    # old from microc - interpreter
    # generatedfiles="$generatedfiles ${basename}.i.out" &&
    # Run "$YO" "-i" "<" $1 ">" ${basename}.i.out &&
    # Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff

    generatedfiles="$generatedfiles ${basename}.c.out" &&
    { 
        Run "$YO" "-b" $1 "2>" ${basename}.c.out || 
        Run "$binaryoutput" ">" ${basename}.b.out 
    } &&
    Compare ${basename}.c.out ${reffile}.out ${basename}.c.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}
TestTypeReader() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    tmpfiles=""
    # old from microc - interpreter
    # generatedfiles="$generatedfiles ${basename}.i.out" &&
    # Run "$YO" "-i" "<" $1 ">" ${basename}.i.out &&
    # Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff
    YO="./typereader_test"
    generatedfiles="$generatedfiles ${basename}.f.cpp ${basename}.f.out yo.prog"
    Run "$YO" "<" "../test/typereader/intermediate/$basename.yo" ">" ${basename}.f.cpp &&
    #g++ ${basename}.f.cpp libclip.cpp yolib.h -lstdc++ -lopenshot-audio -lopenshot -I/usr/local/include/libopenshot -I/usr/local/include/libopenshot-audio -lconfig++ -lavdevice -lavformat  -lavcodec -lavutil -lz `pkg-config --cflags --libs libconfig++ Qt5Gui Qt5Widgets Magick++` -fPIC -std=c++11 -o yo.prog 
    g++ -o yo.prog ${basename}.f.cpp yolib.h -std=c++11 &&
    Run "./yo.prog" ">" ${basename}.f.out &&
    Compare ${basename}.f.out ${reffile}.out ${basename}.f.diff


    #generatedfiles="$generatedfiles ${basename}.f.out" &&
    #tmpfiles="$tmpfiles tests/${basename}.lrx_lrxtmp.c a.out" &&
    #Run "$YO" "-b" $1 &&
    #Run "$binaryoutput" ">" ${basename}.f.out &&
    #Compare ${basename}.f.out ${reffile}.out ${basename}.f.diff
    
    #rm -f $tmpfiles

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}

TestRunningProgram() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    tmpfiles=""
    # old from microc - interpreter
    # generatedfiles="$generatedfiles ${basename}.i.out" &&
    # Run "$YO" "-i" "<" $1 ">" ${basename}.i.out &&
    # Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff
    YO="./generate_test"
    generatedfiles="$generatedfiles ${basename}.f.cpp ${basename}.f.out yo.prog"
    Run "$YO" "<" "../test/intermediate/$basename.yo" ">" ${basename}.f.cpp &&
    #g++ ${basename}.f.cpp libclip.cpp yolib.h -lstdc++ -lopenshot-audio -lopenshot -I/usr/local/include/libopenshot -I/usr/local/include/libopenshot-audio -lconfig++ -lavdevice -lavformat  -lavcodec -lavutil -lz `pkg-config --cflags --libs libconfig++ Qt5Gui Qt5Widgets Magick++` -fPIC -std=c++11 -o yo.prog 
    g++ -o yo.prog ${basename}.f.cpp yolib.h -std=c++11 &&
    Run "./yo.prog" ">" ${basename}.f.out &&
    Compare ${basename}.f.out ${reffile}.out ${basename}.f.diff


    #generatedfiles="$generatedfiles ${basename}.f.out" &&
    #tmpfiles="$tmpfiles tests/${basename}.lrx_lrxtmp.c a.out" &&
    #Run "$YO" "-b" $1 &&
    #Run "$binaryoutput" ">" ${basename}.f.out &&
    #Compare ${basename}.f.out ${reffile}.out ${basename}.f.diff
    
    #rm -f $tmpfiles

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "OK"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi
}


MunanTest() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    tmpfiles=""

    YO="./generate_test"
    generatedfiles="$generatedfiles ${basename}.f.cpp ${basename}.f.out yo.prog"
    echo "\n"
    echo "\n"
    Run "$YO" "<" "../test/intermediate/$basename.yo" &&
    #g++ ${basename}.f.cpp libclip.cpp yolib.h -lstdc++ -lopenshot-audio -lopenshot -I/usr/local/include/libopenshot -I/usr/local/include/libopenshot-audio -lconfig++ -lavdevice -lavformat  -lavcodec -lavutil -lz `pkg-config --cflags --libs libconfig++ Qt5Gui Qt5Widgets Magick++` -fPIC -std=c++11 -o yo.prog 
    #g++ -o yo.prog ${basename}.f.cpp yolib.h -std=c++11 &&
    #Run "./yo.prog" ">" ${basename}.f.out &&
    #Compare ${basename}.f.out ${reffile}.out ${basename}.f.diff

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
        rm -f $generatedfiles
    fi
    echo "\n"
    globalerror=$error
    fi
}


while getopts kdpsh c; do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
    h) # Help
        Usage
        ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.yo"
fi

for file in $files
do
    case $file in
    *test-preprocess*)
        echo "##### Now Testing Preprocessor #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        CheckPreprocessor $file 2>> $globallog
        ;;
    *test-parser*)
        echo "##### Now Testing Parser #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        CheckParser $file 2>> $globallog
        ;;
    *test-semantic*)
        echo "##### Now Testing Semantic Analysis #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        CheckSemanticAnalysis $file 2>> $globallog
        ;;
    *test-full*)
        echo "##### Now Testing FullStack #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        TestRunningProgram $file 2>> $globallog
        ;;
    *test-munan*)
        echo "##### Now Testing Munan #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        MunanTest $file 
        ;;
    *test-typereader*)
        echo "##### Now Testing Single FullStack #####"
        echo "preprocessing....."
        python $preproc_path $file
        echo "\033[32m OK \033[0m"
        TestTypeReader $file 2>> $globallog
        ;;
    *test-fail*)
        CheckFail $file 2>> $globallog
        ;;
    *test-*)
        Check $file 2>> $globallog
        ;;
    
        #echo "unknown file type $file"
        #globalerror=1
        #;;
    esac
done

exit $globalerror
