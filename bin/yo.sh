#!/bin/sh

binaryoutput="./a.out"
preproc_path="../util/pre.py"

# Set time limit for all operations
ulimit -t 30

globallog=yo.log
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

TestRunningProgram() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.yo//'`
    reffile=`echo $1 | sed 's/.yo$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    echo "\033[32mRunning $basename... \033[0m"
    echo "###### Running $basename" 1>&2
    generatedfiles=""
    tmpfiles=""

    YO="../src/generate_test"
    #generatedfiles="$generatedfiles ${basename}.f.cpp ${basename}.f.out yo.prog ${basedir%.}${basename}.yo.pre"
    generatedfiles=""
    Run "$YO" "<" "${basedir%.}$basename.yo.pre" ">" ${basename}.f.cpp &&
    g++ ${basename}.f.cpp ../src/yolib.h -lstdc++ -lopenshot-audio -lopenshot -I/usr/local/include/libopenshot -I/usr/local/include/libopenshot-audio -lconfig++ -lavdevice -lavformat  -lavcodec -lavutil -lz `pkg-config --cflags --libs libconfig++ Qt5Gui Qt5Widgets Magick++` -fPIC -std=c++11 -o yo.prog 
    #g++ -o yo.prog ${basename}.f.cpp yolib.h -std=c++11 &&
    Run "./yo.prog" 
    

    if [ $error -eq 0 ] ; then
    if [ $keep -eq 0 ] ; then
          rm -f $generatedfiles
    fi
    echo "\033[32m\nFinished\033[0m"
    echo "###### SUCCESS" 1>&2
    else
    echo "###### FAILED" 1>&2
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
        python $preproc_path $file
        TestRunningProgram $file #2>> $globallog
done

exit $globalerror
