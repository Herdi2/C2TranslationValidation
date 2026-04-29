#!/bin/bash

JAVA_FILE=$1
JAVA_METHOD=$2
FILE_NAME=$(basename $JAVA_FILE)
JAVA_CLASS=$(echo $FILE_NAME | cut -d "." -f 1) 

$JDK -Xcomp \
     -Xbatch \
     -XX:+ReintroduceBugs \
     -XX:-UseCompressedOops \
     -XX:+PrintFloatBits \
     -XX:+DelayArithmeticOpts \
     -XX:+PrintRealMinMax \
     -XX:-TieredCompilation \
     -XX:CompileCommand=compileonly,$JAVA_CLASS::$JAVA_METHOD \
     $JAVA_FILE

$JDK -Xint \
     $JAVA_FILE
