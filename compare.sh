#!/bin/bash

JAVA_FILE=$1
JAVA_METHOD=$2
FILE_NAME=$(basename $JAVA_FILE)
JAVA_CLASS=$(echo $FILE_NAME | cut -d "." -f 1) 

$JAVA -Xcomp \
     -Xbatch \
     -XX:+DelayMem \
     -XX:ControlBugs=21 \
     -XX:-UseCompressedOops \
     -XX:-TieredCompilation \
     -XX:+DelayArithmeticOpts \
     -XX:CompileCommand=compileonly,$JAVA_CLASS::$JAVA_METHOD \
     $JAVA_FILE

$JAVA -Xint \
     $JAVA_FILE
