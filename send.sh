#!/bin/bash
FILE_PATH=$1
METHOD_NAME=$2
FILE_NAME=$(basename $FILE_PATH)
CLASS_NAME=$(echo $FILE_NAME | cut -d "." -f 1) 
JAVA=~/work/jdk-thesis/build/linux-x86_64-server-fastdebug/images/jdk/bin/java
$JAVA -Xcomp \
      -XX:+TraceIterativeGVN \
      -XX:CompileCommand=compileonly,$CLASS_NAME::$METHOD_NAME \
      -XX:-UseCompressedOops \
      -XX:+DelayArithmeticOpts \
      -XX:+PrintFloatBits \
      -XX:PrintIdealGraphLevel=6 \
      $FILE_PATH
