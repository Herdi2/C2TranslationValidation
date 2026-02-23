#!/bin/bash
FILE_PATH=$1
METHOD_NAME=$2
FILE_NAME=$(basename $FILE_PATH)
CLASS_NAME=$(echo $FILE_NAME | cut -d "." -f 1) 
java -Xcomp \
     -XX:CompileCommand=compileonly,$CLASS_NAME::$METHOD_NAME \
     -XX:PrintIdealGraphLevel=1 \
     $FILE_PATH
