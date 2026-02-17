#!/bin/bash
FILE_PATH=$1
FILE_NAME=$(basename $FILE_PATH)
CLASS_NAME=$(echo $FILE_NAME | cut -d "." -f 1) 
java -Xcomp \
     -XX:CompileCommand=compileonly,$CLASS_NAME::method \
     -XX:PrintIdealGraphLevel=1 \
     $FILE_PATH
