#!/usr/bin/env sh


if [ -s ".pid" ]
then
    echo "Killing previous server"
    PID=`cat .pid`;
    echo "PID: " $PID;
    (kill `cat .pid`) || 0
    rm .pid
fi

set -e

echo "Cleaning artifacts"
make clean
echo "Rebuilding main"
make
echo "Running server"
./main.byte &
MAIN_PID=$!
echo "Saving process id to .pid: " $MAIN_PID
echo $MAIN_PID > .pid
echo "Done"
