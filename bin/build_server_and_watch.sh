fswatch -o src/*.ml | xargs -n1 -I{} bin/build_and_run_server.sh
