fswatch -o src/client/main.ml | xargs -n1 -I{} bin/build_client.sh
