# rebuild docker images with local branch
docker build --no-cache -t mr148/bioc-redis:RELEASE_3_15 -f Dockerfile.worker.RELEASE_3_15 .
docker build --no-cache -t mr148/bioc-redis:manager -f Dockerfile.localmanager .

docker push mr148/bioc-redis:RELEASE_3_15
docker push mr148/bioc-redis:manager
