# rebuild docker images with local branch
docker build --no-cache -t mr148/bioc-redis:RELEASE_3_14 -f Dockerfile.worker.RELEASE_3_14 .
docker build --no-cache -t mr148/bioc-redis:manager -f Dockerfile.localmanager .

docker push mr148/bioc-redis:RELEASE_3_14
docker push mr148/bioc-redis:manager
