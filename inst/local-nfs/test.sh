# git clone git@github.com:Bioconductor/BiocKubeInstall
# git checkout -b local
cd ~/bioc/BiocKubeInstall/inst/local-nfs

minikube start --cpus 6 --memory 16384

kubectl cluster-info

kubectl create -f nfs-service/nfs-server-gce-pv.yaml
kubectl create -f nfs-service/nfs-server-rc.yaml
kubectl create -f nfs-service/nfs-server-service.yaml

# check that nfs-server is running
kubectl get services
# get IP from nfs-server
kubectl describe services nfs-server

# update IP in nfs-service/nfs-pv.yaml
# copy and paste into 
vim nfs-service/nfs-pv.yaml

## check 
kubectl get pvc
kubectl get pv

## if any running: kubectl delete pvc --all / pvc-name
## if any running: kubectl delete pv --all / pv-name

kubectl create -f nfs-service/nfs-pv.yaml
kubectl create -f nfs-service/nfs-pvc.yaml


## check 
kubectl get pvc
kubectl get pv

kubectl create -f rstudio-service.yaml
kubectl create -f redis-service.yaml
kubectl create -f redis-pod.yaml
kubectl create -f manager-pod.yaml
kubectl create -f worker-jobs.yaml

## to remove a group of pods... 
## kubectl delete -f worker-jobs.yaml

## see all pods
kubectl get pods

## get info for debugging
kubectl describe pod manager
kubectl describe pods workcluster

## see all services
kubectl get services 

# minikube stop
# minikube delete


