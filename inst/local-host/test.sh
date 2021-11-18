# git clone git@github.com:Bioconductor/BiocKubeInstall
# git checkout -b local
cd ~/bioc/BiocKubeInstall/inst/local-host

minikube start --cpus 6 --memory 16384
# minikube start --cpus 6 --memory 16384 --driver=kvm2

export KUBE_FEATURE_GATES="BlockVolume=true"

kubectl cluster-info

## running from local checkout out and local cluster
kube=$HOME/gh/kubernetes/cluster/kubectl.sh

# kubectl -f nfs-service/nfs-server-gce-pv.yaml
# kubectl create -f nfs-service/nfs-server-rc.yaml
# kubectl create -f nfs-service/nfs-server-service.yaml
## OR
kubectl apply -f persist/

# check that nfs-server is running
kubectl get services
# get IP from nfs-server
kubectl describe services nfs-server

kubectl get replicationcontrollers

# update IP in nfs-service/nfs-pv.yaml
# copy and paste into 
vim nfs-service/nfs-pv.yaml

## check 
kubectl get pvc
kubectl get pv

## to delete all persistent volume claims or by name 
## kubectl delete pvc --all # / pvc-name
## to delete all persistent volumes or by name
## kubectl delete pv --all  # / pv-name

kubectl create -f nfs-pv.yaml
kubectl create -f nfs-pvc.yaml

kubectl delete -f nfs-pvc.yaml

kubectl get all

kubectl get pvc
kubectl get pv

## local storage class mode
kubectl create -f persist/local-storage-class.yaml
kubectl create -f persist/local-class.yaml
kubectl create -f persist/local-pvc.yaml
kubectl create -f persist/local-pv.yaml

kubectl delete -f persist/local-class.yaml
kubectl delete sc local-provisioning
kubectl delete pvc local-pvc-name
kubectl delete pv local-provision
# kubectl delete -f persist/local-pvc.yaml
kubectl delete -f persist/local-pv.yaml

## check 
kubectl get pvc
kubectl describe pvc local-pvc-name
kubectl get pv
kubectl get sc

kubectl create -f rstudio-service.yaml
kubectl create -f redis-service.yaml
kubectl create -f redis-pod.yaml
kubectl create -f manager-pod.yaml
kubectl create -f worker-jobs.yaml

kubectl delete -f rstudio-service.yaml
kubectl delete -f redis-service.yaml
kubectl delete -f redis-pod.yaml
kubectl delete -f manager-pod.yaml
kubectl delete -f worker-jobs.yaml

## to remove a group of pods... 
## kubectl delete -f worker-jobs.yaml

## see all pods
kubectl get pods
kubectl get all
kubectl get pv
kubectl get pvc
kubectl get nodes
kubectl get sc

## get info for debugging
kubectl describe pod manager
kubectl describe pods workcluster

## see all services
kubectl get services 
kubectl get nodes

minikube stop
minikube delete

kubectl exec -it pod/manager -- bash
kubectl exec -it pod/workcluster-846bcbdfb8-gcplf -- bash

