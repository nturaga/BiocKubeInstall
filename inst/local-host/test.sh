# git clone git@github.com:Bioconductor/BiocKubeInstall
# git checkout -b local
cd ~/bioc/BiocKubeInstall/inst/local-host

minikube start --cpus 21 --memory 24384

kubectl get all

export KUBE_FEATURE_GATES="BlockVolume=true"

kubectl cluster-info

## running from local checkout out and local cluster
## kube=$HOME/gh/kubernetes/cluster/kubectl.sh

## local storage class mode
kubectl create -f persist/local-storage-class.yaml
kubectl create -f persist/local-class.yaml
kubectl create -f persist/local-pvc.yaml
kubectl create -f persist/local-pv.yaml

## OR
# kubectl apply -f persist/


## remove local provision
## kubectl delete -f persist/local-class.yaml
## kubectl delete sc local-provisioning
## kubectl delete pvc local-pvc-name
## kubectl delete pv local-provision
## kubectl delete -f persist/local-pvc.yaml
## kubectl delete -f persist/local-pv.yaml

## check 
kubectl get pvc
kubectl get pv
kubectl describe pvc local-pvc-name

## to delete all persistent volume claims or by name 
## kubectl delete pvc --all # / pvc-name
## to delete all persistent volumes or by name
## kubectl delete pv --all  # / pv-name
kubectl get all

kubectl create -f rstudio-service.yaml
kubectl create -f redis-service.yaml
kubectl create -f redis-pod.yaml
kubectl create -f manager-pod.yaml
kubectl create -f worker-jobs.yaml

## delete
## kubectl delete -f rstudio-service.yaml
## kubectl delete -f redis-service.yaml
## kubectl delete -f redis-pod.yaml
## kubectl delete -f manager-pod.yaml
## kubectl delete -f worker-jobs.yaml

## to remove a group of pods... 
## kubectl delete -f worker-jobs.yaml

## see all pods
kubectl get pods
kubectl get all

## get info for debugging
kubectl describe pod manager
kubectl describe pods workcluster

## kill minikube
## minikube stop
## minikube delete

## patch in to the manager pod and run test.R
kubectl exec -it pod/manager -- bash

# kubectl exec -it pod/workcluster-846bcbdfb8-gcplf -- bash

