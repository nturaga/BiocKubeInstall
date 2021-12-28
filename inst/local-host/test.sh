# git clone git@github.com:Bioconductor/BiocKubeInstall
# git checkout -b local
cd ~/bioc/BiocKubeInstall/inst/local-host

minikube start --cpus 21 --memory 24384

kubectl get all
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
kubectl describe pv local-path
kubectl describe pv local-provision

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

## logs
kubectl logs pod/manager
kubectl logs pod/workcluster-bd97c44f7-7fzqt

## kill minikube
## minikube stop
## minikube delete

## patch in to the manager pod and run test.R
kubectl exec -it pod/manager -- bash
## get a count of packages from inside pod
## ls /host/binary_3_14/*tar.gz | wc -l
## more /host/logs_3_14/kube_install.log

## patch in to worker when manager completes
kubectl exec -it pod/workcluster-bd97c44f7-7fzqt -- bash

## copy contents
kubectl cp workcluster-bd97c44f7-2vd48:host/binary_3_14 /home/user/data
kubectl cp workcluster-bd97c44f7-2vd48:host/logs_3_14/kube_install.log /home/user/data/kube_install.log
