minikube run of BiocKubeInstall
=====

# Step 1: Start minikube with required configuration

Assumption is you have docker running, with preferences on Docker set
to a higher performance

	minikube start --cpus 6 --memory 16384 --driver=hyperkit
	
# Step 2: Start NFS server

The steps to start the NFS server are as follows,

	kubectl create -f nfs-server/nfs-server-gce-pv.yaml
	
	kubectl create -f nfs-server/nfs-server-service.yaml
	
	kubectl create -f nfs-server/nfs-server-rc.yaml

Get the IP address of the NFS service, 

	kubectl describe service nfs-server

Then, to start NFS server you must replace the IP address in the
`nfs-pv.yaml` listed as follows,

	nfs:
	  server: <IP>
	  

After that,

	kubectl create -f nfs-server/nfs-pv.yaml
	kubectl create -f nfs-server/nfs-pvc.yaml
	
# Step 3: Launch the app

	kubectl apply -f app/

