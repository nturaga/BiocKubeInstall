# README

## INTRO

- Make sure BiocParallel Release 3.14 is being used on the manager node to be in sync with versions on the workers.

- Manually replace the version using,

		BiocManager::install('BiocParallel', ref='RELEASE_3_14')

## Build docker image

	docker build -t bioconductor/bioc-redis-jupyter:RELEASE_3_14 .

## Manual run of helm chart on GKE

Start cluster

	export GKE_CLUSTER=biocjupyter
	export GKE_ZONE=us-east1-b
	export GCP_PD_SIZE=200Gi

	gcloud container clusters create \
		 --zone "$GKE_ZONE" \
         --num-nodes 10 \
         --machine-type=e2-standard-4 "$GKE_CLUSTER"
		 
	gcloud compute disks create "biockubeinstall-nfs-pd-test" --size $GCP_PD_SIZE --zone "$GKE_ZONE"

Get creds

	gcloud container clusters get-credentials "$GKE_CLUSTER" --zone "$GKE_ZONE"

	cd ~/.ssh
	
	kubectl create secret generic bioc-binaries-service-account-auth --from-file=service_account_key=bioc-binaries.json

	cd ~/Documents/bioc/BiocKubeInstall

Helm install

	helm install biocjupytercluster --set workerPoolSize=10 \
          --set biocVersion='3.14' \
          --set workerImageTag='RELEASE_3_14' \
          --set volumeMountSize=$GCP_PD_SIZE \
          --set gcpPdName="biockubeinstall-nfs-pd-test" inst/helm-chart-anvil-jupyter --wait


