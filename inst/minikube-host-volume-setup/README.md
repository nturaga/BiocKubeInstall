minikube run of BiocKubeInstall
=====

# Step 1: Start minikube with required configuration

Assumption is you have docker running, with preferences on Docker set
to a higher performance

	minikube start --cpus 6 --memory 16384 \
		--mount-string="$HOME/R/bioconductor_docker/data:/host"
	
# Step 2: Launch the app

	kubectl apply -f app/

