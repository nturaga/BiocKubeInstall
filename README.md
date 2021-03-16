# BiocKubeInstall

Internal Bioconductor package used to create binaries for docker
images produced by Bioconductor. The package installation and binary
creation is parallelized on a Kubernetes cluster launched (at the
moment) on a Kubernetes cluster using GKE.

The package works in sync with the `Bioconductor/k8sredis` kubernetes
application.

## Author

Nitesh Turaga - nturaga.bioc@gmail.com

Martin Morgan

## Links

**k8sredis**: www.github.com/Bioconductor/k8sredis

**Vignette**: [BiocKubeInstall_Tutorial](https://bioconductor.github.io/BiocKubeInstall/articles/BiocKubeInstall_Tutorial.html)

## Components 

1. inst/helm-chart : helm chart for k8s app

2. inst/docker : docker images used inside the k8s application

3. BiocKubeInstall R/ : Code which runs on the manager and worker replica sets. 

4. Github Actions: Start cluster and deploy to cluster, delete cluster, and test binary package installs 3 times a week. 

## Usage for AnVIL platform and all bioconductor_docker inherited images

```
BiocManager::install('Bioconductor/AnVIL')

## Should show the Google storage api as the top result
AnVIL::repositories()

AnVIL::install('Rhtslib')
``` 
