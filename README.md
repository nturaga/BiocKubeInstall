# BiocKubeInstall

Internal Bioconductor package used to create binaries for docker
images produced by Bioconductor. The package installation and binary
creation is parallelized on a Kubernetes cluster launched (at the
moment) on a Kubernetes cluster using GKE.

The package works in sync with the `Bioconductor/k8sredis` kubernetes
application.

## Author

Nitesh Turaga: nturaga.bioc at gmail dot com

Martin Morgan

## Links

**k8sredis**: www.github.com/Bioconductor/k8sredis

**Vignette**: [BiocKubeInstall_Tutorial](https://bioconductor.github.io/BiocKubeInstall/articles/BiocKubeInstall_Tutorial.html)

