# BiocKubeInstall

Internal Bioconductor package used to create binaries for docker
images produced by Bioconductor. The package installation and binary
creation is parallelized on a Kubernetes cluster launched (at the
moment) on a Kubernetes cluster using GKE.

The package works in sync with the `Bioconductor/k8sredis` kubernetes
application.

## Quickstart 

Coming soon....


## TODO

1. Validity checks in package

2. Errors in write_PACKAGES()

```
> tools::write_PACKAGES(bin_path, addFiles=TRUE)


gzip: stdin: unexpected end of file
/usr/bin/tar: Unexpected EOF in archive
/usr/bin/tar: Error is not recoverable: exiting now
/usr/bin/tar: Unexpected EOF in archive
/usr/bin/tar: Error is not recoverable: exiting now
```

3. More automation
