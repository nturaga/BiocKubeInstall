## Startup: Define important variables
library(BiocKubeInstall)


##########
## GLOBALS
##########

## Ports needed for Redis to work
Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

## the parameter parallelism has to match what is on the
## worker-jobs.yaml file.
parallelism <- 6L
lib_path <- "/host/library"
bin_path <- "/host/binaries"
## the 'binary_repository' is where the existing binaries are located.
secret_path <- "/home/rstudio/key.json"
binary_repo <- "anvil-rstudio-bioconductor-test/0.99/3.11/"
cran_bucket <- "anvil-rstudio-bioconductor/0.99/3.11/src/contrib/"

##########
## RUN
##########

## Step 1:  Wait till all the worker pods are up and running
BiocKubeInstall::kube_wait(workers = parallelism)

## Step. 2 : Load deps and installed packages
deps <- BiocKubeInstall::pkg_dependencies(binary_repo = binary_repo)

## Step 3: Run kube_install so package binaries are built
res <- BiocKubeInstall::kube_install(workers = parallelism,
                                     lib_path = lib_path,
                                     bin_path = bin_path,
                                     deps = deps)

## Step 4: Run sync to google bucket
BiocKubeInstall::gcloud_binary_sync(secret = secret_path,
                                    bin_path = bin_path,
                                    bucket = cran_bucket)

## Step 5: check if all workers were used
check <- table(unlist(res))
