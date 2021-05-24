library(dplyr)
library(AnVIL)


base <- contrib.url("bioconductor_docker/packages/3.12/bioc")
contriburl <- paste0("https://storage.googleapis.com/", base)
gs <- paste0("gs://", base)

db = available.packages(contriburl)
ls = gsutil_ls(gs)

pkgs0 <- basename(ls)
pkgs1 <- strsplit(pkgs0, "_")
pkgs <- pkgs1[lengths(pkgs1) == 5L]
PACKAGES <- ls[lengths(pkgs1) == 1L]

observed <- tibble(
    Package = sapply(pkgs, `[[`, 1),
    Version = sapply(pkgs, `[[`, 2)
)
expected <- db[, c("Package", "Version")] |> as_tibble()
o
missing <-
    anti_join(expected, observed) |>
    left_join(observed, by = "Package", suffix = c(".PACKAGES", ".Repository"))

PACKAGES <- setNames(
    sub("gs://", "https://storage.googleapis.com/", PACKAGES),
    basename(PACKAGES)
)

p1 <- read.dcf(url(PACKAGES[["PACKAGES"]]))
p2 <- read.dcf(gzcon(url(PACKAGES[["PACKAGES.gz"]])))
p3 <- local({
    download.file(PACKAGES[["PACKAGES.rds"]], fl <- tempfile())
    rds <- readRDS(fl)
    rownames(rds) <- NULL
    rds[,colnames(p1)]
})
identical(p1, p2)
identical(p1, p3)

x <- c(
    "rhdf5filters", "reticulate", "msigdbr", "viridis", "statmod", "rmarkdown",
    "dendextend"
)




