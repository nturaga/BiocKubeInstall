source <- file_share <- "https://bioconductordocker.file.core.windows.net/biocbinaries/binary_3_13/*"

destination <- blob_store <- "https://bioconductordocker.blob.core.windows.net/packages/3.12/bioc/src/contrib/"

az_copy <-
    function(source, destination, mysas)
{
    fs <- file_endpoint(source, sas = mysas)
    bs <- blob_endpoint(destination, sas = mysas)
    ## azcopy
    call_azcopy(
        "copy", file_share,blob_store
    )
}
