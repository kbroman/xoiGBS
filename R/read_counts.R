#' read counts data from a set of files
#'
#' read counts data from a set of files
#'
#' @param files A vector of character strings with the counts data. These text files can be gzipped. Each file should be one backcross individual, and the files should have six whitespace-delimited columns: chromosome, basepairs positions, allele1, allele2, readcount1, readcount2, with allele1 being the allele for the homozygote parent. Alternatively, it can be a single directory, in which case we read all the .txt or .gz files in that directory.
#'
#' @return A list of data frames with the contents of the files
#'
#' @details Names are taken from the file names...everything before ".txt" or ".gz", but removing "_read_counts" if it's part of the name.
#' So BCA81-2_read_counts.txt.gz becomes BCA81-2

#' @importFrom data.table fread
#' @importFrom R.utils gunzip
#'
#' @export

read_counts <-
    function(files)
{
    if(length(files)==1 && dir.exists(files)) {
        # treat it as a directory
        dir <- files
        files <- c(list.files(dir, pattern="\\.txt$"),
                   list.files(dir, pattern="\\.gz$"))
        files <- file.path(dir, files)
    }
    return(files)

    # could make this multi-threaded?
    result <- lapply(files, read_counts_file)

    # add names taken from file names
    ids <- basename(files)
    ids <- sub("\\.gz", "", ids)
    ids <- sub("\\.txt", "", ids)
    ids <- sub("_read_counts", "", ids)

    names(result) <- ids

    result
}


# internal function that does the work, reading one file
read_counts_file <-
    function(file, quiet=TRUE)
{

    gzipped <- FALSE
    # if file was gzipped, uncompress it
    if(grepl("\\.gz$", file)) {
        gzipped <- TRUE

        file <- gunzip(file, overwrite=TRUE, remove=FALSE, temporary=TRUE)
        if(!quiet) message("gunzipped to ", file)
    }

    result <- data.table::fread(file)

    # if file was gzipped, clean up afterwards
    if(gzipped) {
        unlink(file)
        if(!quiet) message("removed temporary file ", file)
    }

    as.data.frame(result)
}
