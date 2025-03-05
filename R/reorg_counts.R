#' reorganize count data
#'
#' Reorganize count data, from by-individual data frames to a list of 3-dimensional arrays.
#'
#' @param counts A list of data frames, one per individual, with six columns: chromosome, basepairs
#' position, allele1, allele2, readcount1, and readcount 2, as read with `read_counts()`.
#'
#' @param map Optional map of SNPs to consider in the output, as a
#' list of vectors of marker positions. If provided, any other
#' positions will be ignored.
#'
#' @param clean_chr If TRUE, remove "chr" from the chromosome names, so chr13 becomes just 13
#'
#' @param quiet If FALSE, print some tracing info
#'
#' @return A list containing `map` and `counts`, with the map being a list of vectors of Mbp positions, and the counts being a list of 3-dimensional arrays, position x individual x allele.
#'
#' @export

reorg_counts <-
    function(counts, map=NULL, clean_chr=TRUE, quiet=TRUE)
{
    # set of unique chromosomes
    chr <- unique(unlist(lapply(counts, function(a) unique(a[,1]))))

    # clean version of those names
    if(clean_chr) cchr <- sub("^chr", "", chr) else cchr <- chr

    if(!is.null(map)) { # omit other chromosomes; pad with further chromosomes
        if(!all(cchr %in% names(map))) {
            chr <- chr[cchr %in% names(map)]
            cchr <- cchr[cchr %in% names(map)]
        }
        if(!all(names(map) %in% cchr)) {
            chr2add <- names(map)[!(names(map) %in% cchr)]
            chr <- c(chr, chr2add)
            cchr <- c(cchr, chr2add)

            # reorder
            chr <- chr[match(cchr, names(map))]
            cchr <- names(map)
        }
    }


    ind <- names(counts)
    alleles <- sub("Count_", "", colnames(counts[[1]])[5:6])

    output_counts <- vector("list", length(chr))
    names(output_counts) <- chr

    if(is.null(map)) {
        map <- output_counts
    } else { # temporarily use ugly names
        names(map) <- chr

        # also, change Mbp -> bp
        for(i in seq_along(map)) map[[i]] <- map[[i]]*1e6
    }

    # loop over chromosomes
    for(this_chr in chr) {
        if(!quiet) message("- chr ", this_chr)

        # pull out that chromosome
        subcounts <- lapply(counts, function(a) a[a[,1]==this_chr,,drop=FALSE])

        # unique positions
        pos <- sort(unique(unlist(lapply(subcounts, "[[", 2))))

        # drop those that aren't in map
        if(!is.null(map[[this_chr]])) {
            pos <- pos[pos %in% map[[this_chr]]]
        }

        map[[this_chr]] <- pos/1e6

        # object to contain the counts
        output_counts[[this_chr]] <- array(0, dim=c(length(pos), length(ind), 2))
        dimnames(output_counts[[this_chr]]) <- list(pos, ind, alleles)

        # fill in the counts
        for(i in ind) {
            # drop positions not in map
            rownames(subcounts[[i]]) <- as.character(subcounts[[i]][,2])
            this_pos <- subcounts[[i]][,2]
            this_pos <- this_pos[this_pos %in% pos]
            this_pos <- as.character(this_pos)

            # fill in data
            output_counts[[this_chr]][this_pos, i, 1] <- subcounts[[i]][this_pos,5]
            output_counts[[this_chr]][this_pos, i, 2] <- subcounts[[i]][this_pos,6]
        }

    }

    names(map) <- names(output_counts) <- cchr

    list(map=map, counts=output_counts)
}
