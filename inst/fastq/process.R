.fastqfy <- function(sequences,
                     temporary=TRUE,
                     file=NULL
){
    lines <- list()
    lines[[1]] <- paste0("@", sequences)
    lines[[2]] <- sequences
    lines[[3]] <- paste0("+", sequences)
    lines[[4]] <- vapply(nchar(sequences), function(x){
                      paste0(rep('~', x), collapse='')
                  }, FUN.VALUE="a")    
    temp <- split(do.call(cbind, lines),
                  f=sequences)
    temp <- matrix(unlist(temp), ncol=1)
    if (temporary){
        file <- tempfile()
    } else {
        if (is.null(file)){
            stop("If temporary=FALSE, 'file' must be provided.")
        }
    }
    write.table(temp, 
                file=file,
                quote=FALSE,
                row.names=FALSE,
                col.names=FALSE)
    return(file)
}


seqs <- c("TCGGCTCTCACCGTGTCCG",
          "GCACTGCGGTGAGTGCTGT",
          "GTCATGCCCCCTCAGCCAG",
          "CCTGTGATCCACGGAGGCT",
          "CAACCCAGCCCCCCTCCAA",
          "GCCTTTTACAGTTCGTACT",
          "ACATCAGAAAGAGCGGCAG")

.fastqfy(seqs,
         temporary=FALSE,
         file="sequences.fastq")


