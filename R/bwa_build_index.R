#' @title R wrapper to create BWA index files
#' 
#' @description R wrapper to create BWA index files from a FASTA file.
#' 
#' @param fasta String specifying path to a FASTA file. 
#' @param index_prefix String specifying prefix of the output BWA index.
#' @param ... Other arguments to pass to \code{bwa index}.
#' 
#' @return No return value. BWA index files are produced as a side-effect.
#' 
#' @examples
#' dir <- tempdir()
#' fasta <- system.file(package="Rbwa",
#'                      "fasta/chr12.fa")
#' bwa_build_index(fasta,
#'                 index_prefix=file.path(dir,"chr12"))
#' 
#' @author Jean-Philippe Fortin
#' 
#' @export
bwa_build_index <- function(fasta,
                            index_prefix=NULL,
                            ...
){
    if (!file.exists(fasta)){
        stop("fasta file cannot be found.")
    }
    if (is.null(index_prefix)){
        index_prefix <- gsub(".fa$|.FA$|.fasta$", "", fasta)
    }
    args <- list(...)
    args <- sprintf("%s -p %s %s %s",
                    "index",
                    index_prefix,
                    .createFlags(args),
                    fasta)
    return(invisible(.bwaBin(args)))
}