#' @title R wrapper to run BWA alignment tool BWA-MEM
#' 
#' @description R wrapper to run BWA alignment tool BWA-MEM.
#' 
#' @param type String specifying type of reads:
#'     "single" for single-end reads (default) or
#'     "paired" for paired-end reads.
#' @param index_prefix String specifying prefix of the BWA index.
#' @param fastq_files Character vector specifying paths of fastq files.
#'     If \code{type=="single"}, must be of length 1.
#'     If \code{type=="paired"}, must be of length 2.
#' @param sam_file String specifying filename of the SAM
#'     aligmment output.
#' @param ... Other arguments to pass to the \code{bwa aln} alignment.
#' 
#' @return No return value. Output SAM file is
#'     produced as side effect. 
#' 
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' dir <- tempdir()
#' fasta <- system.file(package="Rbwa",
#'                      "fasta/chr12.fa")
#' fastq <- system.file(package="Rbwa",
#'                      "fastq/sequences.fastq")
#' index_prefix <- file.path(dir,"chr12")
#' bwa_build_index(fasta, index_prefix=index_prefix)
#' 
#' bwa_mem(index_prefix=index_prefix,
#'         fastq_files=fastq,
#'         sam_file=file.path(dir, "output.sam"))
#' 
#' @export
bwa_mem <- function(type=c("single", "paired"),
                    index_prefix,
                    fastq_files,
                    sam_file,
                    ...
){
    program <- "mem"
    type <- match.arg(type)
    args <- list(...)
    .checkBwaIndex(index_prefix)
    .checkFilesFormat(fastq_files, type, "fastq_files",mustExist=TRUE)
    if (length(sam_file)!=1){
        stop("sam_file must be a character vector of length 1.")
    }


    if (type=="paired"){
        fastq_files <- paste0(fastq_files, collapse=" ")
    }

    
    cmd <- sprintf("%s %s %s %s > %s",
                   program,
                   .createFlags(args),
                   shQuote(path.expand(index_prefix)),
                   fastq_files,
                   sam_file)
   
    return(invisible(.bwaBin(cmd)))
}







