#' @title R wrapper to run BWA alignment tool BWA-backtrack
#' 
#' @description R wrapper to run BWA alignment tool BWA-backtrack.
#' 
#' @param type String specifying type of reads:
#'     "single" for single-end reads (default) or
#'     "paired" for paired-end reads.
#' @param index_prefix String specifying prefix of the BWA index.
#' @param fastq_files Character vector specifying paths of fastq files.
#'     If \code{type=="single"}, must be of length 1.
#'     If \code{type=="paired"}, must be of length 2.
#' @param sai_files Character vector specifying filenames of the BWA
#'     aligmment output files.
#'     If \code{type=="single"}, must be of length 1.
#'     If \code{type=="paired"}, must be of length 2.
#' @param ... Other arguments to pass to the \code{bwa aln} alignment.
#' 
#' @return No return value. Output files from bwa aln alignment are
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
#' bwa_aln(index_prefix=index_prefix,
#'         fastq_files=fastq,
#'         sai_files=file.path(dir, "output.sai"))
#' 
#' @export
bwa_aln <- function(type=c("single", "paired"),
                    index_prefix,
                    fastq_files,
                    sai_files,
                    ...
){
    program <- "aln"
    type <- match.arg(type)
    args <- list(...)
    .checkBwaIndex(index_prefix)
    .checkFilesFormat(fastq_files, type, "fastq_files", mustExist=TRUE)
    .checkFilesFormat(sai_files, type, "sai_files", mustExist=FALSE)

    if (type=="single"){
        cmd <- sprintf("%s %s %s %s > %s",
                       program,
                       .createFlags(args),
                       shQuote(path.expand(index_prefix)),
                       fastq_files,
                       sai_files)
        cmds <- list(cmd)
    } else {
        cmds <- lapply(seq_len(2), function(sample){
            sprintf("%s %s %s %s > %s",
                    program,
                    .createFlags(args),
                    shQuote(path.expand(index_prefix)),
                    fastq_files[sample],
                    sai_files[sample])
        })
    }
    return(invisible(lapply(cmds,.bwaBin)))
}







