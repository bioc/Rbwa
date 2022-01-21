#' @title R wrapper to convert bwa aln output to SAM format
#' 
#' @description R wrapper to convert bwa aln output to SAM format.
#' 
#' @param type String specifying type of reads:
#'     "single" for single-end reads (default) or
#'     "paired" for paired-end reads.
#' @param index_prefix String specifying prefix of the BWA index.
#' @param fastq_files Character vector specifying paths of fastq files.
#'     If \code{type=="single"}, must be of length 1.
#'     If \code{type=="paired"}, must be of length 2.
#' @param sai_files Character vector specifying filenames of the \code{bwa aln}
#'     aligmment output files.
#'     If \code{type=="single"}, must be of length 1.
#'     If \code{type=="paired"}, must be of length 2.
#' @param sam_file String specifying paths of the SAM output file.
#' @param ... Other arguments to pass to \code{bwa_sam}.
#' 
#' @return No return value. Output SAM files are
#'     produced as side effect.
#' 
#' @author Jean-Philippe Fortin
#' 
#' @examples 
#' # Creating index:
#' dir <- tempdir()
#' fasta <- system.file(package="Rbwa",
#'                      "fasta/chr12.fa")
#' fastq <- system.file(package="Rbwa",
#'                      "fastq/sequences.fastq")
#' index_prefix <- file.path(dir,"chr12")
#' bwa_build_index(fasta, index_prefix=index_prefix)
#' 
#' # Creating alignments:
#' bwa_aln(index_prefix=index_prefix,
#'         fastq_files=fastq,
#'         sai_files=file.path(dir, "output.sai"))
#' 
#' # Generating SAM file:
#' bwa_sam(index_prefix=index_prefix,
#'         fastq_files=fastq,
#'         sai_files=file.path(dir, "output.sai"),
#'         sam_file=file.path(dir, "output.sam"))
#' 
#' # Reading in alignments from SAM file:
#' aln <- readLines(file.path(dir, "output.sam"))
#' aln
#' 
#' @export
bwa_sam <- function(type=c("single", "paired"),
                    index_prefix,
                    fastq_files,
                    sai_files,
                    sam_file,
                    ...
){
    type <- match.arg(type)
    program <- ifelse(type=="single", "samse", "sampe")
    args <- list(...)
    .checkBwaIndex(index_prefix)
    .checkFilesFormat(fastq_files, type, "fastq_files",mustExist=TRUE)
    .checkFilesFormat(sai_files, type, "sai_files", mustExist=TRUE)
    if (length(sam_file)!=1){
        stop("sam_file must be a character vector of length 1")
    }

    if (type=="paired"){
        fastq_files <- paste0(fastq_files, collapse=" ")
        sai_files <- paste0(sai_files, collapse=" ")
    }
    args <- sprintf("%s %s %s %s %s > %s",
                    program,
                    .createFlags(args),
                    shQuote(path.expand(index_prefix)),
                    sai_files,
                    fastq_files,
                    sam_file)
    return(invisible(.bwaBin(args)))
}

