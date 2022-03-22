#' @title Unpack multiple alignments stored in BWA output
#' 
#' @description Unpack multiple alignments stored in BWA output
#' 
#' @param input_sam_file String specifying path of the input SAM file.
#' @param output_sam_file String specifying path of the output SAM file.
#' 
#' @return Returns NULL invisibly. SAM file with multiple alignments
#'    is produced as a side effect.
#' 
#' @details Each row in the SAM file produced by \code{\link{bwa_aln}}
#'     corresponds to the best alignment hit for a given input query
#'     sequence. Other alignments (secondary alignments, or other loci
#'     in case of multiple alignments) are stored in the XA tag.
#' 
#'     \code{xa2multi} conveniently extracts the alignments from the XA
#'     tags and represent them as additional rows in the SAM format.
#' 
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
#' # Generating multiple alignments:
#' xa2multi(input_sam_file=file.path(dir, "output.sam"),
#'          output_sam_file=file.path(dir, "output.multi.sam"))
#' 
#' #' Reading in: 
#' aln <- readLines(file.path(dir, "output.multi.sam"))
#' aln
#' 
#' @export
xa2multi <- function(input_sam_file,
                     output_sam_file
){
    type <- "single"
    .checkFilesFormat(input_sam_file, type, "input_sam_file", mustExist=TRUE)
    .checkFilesFormat(output_sam_file, type, "output_sam_file", mustExist=FALSE)
    program <- system.file("perl/xa2multi.pl",
                           package="Rbwa")

    cmd <- sprintf("perl %s %s > %s",
                   program,
                   input_sam_file,
                   output_sam_file)
    args <- sprintf("%s %s > %s",
                   program,
                   input_sam_file,
                   output_sam_file)
    system2("perl", args)
    invisible(NULL)
}
