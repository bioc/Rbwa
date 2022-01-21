context("bwa sam")

# Building index:
inputdir <- tempfile()
dir.create(inputdir)
fasta <- system.file(package="Rbwa",
                     "fasta/chr12.fa")
index_prefix <- file.path(inputdir,"chr12")
bwa_build_index(fasta=fasta,
                index_prefix=index_prefix)

# Getting fastq:
fastq <- system.file(package="Rbwa",
                     "fastq/sequences.fastq")
fastq1 <- system.file(package="Rbwa",
                      "reads/reads1.fastq")
fastq2 <- system.file(package="Rbwa",
                      "reads/reads2.fastq")
fastqs <- c(fastq1, fastq2)

sai_files_single <- file.path(inputdir, "output.sai")
sai_files_paired <- file.path(inputdir,
                              c("output1.sai", "output2.sai"))


# Single-end alignment
bwa_aln(index_prefix=index_prefix,
        fastq_files=fastq,
        sai_files=sai_files_single)

# Paired-end alignment
bwa_aln(type="paired",
        index_prefix=index_prefix,
        fastq_files=fastqs,
        sai_files=sai_files_paired)

sam_file_single <- file.path(inputdir, "output_single.sam")
bwa_sam(type="single",
        index_prefix=index_prefix,
        fastq_files=fastq,
        sai_files=sai_files_single,
        sam_file=sam_file_single)
sam_file_paired <- file.path(inputdir, "output_paired.sam")
bwa_sam(type="paired",
        index_prefix=index_prefix,
        fastq_files=fastqs,
        sai_files=sai_files_paired,
        sam_file=sam_file_paired)
    

test_that("Correct input for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    output <- file.path(dir, "output_multi.sam")
    xa2multi(sam_file_single,
             output)
    expect_true(file.exists(output))
})

test_that("Correct input for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    output <- file.path(dir, "output_multi.sam")
    xa2multi(sam_file_paired,
             output)
    expect_true(file.exists(output))
})


