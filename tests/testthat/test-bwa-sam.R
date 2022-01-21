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





test_that("Incorrect inputs for single-end alignment", {
    expect_error(bwa_sam(index_prefix="hello",
                         fastq_files=fastq,
                         sai_files=sai_files_single,
                         sam_file="output.sam"))
    expect_error(bwa_sam(index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_single,
                         sam_file="output.sam"))
    expect_error(bwa_sam(index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_paired,
                         sam_file="output.sam"))
    expect_error(bwa_sam(index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_paired,
                         sam_file="output.sam"))
    expect_error(bwa_sam(index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_single,
                         sam_file=c("output.sam", "output2")))
})

test_that("Incorrect inputs for paired-end alignment", {
    expect_error(bwa_sam(type="paired",
                         index_prefix="hello",
                         fastq_files=fastq,
                         sai_files=sai_files_single,
                         sam_file="output.sam"))
    expect_error(bwa_sam(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_single,
                         sam_file="output.sam"))
    expect_error(bwa_sam(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_paired,
                         sam_file="output.sam"))
    expect_error(bwa_sam(type="paired",,
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_paired,
                         sam_file=c("output.sam", "output2")))
})




test_that("Correct input for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    bwa_sam(type="single",
            index_prefix=index_prefix,
            fastq_files=fastq,
            sai_files=sai_files_single,
            sam_file=sam_file)
    expect_true(file.exists(sam_file))
})


test_that("Correct input for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    bwa_sam(type="paired",
            index_prefix=index_prefix,
            fastq_files=fastqs,
            sai_files=sai_files_paired,
            sam_file=sam_file)
    expect_true(file.exists(sam_file))
})









