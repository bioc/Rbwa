context("bwa mem alignment")

# Building index:
dir <- tempfile()
dir.create(dir)
fasta <- system.file(package="Rbwa",
                     "fasta/chr12.fa")
index_prefix <- file.path(dir,"chr12")
bwa_build_index(fasta=fasta, index_prefix=index_prefix)





# Getting fastq:
fastq <- system.file(package="Rbwa",
                     "fastq/sequences.fastq")
fastq1 <- system.file(package="Rbwa",
                      "reads/reads1.fastq")
fastq2 <- system.file(package="Rbwa",
                      "reads/reads2.fastq")
fastqs <- c(fastq1, fastq2)



test_that("Incorrect inputs for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    sam_files <- file.path(dir, c("output1.sam",
                                  "output2.sam"))

    expect_error(bwa_mem(index_prefix="hello",
                         fastq_files=fastq,
                         sam_file=sam_file))
    expect_error(bwa_mem(index_prefix=index_prefix,
                         fastq_files="hello",
                         sam_file=sam_file))
    expect_error(bwa_mem(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastq,
                         sam_file=sam_file))
    expect_error(bwa_mem(index_prefix=index_prefix,
                         fastq_files=c(fastq1, fastq2),
                         sam_file=sam_file))
    expect_error(bwa_mem(index_prefix=index_prefix,
                         fastq_files=fastq,
                         sam_file=sam_files))
})



test_that("Incorrect inputs for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    sam_files <- file.path(dir, c("output1.sam",
                                  "output2.sam"))
    expect_error(bwa_mem(index_prefix="hello",
                         fastq_files=fastqs,
                         sam_file=sam_file))
    expect_error(bwa_mem(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastq,
                         sam_file=sam_file))
    expect_error(bwa_mem(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sam_file=sam_files))
})




test_that("Correct input for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    bwa_mem(index_prefix=index_prefix,
            fastq_files=fastq,
            sam_file=sam_file)
    expect_true(file.exists(sam_file))
})



test_that("Correct input for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sam_file <- file.path(dir, "output.sam")
    bwa_mem(index_prefix=index_prefix,
            type="paired",
            fastq_files=fastqs,
            sam_file=sam_file)
    expect_true(file.exists(sam_file))
})


