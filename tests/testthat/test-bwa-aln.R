context("bwa aln alignment")

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



test_that("Incorrect inputs for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sai_files_single <- file.path(dir, "output.sai")
    sai_files_paired <- file.path(dir,
                                  c("output1.sai", "output2.sai"))

    expect_error(bwa_aln(index_prefix="hello",
                         fastq_files=fastq,
                         sai_files=sai_files_single))
    expect_error(bwa_aln(index_prefix=index_prefix,
                         fastq_files="hello",
                         sai_files=sai_files_single))
    expect_error(bwa_aln(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_single))
    expect_error(bwa_aln(index_prefix=index_prefix,
                         fastq_files=c(fastq1, fastq2),
                         sai_files=sai_files_single))
    expect_error(bwa_aln(index_prefix=index_prefix,
                         fastq_files=c(fastq1, fastq2),
                         sai_files=sai_files_paired))
    expect_error(bwa_aln(index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_paired))
})

test_that("Incorrect inputs for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sai_files_single <- file.path(dir, "output.sai")
    sai_files_paired <- file.path(dir,
                                  c("output1.sai", "output2.sai"))
    fastqs <- c(fastq1, fastq2)


    expect_error(bwa_aln(type="single",
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_single))
    expect_error(bwa_aln(type="single",
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_paired))
    expect_error(bwa_aln(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastqs,
                         sai_files=sai_files_single))
    expect_error(bwa_aln(type="paired",
                         index_prefix=index_prefix,
                         fastq_files=fastq,
                         sai_files=sai_files_paired))    
})




test_that("Correct input for single-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sai_files_single <- file.path(dir, "output.sai")
    bwa_aln(index_prefix=index_prefix,
            fastq_files=fastq,
            sai_files=sai_files_single)
    expect_true(file.exists(sai_files_single))
})

test_that("Correct input for paired-end alignment", {
    dir <- tempfile()
    dir.create(dir)
    sai_files_paired <- file.path(dir,
                                  c("output1.sai","output2.sai"))
    bwa_aln(index_prefix=index_prefix,
            type="paired",
            fastq_files=c(fastq1, fastq2),
            sai_files=sai_files_paired)
    expect_true(all(file.exists(sai_files_paired)))

})



