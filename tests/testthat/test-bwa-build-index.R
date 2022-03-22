context("bwa_build_index")

fasta <- system.file(package="Rbwa",
                     "fasta/chr12.fa")

test_that("Incorrect inputs", {
    expect_error(
        bwa_build_index(fasta="notfound.fa")
    )
    expect_error(
        bwa_build_index(fasta=gsub(".fa", "", fasta))
    )
})

test_that("Index generation", {
    dir <- tempdir()
    index_prefix <- file.path(dir,"chr12")
    outfiles <- c("chr12.amb",
                  "chr12.ann",
                  "chr12.bwt",
                  "chr12.pac",
                  "chr12.sa")
    expect_invisible(bwa_build_index(fasta=fasta,
                                     index_prefix=index_prefix))
    expect_true(all(file.exists(file.path(dir, outfiles))))
    
})
