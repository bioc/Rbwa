.bwaBin <- function(args=""){
    if(is.null(args) || args==""){
        stop("The bwa binaries need to be called",
             " with additional arguments")
    }
    args <- gsub("^ *| *$", "", args)
    bin  <- file.path(system.file(package="Rbwa"), "bwa")
    call <- paste(shQuote(bin), args)
    output <- system(call, intern=TRUE)
    return(output)
}



.checkBwaIndex <- function(index_prefix){
    suffixes <- c("amb","ann", "bwt", "pac", "sa")
    files   <- paste0(index_prefix, ".", suffixes)
    missing <- files[!file.exists(files)]
    if (length(missing)>0){
        missing <- paste0(missing, collapse="\n") 
        stop("The following files needed for bwa",
             " are missing: \n", missing)
    }
    return(invisible())
}



.checkFilesFormat <- function(input_files,
                              type=c("single", "paired"),
                              name="fastq_files",
                              mustExist=TRUE
){
    type <- match.arg(type)
    if (type=="single"){
        if (length(input_files)!=1){
            stop("For type=single, ",name," must be a character vector ",
                 "of length 1.")
        }
        if (!is.character(input_files)){
            stop(name," must be a character vector")
        }
        if (mustExist){
            if (!file.exists(input_files)){
                stop("File specified by ",name," does not exist.")
            }
        }
    } else if (type=="paired"){
        if (length(input_files)!=2){
            stop("For type=double, ",name," must be a character",
                 " vector of length 2.")
        }
        if (!is.character(input_files)){
            stop(name," must be a character vector")
        }
        if (mustExist){
            if (!file.exists(input_files[1])){
                stop("First file specified by ",name," does not exist.")
            }
            if (!file.exists(input_files[2])){
                stop("Second file specified by ",name," does not exist.")
            }
        }
    }
    invisible(NULL)
}


# Helper function to create a scalar of command line
# arguments from a named list.
# Based on code in Rbowtie R package.
#
# - Logical list entries are being interpreted as flags
# - All other entries are being collapsed into the form '<entryName>=<entryValue>'
# - Vectors of non-logical entry values will be collapsed into a
#    single comma-separated scalar.
.createFlags <- function(flagList){
    if(!length(flagList)){
        return("")
    }
    if(is.null(names(flagList)) || any(names(flagList)=="")){
        stop("Unable to create command line arguments from input.")
    }
    logFlags <- vapply(flagList, is.logical, TRUE)
    if(any(logFlags)){
        logicalFlags <- .createLogicalFlags(flagList[logFlags])
    } else {
        logicalFlags <- NULL
    }
    nonLogicalFlags <- .createNonLogicalFlags(flagList[!logFlags])
    flags <- paste(logicalFlags,
                   nonLogicalFlags,
                   collapse=" ")
    flags <- gsub("^ *| *$", "", flags)
    return(flags)
}

.createLogicalFlags <- function(flagList){
    fnames <- names(flagList)
    fnames <- fnames[vapply(flagList, "[", 1, FUN.VALUE=TRUE)]
    flags <- vapply(fnames, function(x) {
        ifelse(nchar(x)==1,
               sprintf("-%s", x),
               sprintf("--%s", x))
    }, FUN.VALUE="a")
    flags <- paste(flags, collapse=" ")
    return(flags)
}


.createNonLogicalFlags <- function(flagList){
    fnames <- vapply(names(flagList), function(x){
        ifelse(nchar(x)==1,
               sprintf("-%s", x),
               sprintf("--%s", x))
    }, FUN.VALUE="a")
    flags <- vapply(flagList, paste, collapse=",", FUN.VALUE="a")
    flags <- paste(fnames, flags, collapse=" ", sep=" ")
    return(flags)
}





