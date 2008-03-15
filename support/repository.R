library(reposTools)
setwd("repository")

buildPkgDf <- function (PACKAGEfile = "PACKAGES.in", infoDir = ".",
                        prefixPath = NULL, mangSep = ":v:") 
{
  cDir <- getwd()
  on.exit(setwd(cDir))
  setwd(infoDir)
  fields <- c("Package", "Version", "Keywords", "Depends", 
              "Title", "Suggests", "Imports", "Replaces", "Description", 
              "URL", "Author", "Maintainer", "License", "Status", "Priority", 
              "SystemRequirements", "ReleaseLevel", "Contains", "Uses")
  dcfList <- getPackDcfList(PACKAGEfile, fields, mangSep)
  df <- reposDfFromList(dcfList, fields)
  stringVex <- c("Keywords", "Depends", "Suggests", "Imports", 
                 "Replaces")
  df <- splitDFstrings(df, stringVex)
  df <- splitDFstrings(df, "Contains", delim = "[[:space:]]+")
  df <- replaceDFversions(df)
  xFields <- c("OSspecific", "KeySearchOrder")
  xMtrx <- matrix(nrow = nrow(df), ncol = length(xFields))
  colnames(xMtrx) <- xFields
  df <- cbind(df, xMtrx)
  # BEGIN GJW - at least grab the PACAKGES file and install it in right place
  system(sprintf("cp %s /home/gjw/projects/rattle/repository/PACKAGES",
                 paste(infoDir, PACKAGEfile, sep="/")))##
  system(paste("cd /home/gjw/projects/rattle/repository/;",
               "gzip -c PACKAGES > PACKAGES.gz"))
  # END GJW
  df <- addOsSpecific(df, prefixPath) # THIS FAILS
  class(df) <- c(class(df), "Pkg")
  return(df)
}


genRepos("Rattle Repository", "http://rattle.togaware.com/")

repName <- "Rattle Repository"
urlBase <- "http://rattle.togaware.com/"
urlPath <- ""
repType <- "package"
repRelLevel <- "release"
dir <- "."
HTML <- TRUE
functionDesc <- FALSE

genPkgRepos(dir, HTML, functionDesc = functionDesc)

dataDir <- dir

    tmpDir <- tempfile()
    dir.create(tmpDir)
    tmpInfo <- file.path(tmpDir, "info")
    dir.create(tmpInfo)
    curDir <- getwd()
    setwd(dataDir)
    pkgs <- dir(pattern = ".*\\.tar\\.gz|.*\\.tgz|.*\\.zip")
    PACKin <- SrcStat <- WinStat <- NULL
for (pkg  in pkgs) {
        ext <- getExt(pkg)
        DESC <- try(getPackageDescriptionAsMatrix(pkg))
        if (inherits(DESC, "try-error")) {
            badPkg <- paste(pkg, "BAD", sep = ".")
            warning("Skipping ivalid package ", sQuote(pkg), 
                " and renaming to ", sQuote(badPkg))
            file.rename(pkg, badPkg)
            next
        }
        if (!is.null(DESC)) {
            samePack <- which(PACKin[, "Package"] == DESC[, "Package"])
            if ((length(samePack) == 0) || (all(PACKin[samePack, 
                "Version", drop = FALSE] != DESC[, "Version"]))) {
                PACKin <- rbind(PACKin, DESC)
            }
            if (!is.na(DESC[, "Built"])) {
                parts <- strsplit(DESC[, "Built"], "; ")
                pkgRvers <- strsplit(parts[[1]][1], " ")[[1]][2]
                pkgDate <- parts[[1]][3]
            }
            else {
                pkgRvers <- ""
                pkgDate <- date()
            }
            newStatus <- c(DESC[, "Package"], DESC[, "Version"], 
                pkg, "OK", pkgRvers, pkgDate)
            switch(ext, gz = SrcStat <- rbind(SrcStat, newStatus), 
                tgz = SrcStat <- rbind(SrcStat, newStatus), zip = WinStat <- rbind(WinStat, 
                  newStatus))
        }
    }
    if (!is.null(SrcStat)) 
        row.names(SrcStat) <- NULL
    if (!is.null(WinStat)) 
        row.names(WinStat) <- NULL
    fields <- c("Package", "Version", "File", "Status", "Rvers", 
        "Date")
    write.dcf(PACKin, file.path(tmpInfo, "PACKAGES.in"))
    if (!is.null(SrcStat)) {
        colnames(SrcStat) <- fields
        write.dcf(SrcStat, file.path(tmpInfo, "Source.status"))
    }
    if (!is.null(WinStat)) {
        colnames(WinStat) <- fields
        write.dcf(WinStat, file.path(tmpInfo, "Win32.status"))
    }
    df <- buildPkgDf(infoDir = tmpInfo)

PACKAGEfile = "PACKAGES.in"
infoDir <- tmpInfo
prefixPath = NULL
 mangSep = ":v:"

    cDir <- getwd()
    setwd(infoDir)
fields <- c("Package", "Version", "Keywords", "Depends", 
        "Title", "Suggests", "Imports", "Replaces", "Description", 
        "URL", "Author", "Maintainer", "License", "Status", "Priority", 
        "SystemRequirements", "ReleaseLevel", "Contains", "Uses")
    dcfList <- getPackDcfList(PACKAGEfile, fields, mangSep)
    df <- reposDfFromList(dcfList, fields)
    stringVex <- c("Keywords", "Depends", "Suggests", "Imports", 
        "Replaces")
    df <- splitDFstrings(df, stringVex)
    df <- splitDFstrings(df, "Contains", delim = "[[:space:]]+")
    df <- replaceDFversions(df)
    xFields <- c("OSspecific", "KeySearchOrder")
    xMtrx <- matrix(nrow = nrow(df), ncol = length(xFields))
    colnames(xMtrx) <- xFields
    df <- cbind(df, xMtrx)
    df <- addOsSpecific(df, prefixPath)
    class(df) <- c(class(df), "Pkg")
