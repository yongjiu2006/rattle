## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2008-03-16 22:35:09 Graham Williams>
##
## Implement LOG functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

initiateLog <- function()
{
  startLog(paste(sprintf("Rattle version %s User %s",
                         VERSION, Sys.info()["user"]),
           #sprintf("## Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
          "\n\n## We can export the contents of this log textview to file using
## the export button or Tools menu. This will save a log of what we have done,
## potentially to repeat the process by sending the same commands directly
## to R. For example, if we export to the file \"model061205.R\" then in the
## R Console we can issue the command 'source(\"model061205.R\")' to run the
## commands in that file. We may want to edit the file to suit our needs.
## We can also directly edit this current log textview to record additional
## information about the work you have done, before exporting the log.
##
## Saving and loading projects also retains this log.

library(rattle)

## The variable crs is used by Rattle to store the Current Rattle State.
## We initialise it here to be empty and Rattle then starts populating it.
## Simply type \"str(crs)\" in the R Console to see a summary of what is
## stored there!

crs <- NULL"))

}

startLog <- function(msg=NULL)
{
  # Output a suitable separator to the log textview, and if there is
  # an optional MSG, display that message, as an introduction to this
  # section.
  
  # Assign from GLOBAL to avoid "no visible binding" from "R CMD check."
  
  .START.LOG.COMMENT <- .START.LOG.COMMENT

  appendLog(paste("\n\n##", paste(rep("=", 60), collapse=""),
                "\n## Rattle timestamp: ", Sys.time(), sep=""),
          no.start=TRUE)
  if (not.null(msg))
    appendLog(paste(sep="", .START.LOG.COMMENT, msg), no.start=TRUE)
}

appendLog <- function(start, ..., sep=" ", no.start=FALSE)
{
  # Assign from GLOBAL to avoid "no visible binding" from "R CMD check."
  
  .START.LOG.COMMENT <- .START.LOG.COMMENT
  .END.LOG.COMMENT <- .END.LOG.COMMENT

  if (no.start)
    msg <- paste(sep=sep, start, ...)
  else
    msg <- paste(sep="", .START.LOG.COMMENT, start, .END.LOG.COMMENT, ...)
  if (length(msg) == 0) msg <-""

  ## Always place text at the end, irrespective of where the cursor is.

  log.buf <- theWidget("log_textview")$getBuffer()
  location <- log.buf$getEndIter()$iter

  log.buf$insert(location, msg)
}

exportLogTab <- function()
{
  # Obtain filename to the LOG textview to.
  
  dialog <- gtkFileChooserDialog("Export Log", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname)) dialog$setCurrentName(get.stem(crs$dataname))

  ff <- gtkFileFilterNew()
  ff$setName("R Files")
  ff$addPattern("*.R")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (get.extension(save.name) != "R")
    save.name <- sprintf("%s.R", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("The log file", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  write(getTextviewContent("log_textview"), save.name)

  setStatusBar("The log has been exported to", save.name)

  infoDialog("The log has been exported to", save.name)

}

