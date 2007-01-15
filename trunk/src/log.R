## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-28 09:40:12 Graham Williams>
##
## Implement LOG functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

addInitialLogMessage <- function()
{
  addToLog(sprintf("Rattle version %s", VERSION),
           sprintf("## Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
          "## We can export the contents of this log textview to file using
## the export button or menu. This will save a log of what we have done,
## potentially to repeat the process by sending the same commands directly
## to R. For example, if we export to the file \"model061205.R\" then in the
## R Console we can issue the command 'source(\"model061205.R\")' to run the
## commands in that file. We may want to edit the file to suit our needs.
## We can also directly edit this actual log textview to record additional
## information directly, before exporting.
##
## Saving and loading projects also retains this log.

library(rattle)

## The variable crs is used by Rattle to store the Current Rattle State.
## We initialise it here to be empty and Rattle then starts populating it.
## Simply type \"str(crs)\" in the R Console to see a summary of what is
## stored there!

crs <- NULL")

}

exportLogTab <- function()
{
  ## Obtain filename to the LOG textview to.
  
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

addLogSeparator <- function(msg=NULL)
{
  ## Output a suitable separator to the log textview, and if there is
  ## an optional MSG, display that message, as an introduction to this
  ## section.
  
  addToLog(paste("\n\n##", paste(rep("=", 60), collapse=""),
                "\n## Rattle timestamp: ", Sys.time(), sep=""),
          no.start=TRUE)
  if (not.null(msg))
    addToLog(paste(sep="", START.LOG.COMMENT, msg), no.start=TRUE)
}

addToLog <- function(start, ..., sep=" ", no.start=FALSE)
{
  if (no.start)
    msg <- paste(sep=sep, start, ...)
  else
    msg <- paste(sep="", START.LOG.COMMENT, start, END.LOG.COMMENT, ...)
  if (length(msg) == 0) msg <-""

  ## Always place text at the end, irrespective of where the cursor is.

  log.buf <- theWidget("log_textview")$getBuffer()
  location <- log.buf$getEndIter()$iter

  log.buf$insert(location, msg)
}

