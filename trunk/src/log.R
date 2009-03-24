# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-03-24 18:02:33 Graham Williams>
#
# Implement LOG functionality.
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# This files is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

initiateLog <- function()
{
  if (! is.null(crv$log.intro))
    appendTextview("log_textview", crv$log.intro, tvsep=FALSE)

  startLog(paste(sprintf("%s version %s user '%s'",
                         crv$appname, crv$version, Sys.info()["user"]),
           #sprintf("# Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
          "\n\n# Export this log textview to a file using the Export button or the Tools
# menu to save a log of all activity. This facilitates repeatability. Exporting
# to file 'myrf01.R', for example, allows us to the type in the R Console
# the command 'source(\"myrf01.R\")' to repeat the process automatically.
# Generally, we may want to edit the file to suit our needs. We can also directly
# edit this current log textview to record additional information before exporting.
#
# Saving and loading projects also retains this log.

",
                 crv$library.command,
                 "

# This log generally records the process of building a model. However, with very
# little effort the log can be used to score a new dataset. The logical variable
# 'building' is used to toggle between generating transformations, as when building
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building",
                 ifelse(packageIsAvailable("vcd"), "

# The vcd package is used to generate the colours used in plots, if available.

library(vcd)", "")))

}

startLog <- function(msg=NULL)
{
  # Output a suitable separator to the log textview, and if there is
  # an optional MSG, display that message, as an introduction to this
  # section.
  
  if (! exists("rattleGUI")) return()

  appendLog(paste("\n\n#", paste(rep("=", 60), collapse=""),
                  "\n# ", crv$appname, " timestamp: ", Sys.time(), sep=""),
          no.start=TRUE)
  if (not.null(msg))
    appendLog(paste(sep="", crv$start.log.comment, msg), no.start=TRUE)
}

appendLog <- function(start, ..., sep=" ", no.start=FALSE)
{
  if (! exists("rattleGUI")) return()
  
  if (no.start)
    msg <- paste(sep=sep, start, ...)
  else
    msg <- paste(sep="", crv$start.log.comment, start, crv$end.log.comment, ...)
  if (length(msg) == 0) msg <-""

  # Always place text at the end, irrespective of where the cursor is.

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
  dialog$setDoOverwriteConfirmation(TRUE)

  if(not.null(crs$dataname))
    dialog$setCurrentName(sprintf("%s_script.R", get.stem(crs$dataname)))

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

  write(getTextviewContent("log_textview"), save.name)

  setStatusBar("The log has been exported to", save.name)
}

