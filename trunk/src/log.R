# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-01-31 21:07:15 Graham Williams>
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
  if (isRStat()) resetTextview("log_textview")
  
  startLog(paste(sprintf("%s version %s User %s",
                         crv$appname, crv$VERSION, Sys.info()["user"]),
           #sprintf("# Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
          "\n\n# We can export the contents of this log textview to file using
# the export button or Tools menu. This will save a log of what we have done,
# potentially to repeat the process by sending the same commands directly
# to R. For example, if we export to the file \"model061205.R\" then in the
# R Console we can issue the command 'source(\"model061205.R\")' to run the
# commands in that file. We may want to edit the file to suit our needs.
# We can also directly edit this current log textview to record additional
# information about the work you have done, before exporting the log.
#
# Saving and loading projects also retains this log.

library(rattle)

# The variable crs is used by Rattle to store the Current Rattle State.
# We initialise it here to be empty and Rattle then starts populating it.
# Simply type \"str(crs)\" in the R Console to see a summary of what is
# stored there!

crs <- NULL

# The commands in this log generally record the process of building a model.
# However, with very little effort the log can be used to score a new dataset.
# The variable, building, can be used to toggle between these two. For example
# it is used to toggle between generating transformations, as when building a
# model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building",
                 ifelse(packageIsAvailable("vcd"), "

# The vcd package is used to generate the colours used in plots, if
# it is available.

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
    appendLog(paste(sep="", .START.LOG.COMMENT, msg), no.start=TRUE)
}

appendLog <- function(start, ..., sep=" ", no.start=FALSE)
{
  if (! exists("rattleGUI")) return()
  
  if (no.start)
    msg <- paste(sep=sep, start, ...)
  else
    msg <- paste(sep="", .START.LOG.COMMENT, start, .END.LOG.COMMENT, ...)
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

