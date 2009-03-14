# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-03-09 17:25:07 Graham Williams>
#
# Implement functionality associated with the Export button and Menu.
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

on_export_button_clicked <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(dispatchExportButton())
  set.cursor()
}

dispatchExportButton <- function()
{
  # Check which tab of notebook and dispatch to appropriate execute action

  ct <- getCurrentPageLabel(crv$NOTEBOOK)

  if (ct == crv$NOTEBOOK.CLUSTER.NAME)
  {  
    exportClusterTab()
  }
  else if (ct == crv$NOTEBOOK.MODEL.NAME)
  {
    exportModelTab()
  }
  else if (ct == crv$NOTEBOOK.ASSOCIATE.NAME)
  {
    exportAssociateTab()
  }
  else if (ct == crv$NOTEBOOK.DATA.NAME ||
           ct == crv$NOTEBOOK.TRANSFORM.NAME)
  {
    # For any of the DATA, SELECT, or TRANSFORM tabs, the logical
    # thing to EXPORT is the dataset.
    
    exportDataTab()
  }
##   else if (ct == crv$NOTEBOOK.EVALUATE.NAME)
##   {
##     exportEvaluateTab()
##   }
  else  if (ct == crv$NOTEBOOK.LOG.NAME) 
  {
    exportLogTab()
  }
  else
    infoDialog("No export functionality is available for the",
               ct, "tab. Nothing done.")
}

## This is handled by the Cairo device save button now. Might want to
## interpret Export differently for these now.

## exportExploreTab <- function()
## {
##   if (theWidget("explot_radiobutton")$getActive())
##     exportPlot("dist")
##   else if (theWidget("correlation_radiobutton")$getActive())
##     exportPlot("corr")
##   else if (theWidget("hiercor_radiobutton")$getActive())
##     exportPlot("hiercorr")
##   else if (theWidget("prcomp_radiobutton")$getActive())
##     exportPlot("prcomp")
##   else
##     infoDialog("No export functionality is available for the",
##                "selected option.")
## }

## ########################################################################

## exportEvaluateTab <- function()
## {
##   if (theWidget("risk_radiobutton")$getActive())
##     exportPlot("risk")
##   else if (theWidget("lift_radiobutton")$getActive())
##     exportPlot("lift")
##   else if (theWidget("roc_radiobutton")$getActive())
##     exportPlot("roc")
##   else if (theWidget("precision_radiobutton")$getActive())
##     exportPlot("precision")
##   else if (theWidget("sensitivity_radiobutton")$getActive())
##     exportPlot("sensitivity")
##   else
##     infoDialog("No export functionality from the Evaluate tab for",
##                "the selected option is yet available.")
## }

## exportPlot <- function(type="plot", devices=NULL)
## {
##   if (is.null(dev.list()))
##   {
##     warnDialog("There are currently no active graphics devices.",
##                "So there is nothing to export!",
##                "Please Execute (F2) to obtain a plot to export.")
##     return()
##   }

##   # Obtain a filename to save to. Ideally, this would also prompt for
##   # the device to export, and the fontsize, etc.

##   dialog <- gtkFileChooserDialog("Export Graphics (pdf, png, jpg)",
##                                  NULL, "save",
##                                  "gtk-cancel", GtkResponseType["cancel"],
##                                  "gtk-save", GtkResponseType["accept"])

##   if(not.null(crs$dataname))
##     dialog$setCurrentName(paste(get.stem(crs$dataname),
##                                 "_", type, ".pdf", sep=""))

##   ff <- gtkFileFilterNew()
##   ff$setName("Graphics Files")
##   ff$addPattern("*.pdf")
##   ff$addPattern("*.png")
##   ff$addPattern("*.jpg")
##   dialog$addFilter(ff)

##   ff <- gtkFileFilterNew()
##   ff$setName("All Files")
##   ff$addPattern("*")
##   dialog$addFilter(ff)
  
##   if (dialog$run() == GtkResponseType["accept"])
##   {
##     save.name <- dialog$getFilename()
##     dialog$destroy()
##   }
##   else
##   {
##     dialog$destroy()
##     return()
##   }

##   if (get.extension(save.name) == "") save.name <- sprintf("%s.pdf", save.name)
    
##   if (file.exists(save.name))
##     if ( ! questionDialog("A Graphics file of the name", save.name,
##                                 "already exists. Do you want to overwrite",
##                                 "this file?"))
##       return()
  
##   cur <- dev.cur()
##   ext <- get.extension(save.name)
##   if (ext == "pdf")
##     dev.copy(pdf, file=save.name, width=7, height=7)
##   else if (ext == "png")
##     dev.copy(png, file=save.name, width=700, height=700)
##   else if (ext == "jpg")
##     dev.copy(jpeg, file=save.name, width=700, height=700)
##   dev.off()
##   dev.set(cur)
  
##   infoDialog(sprintf("R Graphics: Device %d (ACTIVE)", cur),
##              "has been exported to", save.name)
## }
  
getExportSaveName <- function(mtype)
{
  # 090117 Request a filename to save the model to and return as a
  # string that filename.

  # Require the pmml package for either exporting to PMML or C (via
  # PMML).
  
  lib.cmd <- "require(pmml, quietly=TRUE)"
  if (! (exists("pmml") ||
         packageIsAvailable("pmml", paste("export a", commonName(mtype), "model"))))
      return(NULL)
  appendLog("Load the PMML package to export a model.", lib.cmd)
  # Load the package unless we already have a pmml defined (through source).
  if (! exists("pmml")) eval(parse(text=lib.cmd))

  # Obtain filename to write the PMML or C code to.  081218 We use the
  # glade generated filechooser rather than my original hand-coded
  # one. It is much simpler to handle the formatting. It has been
  # modified to offer a choice of Class/Score and PMML/Info to the C
  # file.

  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    dialogGUI <- gladeXMLNew("rattle.glade",
                             root="export_filechooserdialog")
  else
    dialogGUI <- gladeXMLNew(file.path(etc,"rattle.glade"),
                             root="export_filechooserdialog")

  if (! crv$export.to.c.available)
    dialogGUI$getWidget("export_filechooser_options_table")$hide()

  dialog <- dialogGUI$getWidget("export_filechooserdialog")

  if (crv$export.to.c.available) dialog$setTitle("Export C or PMML")

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_", mtype,
                                ifelse(crv$export.to.c.available, ".c", ".xml"),
                                sep=""))

  if (crv$export.to.c.available)
  {
    ff <- gtkFileFilterNew()
    ff$setName("C Files")
    ff$addPattern("*.c")
    dialog$addFilter(ff)
  }

  ff <- gtkFileFilterNew()
  ff$setName("PMML Files")
  ff$addPattern("*.xml")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)

  if (crv$export.to.c.available)
  {
    if (mtype %in% c("glm"))
      dialogGUI$
      getWidget("export_filechooser_probabilities_radiobutton")$setActive(TRUE)
    
    # 081218 Add glm when implemented.
    
    if (!binomialTarget() || ! mtype %in% c("rpart", "glm"))
    {
      dialogGUI$
      getWidget("export_filechooser_target_label")$setSensitive(FALSE)

      dialogGUI$
      getWidget("export_filechooser_class_radiobutton")$setSensitive(FALSE)

      dialogGUI$
      getWidget("export_filechooser_probabilities_radiobutton")$setSensitive(FALSE)
    }
  }

  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    save.type <- dialog$getFilter()$getName()
    if (crv$export.to.c.available)
    {
      includePMML <- dialogGUI$
      getWidget("export_filechooser_pmml_checkbutton")$getActive()

      includeMetaData <- dialogGUI$
      getWidget("export_filechooser_metadata_checkbutton")$getActive()

      exportClass <- dialogGUI$
      getWidget("export_filechooser_class_radiobutton")$getActive()
    }
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return(NULL)
  }

  # 081222 Maybe assume now that we need to get an extension specified
  # by the user - don't do things behind their back.
###   if (get.extension(save.name) == "")
###   {
###     if (save.type == "C Files")
###       save.name <- sprintf("%s.c", save.name)
###     else
###     {
###       if (save.type == "PMML Files")
###         save.name <- sprintf("%s.xml", save.name)
###       else if (isRStat())
###         save.name <- sprintf("%s.c", save.name)
###       else
###         save.name <- sprintf("%s.xml", save.name)
###     }
###   }

  ext <- tolower(get.extension(save.name))

  ## if (ext == "c" && length(getAnywhere("pmmltoc")$where) == 0)
  ## {
  ##   errorDialog("The PMMLtoC functionality does not appear to be available.",
  ##               "This function needs to be loaded.",
  ##               if (isRattle())
  ##               paste("It is not available in Rattle by default.",
  ##                     "\n\n", crv$support.msg))
  ##   return(NULL)
  ## }

  if (crv$export.to.c.available)
  {
    attr(save.name, "includePMML") <- includePMML
    attr(save.name, "includeMetaData") <- includeMetaData
    attr(save.name, "exportClass") <- exportClass
  }
  
  return(save.name)
}


