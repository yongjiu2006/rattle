## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-02-20 18:46:27 Graham>
##
## Implement functionality associated with the Export button and Menu.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

on_export_button_clicked <- function(action, window)
{
  ## Wrap the actual call with a "try" so that the watch cursor turns
  ## off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(dispatchExportButton())
  set.cursor()
}

dispatchExportButton <- function()
{
  
  ## Check which tab of notebook and dispatch to appropriate execute action

  ct <- getCurrentPageLabel(.NOTEBOOK)
  
  if (ct == .NOTEBOOK.EXPLORE.NAME)
  {  
    exportExploreTab()
  }
  else if (ct == .NOTEBOOK.CLUSTER.NAME)
  {  
    exportClusterTab()
  }
  else if (ct == .NOTEBOOK.MODEL.NAME)
  {
    exportModelTab()
  }
  else if (ct == .NOTEBOOK.EVALUATE.NAME)
  {
    exportEvaluateTab()
  }
  else  if (ct == .NOTEBOOK.LOG.NAME) 
  {
    exportLogTab()
  }
  else
    infoDialog("No export functionality is available for the",
               ct, "tab. Nothing done.")
}

exportExploreTab <- function()
{
  if (theWidget("explot_radiobutton")$getActive())
    exportExplotOption()
  else
    infoDialog("No export functionality is available for the",
               "selected option.")
}

exportExplotOption <- function()
{
  if (is.null(dev.list()))
  {
    warnDialog("There are currently no active graphics devices.",
               "So there is nothing to export!",
               "Please select some plot options in the interface",
               "and then Execute (F5).")
    return()
  }

  # Obtain a filename to save to. Ideally, this would also prompt for
  # the device to export, and the fontsize, etc.

  dialog <- gtkFileChooserDialog("Export Graphics (pdf, png, jpg)",
                                 NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_plot.pdf", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("Graphics Files")
  ff$addPattern("*.pdf")
  ff$addPattern("*.png")
  ff$addPattern("*.jpg")
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

  if (get.extension(save.name) == "") save.name <- sprintf("%s.pdf", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("A Graphics file of the name", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  
  cur <- dev.cur()
  ext <- get.extension(save.name)
  if (ext == "pdf")
    dev.copy(pdf, file=save.name, pointsize=10)
  else if (ext == "png")
    dev.copy(png, file=save.name, width=700, height=700)
  else if (ext == "jpg")
    dev.copy(jpeg, file=save.name, width=700, height=700)
  dev.off()
  dev.set(cur)
  
  infoDialog(sprintf("R Graphics: Device %d (ACTIVE)", cur),
             "has been exported to", save.name)
}

########################################################################

exportEvaluateTab <- function()
{
  if (theWidget("risk_radiobutton")$getActive())
    exportRiskOption()
  else
    infoDialog("No export functionality from the Evaluate tab for",
               "the selected option is yet available.")
}

exportRiskOption <- function()
{
  if (is.null(dev.list()))
  {
    warnDialog("There are currently no active graphics devices.",
               "So there is nothing to export!",
               "Please Execute (F5) to obtain a Risk plot to export.")
    return()
  }

  # Obtain a filename to save to. Ideally, this would also prompt for
  # the device to export, and the fontsize, etc.

  dialog <- gtkFileChooserDialog("Export Graphics (pdf, png, jpg)",
                                 NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname),
                                "_riskplot.pdf", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("Graphics Files")
  ff$addPattern("*.pdf")
  ff$addPattern("*.png")
  ff$addPattern("*.jpg")
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

  if (get.extension(save.name) == "") save.name <- sprintf("%s.pdf", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("A Graphics file of the name", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  
  cur <- dev.cur()
  ext <- get.extension(save.name)
  if (ext == "pdf")
    dev.copy(pdf, file=save.name, width=7, height=7)
  else if (ext == "png")
    dev.copy(png, file=save.name, width=700, height=700)
  else if (ext == "jpg")
    dev.copy(jpeg, file=save.name, width=700, height=700)
  dev.off()
  dev.set(cur)
  
  infoDialog(sprintf("R Graphics: Device %d (ACTIVE)", cur),
             "has been exported to", save.name)
}
