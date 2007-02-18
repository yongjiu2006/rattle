## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-22 08:45:24 Graham Williams>
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
  
  if (ct == .NOTEBOOK.CLUSTER.NAME)
  {  
    exportClusterTab()
  }
  else if (ct == .NOTEBOOK.MODEL.NAME)
  {
    exportModelTab()
  }
  else  if (ct == .NOTEBOOK.LOG.NAME) 
  {
    exportLogTab()
  }
  else
    infoDialog("No export functionality is available for the",
               ct, "tab. Nothing done.")
}

