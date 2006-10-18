## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-19 04:30:25 Graham Williams>
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

  ct <- getCurrentPageLabel(NOTEBOOK)
  
  if (ct == NOTEBOOK.CLUSTER.NAME)
  {  
    ## TODO This should be in cluster.R as exportClusterTab

    if (noDatasetLoaded()) return()
    require(XML, quietly=TRUE)
    if (rattleWidget("kmeans_radiobutton")$getActive())
    {
      if (is.null(crs$kmeans))
      {
        errorDialog("No KMeans cluster is available. Be sure to build",
                     "a cluster before trying to export it! You will need",
                     "to press the Execute button (F5) in order to build the",
                     "KMeans cluster.")
        return()
      }
      else
      {
        write(collectOutput("pmml.kmeans(crs$kmeans)", TRUE),
              file=sprintf("%s-kmeans.pmml", gsub(".csv", "", crs$dataname)))
        infoDialog("The PMML file",
                    sprintf('"%s-kmeans.pmml"', gsub(".csv", "", crs$dataname)),
                    "has been written.")
      }
    }
    else if (rattleWidget("hclust_radiobutton")$getActive())
    {
      errorDialog("PMML export for hierarchical clustering is not yet",
                   "implemented.")
      return()
    }
  }
  else if (ct == NOTEBOOK.MODEL.NAME)
  {
    ## TODO This should be in model.R as exportModelTab

    if (noDatasetLoaded()) return()
    require(XML, quietly=TRUE)
    if (rattleWidget("rpart_radiobutton")$getActive())
    {
      if (is.null(crs$rpart))
      {
        errorDialog("No decision tree model is available. Be sure to build",
                     "the model before trying to export it! You will need",
                     "to press the Execute button (F5) in order to build the",
                     "model.")
        return()
      }
      else
      {
        write(collectOutput("pmml.rpart(crs$rpart)", TRUE),
              file=sprintf("%s-rpart.pmml", gsub(".csv", "", crs$dataname)))
        infoDialog("The PMML file",
                    sprintf('"%s-rpart.pmml"', gsub(".csv", "", crs$dataname)),
                    "has been written.")
      }
    }
    else
    {
      errorDialog("PMML export for this model is not yet implemented.")
      return()
    }
  }
  else  if (ct == NOTEBOOK.LOG.NAME) 
  {
    exportLogTab()
  }
  else
    infoDialog("No export functionality is available for the",
               ct, "tab. Nothing done.")
}

