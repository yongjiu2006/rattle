## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-03-26 19:21:46 Graham>
##
## Paradigm control.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

##
## Callbacks
##

on_twoclass_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    ## Add the new tab after the transform tab.
    
    ep <- getNotebookPage(.NOTEBOOK, .NOTEBOOK.TRANSFORM.NAME)
    
    .NOTEBOOK$insertPage(.NOTEBOOK.MODEL.WIDGET, .NOTEBOOK.MODEL.LABEL, ep+1)
    .NOTEBOOK$insertPage(.NOTEBOOK.EVALUATE.WIDGET,.NOTEBOOK.EVALUATE.LABEL,ep+2)

    ## If the previous current page is not one of the common pages,
    ## then make the newly inserted page the current page. This
    ## doesn't work, since if we are coming from a CLuster page, for
    ## example, that page no longer exists, so we get the Explore as
    ## the last page, and thus this does nothing - unless I remove
    ## Explore from the common pages list! Will result in one oddity,
    ## but we might get away with it.

    if (crs$page != "" && crs$page %notin% .NOTEBOOK.COMMON.NAMES)
    {
      .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.MODEL.NAME))
      switchToPage(.NOTEBOOK.MODEL.NAME)
    }
    setStatusBar("Exposed the Model and Evaluate tabs")
  }
  else
  {
    .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.MODEL.NAME))
    .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.EVALUATE.NAME))
  }
}

on_unsupervised_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    ## Add the new tab after the transform tab.
    
    ep <- getNotebookPage(.NOTEBOOK, .NOTEBOOK.TRANSFORM.NAME)
    
    .NOTEBOOK$insertPage(.NOTEBOOK.CLUSTER.WIDGET, .NOTEBOOK.CLUSTER.LABEL,
                         ep+1)
    .NOTEBOOK$insertPage(.NOTEBOOK.ASSOCIATE.WIDGET, .NOTEBOOK.ASSOCIATE.LABEL,
                         ep+2)

    ## If the previous current page is not one of the common pages,
    ## then make the newly inserted page the current page.

    if (crs$page != "" && crs$page %notin% .NOTEBOOK.COMMON.NAMES)
    {
      .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.CLUSTER.NAME))
      switchToPage(.NOTEBOOK.CLUSTER.NAME)
    }
    
    setStatusBar("Exposed the Cluster and Associate tabs")
  }
  else
  {
    .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.CLUSTER.NAME))
    .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.ASSOCIATE.NAME))
  }
}
