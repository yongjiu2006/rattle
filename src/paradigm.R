## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-14 20:46:10 Graham Williams>
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

    ## Add the new tab after the explore tab.
    
    ep <- getNotebookPage(NOTEBOOK, NOTEBOOK.EXPLORE.NAME)
    
    NOTEBOOK$insertPage(NOTEBOOK.MODEL.WIDGET, NOTEBOOK.MODEL.LABEL, ep+1)
    NOTEBOOK$insertPage(NOTEBOOK.EVALUATE.WIDGET,NOTEBOOK.EVALUATE.LABEL,ep+2)

    ## If the previous current page is not one of the common pages,
    ## then make the newly inserted page the current page. This
    ## doesn't work, since if we are coming from a CLuster page, for
    ## example, that page no longer exists, so we get the Explore as
    ## the last page, and thus this does nothing - unless I remove
    ## Explore from the common pages list! Will result in one oddity,
    ## but we might get away with it.

    if (! is.element(crs$page, NOTEBOOK.COMMON.NAMES))
    {
      NOTEBOOK$setCurrentPage(getNotebookPage(NOTEBOOK, NOTEBOOK.MODEL.NAME))
      switchToPage(NOTEBOOK.MODEL.NAME)
    }
  }
  else
  {
    NOTEBOOK$removePage(getNotebookPage(NOTEBOOK, NOTEBOOK.MODEL.NAME))
    NOTEBOOK$removePage(getNotebookPage(NOTEBOOK, NOTEBOOK.EVALUATE.NAME))
  }
    
  setStatusBar()
}

on_unsupervised_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    ## Add the new tab after the explore tab.
    
    ep <- getNotebookPage(NOTEBOOK, NOTEBOOK.EXPLORE.NAME)
    
    NOTEBOOK$insertPage(NOTEBOOK.CLUSTER.WIDGET, NOTEBOOK.CLUSTER.LABEL, ep+1)

    ## If the previous current page is not one of the common pages,
    ## then make the newly inserted page the current page.

    if (! is.element(crs$page, NOTEBOOK.COMMON.NAMES))
    {
      NOTEBOOK$setCurrentPage(getNotebookPage(NOTEBOOK, NOTEBOOK.CLUSTER.NAME))
      switchToPage(NOTEBOOK.CLUSTER.NAME)
    }
    
    setStatusBar("Added the Cluster tab")
  }
  else
  {
    NOTEBOOK$removePage(getNotebookPage(NOTEBOOK, NOTEBOOK.CLUSTER.NAME))
  }
}
