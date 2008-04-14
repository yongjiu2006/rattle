# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-04-14 21:30:15 Graham Williams>
#
# Paradigm control.
#
# Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

getParadigm <- function()
{
  if (theWidget("regression_paradigm_radiobutton")$getActive())
    return("regression")
  else if (theWidget("twoclass_radiobutton")$getActive())
    return("classification")
  else if (theWidget("unsupervised_radiobutton")$getActive())
    return("unsupervised")
}

#
# Callbacks
#

on_twoclass_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    # Add the new tab after the transform tab.
    
    ep <- getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.TRANSFORM.NAME)
    
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.MODEL.WIDGET,
                            crv$NOTEBOOK.MODEL.LABEL, ep+1)
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.EVALUATE.WIDGET,
                            crv$NOTEBOOK.EVALUATE.LABEL,ep+2)

    # If the previous current page is not one of the common pages,
    # then make the newly inserted page the current page. This
    # doesn't work, since if we are coming from a CLuster page, for
    # example, that page no longer exists, so we get the Explore as
    # the last page, and thus this does nothing - unless I remove
    # Explore from the common pages list! Will result in one oddity,
    # but we might get away with it.

##     if (crs$page != "" && crs$page %notin% crv$NOTEBOOK.COMMON.NAMES)
##     {
##       crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
##                                                   crv$NOTEBOOK.MODEL.NAME))
##       switchToPage(crv$NOTEBOOK.MODEL.NAME)
##     }

    # 080413 Always change focus to the Data tab on changing the
    # Paradigm. This is perhaps most logical.

    crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                                crv$NOTEBOOK.SELECT.NAME))
    switchToPage(crv$NOTEBOOK.SELECT.NAME)
    
    setStatusBar("Exposed the Model and Evaluate tabs")
  }
  else
  {
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.MODEL.NAME))
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.EVALUATE.NAME))
  }
}

on_regression_paradigm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    # Add the new tabs for REGRESSION after the TRANSFORM tab.
    
    ep <- getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.TRANSFORM.NAME)
    
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.MODEL.WIDGET,
                            crv$NOTEBOOK.MODEL.LABEL, ep+1)
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.EVALUATE.WIDGET,
                            crv$NOTEBOOK.EVALUATE.LABEL,ep+2)

    # If the previous current page is not one of the common pages,
    # then make the newly inserted page the current page. This doesn't
    # work, since if we are coming from a Cluster page, for example,
    # that page no longer exists, so we get the Explore as the last
    # page, and thus this does nothing - unless I remove Explore from
    # the common pages list! Will result in one oddity, but we might
    # get away with it.

##     if (crs$page != "" && crs$page %notin% crv$NOTEBOOK.COMMON.NAMES)
##     {
##       crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
##                                                   crv$NOTEBOOK.MODEL.NAME))
##       switchToPage(crv$NOTEBOOK.MODEL.NAME)
##     }

    # 080413 Always change focus to the Select tab on changing the
    # Paradigm. This is perhaps most logical.

    crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                                crv$NOTEBOOK.SELECT.NAME))
    switchToPage(crv$NOTEBOOK.SELECT.NAME)

    setStatusBar("Exposed the Model and Evaluate tabs")
  }
  else
  {
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.MODEL.NAME))
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.EVALUATE.NAME))
  }
}

on_unsupervised_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {

    # Add the new tab after the transform tab.
    
    ep <- getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.TRANSFORM.NAME)
    
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.CLUSTER.WIDGET,
                            crv$NOTEBOOK.CLUSTER.LABEL,
                            ep+1)
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.ASSOCIATE.WIDGET,
                            crv$NOTEBOOK.ASSOCIATE.LABEL,
                            ep+2)

    # If the previous current page is not one of the common pages,
    # then make the newly inserted page the current page.

##     if (crs$page != "" && crs$page %notin% crv$NOTEBOOK.COMMON.NAMES)
##     {
##       crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
##                                                   crv$NOTEBOOK.CLUSTER.NAME))
##       switchToPage(crv$NOTEBOOK.CLUSTER.NAME)
##     }
    
    # 080413 Always change focus to the Data tab on changing the
    # Paradigm. This is perhaps most logical.

    crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                                crv$NOTEBOOK.SELECT.NAME))
    switchToPage(crv$NOTEBOOK.SELECT.NAME)

    setStatusBar("Exposed the Cluster and Associate tabs")
  }
  else
  {
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.CLUSTER.NAME))
    crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK,
                                            crv$NOTEBOOK.ASSOCIATE.NAME))
  }
}
