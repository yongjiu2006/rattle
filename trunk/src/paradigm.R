# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-04-27 14:39:06 Graham Williams>
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

askChangeParadigmNotOK <- function()
{
  return(not.null(questionDialog("Changing paradigms may clear some models",
                                 "(Tree and Neural Net).",
                                 "\n\nDo you wish to continue?")))
}

switchToSelect <- function(data.available=not.null(crs$dataset))
{
  # 080413 Always change focus to the Select tab on changing the
  # Paradigm, unless there is no data in which case switch to the Data
  # tab. This is perhaps most logical.

  if (data.available)
  {
    crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                                crv$NOTEBOOK.SELECT.NAME))
    switchToPage(crv$NOTEBOOK.SELECT.NAME)
  }
  else
  {
    crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                                crv$NOTEBOOK.DATA.NAME))
    switchToPage(crv$NOTEBOOK.DATA.NAME)
  }
}

#
# Callbacks
#

on_twoclass_radiobutton_toggled <- function(button)
{
  # This is what is now the Classification radio button.
  
  if (button$getActive())
  {
    # Make sure we want to do this because all current models will be
    # lost.

    # 080427 bad interaction with the other radio button untoggle.
    #if (askChangeParadigmNotOK()) return()

    # Add two new tabs (Model and Evaluate) after the Transform tab.
    
    ep <- getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.TRANSFORM.NAME)
    
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.MODEL.WIDGET,
                            crv$NOTEBOOK.MODEL.LABEL, ep+1)
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.EVALUATE.WIDGET,
                            crv$NOTEBOOK.EVALUATE.LABEL,ep+2)

    # Show the relevent model Type radio buttons for the Model tab.

    theWidget("rpart_radiobutton")$show()
    theWidget("boost_radiobutton")$show()
    theWidget("rf_radiobutton")$show()
    theWidget("svm_radiobutton")$show()
    theWidget("regression_radiobutton")$hide()
    theWidget("nnet_radiobutton")$show()
    theWidget("mars_radiobutton")$hide()
    theWidget("all_models_radiobutton")$show()

    # NULL any models shared between classification and regression.
    
    crs$rpart <<- NULL
    crs$nnet <<- NULL
    setTextview("rpart_textview")
    setTextview("nnet_textview")
    
    # Reset the default model to be the decision tree.
    
    theWidget("rpart_radiobutton")$setActive(TRUE)
    
    # Show the relevent model Type check buttons for the Evaluate tab.

    theWidget("rpart_evaluate_checkbutton")$show()
    theWidget("ada_evaluate_checkbutton")$show()
    theWidget("rf_evaluate_checkbutton")$show()
    theWidget("ksvm_evaluate_checkbutton")$show()
    theWidget("glm_evaluate_checkbutton")$hide()
    theWidget("nnet_evaluate_checkbutton")$show()
    theWidget("mars_evaluate_checkbutton")$hide()
    
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

    switchToSelect()
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
    # Make sure we want to do this because all current models will be
    # lost.

    # 080427 bad interaction with the other radio button untoggle.
    #if (askChangeParadigmNotOK()) return()
    
    # Add the new Model and Evaluate tabs after the Transform tab.
    
    ep <- getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.TRANSFORM.NAME)
    
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.MODEL.WIDGET,
                            crv$NOTEBOOK.MODEL.LABEL, ep+1)
    crv$NOTEBOOK$insertPage(crv$NOTEBOOK.EVALUATE.WIDGET,
                            crv$NOTEBOOK.EVALUATE.LABEL,ep+2)

    # Show the relevent model Type radio buttons for the Model tab.
    
    theWidget("rpart_radiobutton")$show()
    theWidget("boost_radiobutton")$hide()
    theWidget("rf_radiobutton")$hide()
    theWidget("svm_radiobutton")$hide()
    theWidget("regression_radiobutton")$show()
    theWidget("nnet_radiobutton")$show()
    theWidget("mars_radiobutton")$hide()
    theWidget("all_models_radiobutton")$show()
    
    # NULL any models shared between classification and regression.
    
    crs$rpart <<- NULL
    crs$nnet <<- NULL
    setTextview("rpart_textview")
    setTextview("nnet_textview")
    
    # Reset the default model to be the decision tree.
    
    theWidget("rpart_radiobutton")$setActive(TRUE)
    
    # Show the relevent model Type check buttons for the Evaluate tab.

    theWidget("rpart_evaluate_checkbutton")$show()
    theWidget("ada_evaluate_checkbutton")$hide()
    theWidget("rf_evaluate_checkbutton")$hide()
    theWidget("ksvm_evaluate_checkbutton")$hide()
    theWidget("glm_evaluate_checkbutton")$show()
    theWidget("nnet_evaluate_checkbutton")$show()
    theWidget("mars_evaluate_checkbutton")$hide()
    
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

    switchToSelect()
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
    
    switchToSelect()
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
