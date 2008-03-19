# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-03-19 06:41:21 Graham Williams>
#
# Implement functionality associated with the Execute button and Menu.
#
# Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

on_execute_button_clicked <- function(action, window)
{
  # Wrap up the actual call with a "try" so that the watch cursor
  # turns off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(dispatchExecuteButton())
  set.cursor()
}

dispatchExecuteButton <- function()
{
  # Assign from GLOBAL to avoid "no visible binding" from "R CMD check."

  crv <- crv

  # Check which tab of notebook and dispatch to appropriate execute action

  ct <- getCurrentPageLabel(crv$NOTEBOOK)
  
  if (ct == crv$NOTEBOOK.DATA.NAME) 
  {
    executeDataTab()
  }
  else if (ct == crv$NOTEBOOK.EXPLORE.NAME)
  {
    executeExploreTab()
  }
  else if (ct == crv$NOTEBOOK.SELECT.NAME)
  {
     executeSelectTab()
  }
  else if (ct == crv$NOTEBOOK.TRANSFORM.NAME)
  {
    executeTransformTab()
  }
  else if (ct == crv$NOTEBOOK.CLUSTER.NAME)
  {
    executeClusterTab()
  }
  else if (ct == crv$NOTEBOOK.ASSOCIATE.NAME)
  {
    executeAssociateTab()
  }
  else if (ct == crv$NOTEBOOK.MODEL.NAME)
  {
    executeModelTab()
  }
  else if (ct == crv$NOTEBOOK.EVALUATE.NAME)
  {

    ## The wrap mode of the confusion_textview may have been set to
    ## word wrap when a model was Executed if it had more than 2
    ## classes, since a message is printed about ROCR etc not handling
    ## any more than 2 classes.
    
    theWidget("confusion_textview")$setWrapMode("none")
    executeEvaluateTab()
  }
}
