## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-28 06:44:59 Graham Williams>
##
## Implement functionality associated with the Execute button and Menu.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

on_execute_button_clicked <- function(action, window)
{
  ## Wrap up the actual call with a "try" so that the watch cursor
  ## turns off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(dispatchExecuteButton())
  set.cursor()
}

dispatchExecuteButton <- function()
{
  
  ## Check which tab of notebook and dispatch to appropriate execute action

  ct <- getCurrentPageLabel(NOTEBOOK)
  
  if (ct == NOTEBOOK.DATA.NAME) 
  {
    executeDataTab()
  }
  else if (ct == NOTEBOOK.EXPLORE.NAME)
  {
    executeExploreTab()
  }
  else if (ct == NOTEBOOK.VARIABLES.NAME)
  {
    executeVariablesTab()
  }
  else if (ct == NOTEBOOK.SAMPLE.NAME)
  {
    executeSampleTab()
  }
  else if (ct == NOTEBOOK.CLUSTER.NAME)
  {
    executeClusterTab()
  }
  else if (ct == NOTEBOOK.ASSOCIATE.NAME)
  {
    executeAssociateTab()
  }
  else if (ct == NOTEBOOK.MODEL.NAME)
  {
    executeModelTab()
  }
  else if (ct == NOTEBOOK.EVALUATE.NAME)
  {

    ## The wrap mode of the confusion_textview may have been set to
    ## word wrap when a model was Executed if it had more than 2
    ## classes, since a message is printed about ROCR etc not handling
    ## any more than 2 classes.
    
    rattleWidget("confusion_textview")$setWrapMode("none")
    executeEvaluateTab()
  }
}
