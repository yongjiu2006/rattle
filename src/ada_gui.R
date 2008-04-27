## RattleGUI TwoClass Ada
##
## This is a model "module" for the rattle GUI interface
##
## Time-stamp: <2008-04-27 11:59:51 Graham Williams>
##
## Copyright (c) 2007 Togaware, GPL Version 2
##
## Below we define the Rattle GUI specific functions for controlling
## ada modelling. The interface callbacks make calls to R functions,
## so that we do not need to restart the application after changing
## the code. This is only of benefit in development, and in production
## requires an extra function call, but that is a small overhead for
## the added debugging efficiencies.

## BUTTONS

on_ada_stumps_button_clicked <- function(button)
# Used to have this as a button, but Frank suggested a checkbutton
# which, when selected, greys out the options that it affects! Seems
# like a good idea. I have changed the interface from a button to a
# checkbutton.
{
  setGuiDefaultsAda(stumps=TRUE)
}

on_ada_stumps_checkbutton_toggled <- function(button)
{
  if (theWidget("ada_stumps_checkbutton")$getActive())
    setGuiDefaultsAda(stumps=TRUE)
  else
    setGuiDefaultsAda()
}

on_ada_defaults_button_clicked <- function(button)
{
  setGuiDefaultsAda()
}

on_ada_importance_button_clicked <- function(button)
{
  plotImportanceAda()
}

on_ada_errors_button_clicked <- function(button)
{
  plotErrorsAda()
}

on_ada_list_button_clicked <- function(button)
{
  listTreesAdaGui()
}

on_ada_draw_button_clicked <- function(button)
{
  drawTreesAdaGui()
}

on_ada_continue_button_clicked <- function(button)
{
  continueModelAdaGui()
}

## HELP

on_help_ada_activate <- function(action, window)
{
  displayHelpAda()
}

## IMPLEMENTATION

listTreesAdaGui <- function()
{
  ## Initial setup. 
  
  TV <- "ada_textview"

  ## Obtain user interface options.

  tree.num <- theWidget("ada_draw_spinbutton")$getValue()

  ## Make sure we have that many trees.

  if (tree.num > length(crs$ada$model$trees))
  {
    errorDialog(sprintf("You have requested tree number %d,", tree.num),
                "but there are only", length(crs$ada$model$trees),
                "trees in the model.",
                "Choose a tree number between 1",
                sprintf("and %d.", length(crs$ada$model$trees)))
    return(FALSE)
  }
  
  ## Command to run.

  display.cmd <- sprintf("listTreesAda(crs$ada, %d)", tree.num)

  ## Perform the action.

  appendLog(sprintf("Display tree number %d.", tree.num), display.cmd)
  addTextview(TV, collectOutput(display.cmd, TRUE), textviewSeparator())
  setStatusBar(paste("Tree", tree.num, "has been added to the textview.",
                     "You may need to scroll the textview to see it."))
}

drawTreesAdaGui <- function()
{
  # Obtain user interface options.

  tree.num <- theWidget("ada_draw_spinbutton")$getValue()

  # Make sure we have that many trees.

  if (tree.num > length(crs$ada$model$trees))
  {
    errorDialog(sprintf("You have requested tree number %d,", tree.num),
                "but there are only", length(crs$ada$model$trees),
                "trees in the model.",
                "Choose a tree number between 1",
                sprintf("and %d.", length(crs$ada$model$trees)))
    return(FALSE)
  }
  
  # Command to run.

  draw.cmd <- sprintf('drawTreesAda(crs$ada, %d, ": %s")', tree.num,
                      paste(crs$dataname, "$", crs$target))

  # Perform the action.

  appendLog(sprintf("Display tree number %d.", tree.num), draw.cmd)
  eval(parse(text=draw.cmd))
  setStatusBar("Tree", tree.num, "has been drawn.")
}

setGuiDefaultsAda <- function(stumps=FALSE)
{
  if (stumps)
  {
    theWidget("ada_maxdepth_spinbutton")$setValue(1)
    theWidget("ada_minsplit_spinbutton")$setValue(0)
    theWidget("ada_cp_spinbutton")$setValue(-1)
    theWidget("ada_xval_spinbutton")$setValue(0)
    # If we have chosen stumps, then stick with stumps and don't allow
    # the user to change the values. This makes sense from a GUI point
    # of view, where we have changed from a button to a checkbutton in
    # the GUI.
    theWidget("ada_max_depth_label")$setSensitive(FALSE)
    theWidget("ada_min_split_label")$setSensitive(FALSE)
    theWidget("ada_complexity_label")$setSensitive(FALSE)
    theWidget("ada_xval_label")$setSensitive(FALSE)
    theWidget("ada_maxdepth_spinbutton")$setSensitive(FALSE)
    theWidget("ada_minsplit_spinbutton")$setSensitive(FALSE)
    theWidget("ada_cp_spinbutton")$setSensitive(FALSE)
    theWidget("ada_xval_spinbutton")$setSensitive(FALSE)
  }
  else
  {
    theWidget("ada_maxdepth_spinbutton")$setValue(30)
    theWidget("ada_minsplit_spinbutton")$setValue(20)
    theWidget("ada_cp_spinbutton")$setValue(0.01)
    theWidget("ada_xval_spinbutton")$setValue(10)
    # These may have been made not sensitive through choosing the
    # Stumps option.
    theWidget("ada_max_depth_label")$setSensitive(TRUE)
    theWidget("ada_min_split_label")$setSensitive(TRUE)
    theWidget("ada_complexity_label")$setSensitive(TRUE)
    theWidget("ada_xval_label")$setSensitive(TRUE)
    theWidget("ada_maxdepth_spinbutton")$setSensitive(TRUE)
    theWidget("ada_minsplit_spinbutton")$setSensitive(TRUE)
    theWidget("ada_cp_spinbutton")$setSensitive(TRUE)
    theWidget("ada_xval_spinbutton")$setSensitive(TRUE)
  }
}

showModelAdaExists <- function(state=!is.null(crs$ada))
{
  # If an ada model exists then show the relevant buttons that require
  # the model to exists.
  
  if (state)
  {
    theWidget("ada_importance_button")$show()
    theWidget("ada_importance_button")$setSensitive(TRUE)
    theWidget("ada_errors_button")$show()
    theWidget("ada_errors_button")$setSensitive(TRUE)
    theWidget("ada_list_button")$show()
    theWidget("ada_list_button")$setSensitive(TRUE)
    theWidget("ada_draw_button")$show()
    theWidget("ada_draw_button")$setSensitive(TRUE)
    theWidget("ada_continue_button")$show()
    theWidget("ada_continue_button")$setSensitive(TRUE)
    theWidget("ada_draw_spinbutton")$show()
    theWidget("ada_draw_spinbutton")$setSensitive(TRUE)
  }
  else
  {
    theWidget("ada_importance_button")$hide()
    theWidget("ada_errors_button")$hide()
    theWidget("ada_list_button")$hide()
    theWidget("ada_draw_button")$hide()
    theWidget("ada_continue_button")$hide()
    theWidget("ada_draw_spinbutton")$hide()
  }    
}

continueModelAdaGui <- function()
{
  ## Extract the new iter from the GUI

  niter <- theWidget("ada_ntree_spinbutton")$getValue()

  ## If the number of iterations has not changed, or is smaller, do
  ## nothing.

  if (niter <= crs$ada$iter)
  {
    infoDialog(sprintf("The new Number of Trees, %d, is no larger", niter),
               sprintf("than the old Number of Trees, %d.", crs$ada$iter),
               "Thus there is nothing to do.")
    return()
  }

  ## Check each of the other parameters to check if any of them have
  ## changed. If so, inform the user of the original value (as in
  ## crs$ada) and do not proceed.
  
  set.cursor("watch")
  continueModelAda(niter)
  set.cursor()
  
}

