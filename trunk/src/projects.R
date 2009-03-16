# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-03-16 23:00:47 Graham Williams>
#
# Project functionality.
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# This files is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

########################################################################
# CALLBACKS

on_new_activate <- function(action, window)     { newProject()  }
on_new_button_clicked <- function(action, window)  { newProject() }

on_open_activate <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(loadProject())
  set.cursor()
}

on_save_menu_activate <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(saveProject())
  set.cursor()
}

on_save_as_activate <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(saveProject())
  set.cursor()
}

on_open_button_clicked <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(loadProject())
  set.cursor()
}

on_save_button_clicked <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(saveProject())
  set.cursor()
}

########################################################################
# NEW PROJECT

newProject <- function()
{
  if ( not.null(listBuiltModels()) )
  {
    if (! questionDialog("You have requested to start a new project.",
                         "\n\nThis will clear the current project (dataset",
                         "and models).",
                         "\n\nIf you choose not to continue you can save",
                         "the project, and then start a new project.",
                         "\n\nDo you wish to continue, and overwrite the",
                         "current project?"
                         ))
      return()
  }

  resetRattle()
  
  # Ensure data sources are enabled again.
  
  enableDataSourceFunctions()

  # Reset things that can't be done in resetRattle()

  # 090314 I was finding that the filechooserbutton was being set to a
  # folder with the setFilename, so try using unselectFilename instead.
  
  #theWidget("data_filechooserbutton")$setFilename("")
  filename <- theWidget("data_filechooserbutton")$getFilename()
  theWidget("data_filechooserbutton")$unselectFilename(filename)
  
  theWidget("data_name_combobox")$setActive(-1)

  # TODO Plenty of other things that should be reset as well.

  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.DATA.NAME))
  switchToPage(crv$NOTEBOOK.DATA.NAME)
  
}

enableDataSourceFunctions <- function(enable=TRUE)
{
  # 080707 Turn the data source options on/off. This is used when
  # loading a project to avoid datasets being loaded. Logically we
  # don't load a new dataset if we have loaded a project. We can
  # always click New to then load a new dataset.
  
  widgets <- c("data_type_label", "data_csv_radiobutton", "data_arff_radiobutton",
                "data_rdata_radiobutton", "data_rdataset_radiobutton",
                "data_library_radiobutton", "data_odbc_radiobutton",
                "data_corpus_radiobutton")
  for (w in widgets) theWidget(w)$setSensitive(enable)
}

########################################################################
# SAVE PROJECT

saveProject <- function()
{

  # Pre-conditions
  
  if (noDatasetLoaded()) return()
  if (variablesHaveChanged("saving the project")) return()

  # Obtain filename to save to
  
  dialog <- gtkFileChooserDialog("Save Project", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])
  dialog$setDoOverwriteConfirmation(TRUE)
  
  dialog$setCurrentName(get.stem(crs$dataname))
  if (! is.null(crs$pwd)) dialog$setCurrentFolder(crs$pwd)

  if (length(crv$project.extensions))
  {
    ff <- gtkFileFilterNew()
    ff$setName("Projects")
    lapply(paste("*.", crv$project.extensions, sep=""), ff$addPattern)
    dialog$addFilter(ff)
  }
 
  ff <- gtkFileFilterNew()
  ff$setName("RData Files")
  ff$addPattern("*.Rdata")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    save.ext <- get.extension(save.name)
    filter.name <- dialog$getFilter()$getName()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  # 081111 Deal with the filename extension. We add an extension of
  # either rattle or Rdata (or other extensions that might be listed
  # in crv$project.extensions) if the save.name does not have one
  # already. The logic of which to add depends on whether one of the
  # filters is active, or else the first extension in the list
  # crv$project.extensions.
  
  if (! save.ext %in% c(crv$project.extensions, "Rdata"))
  {
    if (filter.name == "Projects")
    {
      if (save.ext != crv$project.extensions[1])
        save.name <- sprintf("%s.%s", save.name, crv$project.extensions[1])
    }
    else if (filter.name == "RData Files")
    {
      if (save.ext != "Rdata")
        save.name <- sprintf("%s.Rdata", save.name)
    }
    else
    {
      if (save.ext != crv$project.extensions[1])
        save.name <- sprintf("%s.%s", save.name, crv$project.extensions[1])
    }
  }
  
  # Save the filename to restore on reloading.

  crs$filename <- theWidget("data_filechooserbutton")$getUri()

  # Save all of the text views to be restored on a load.
  # Put the following into a function and call for each textview.

  crs$text$summary <- getTextviewContent("summary_textview")
  crs$text$correlation <- getTextviewContent("correlation_textview")
  crs$text$prcomp <- getTextviewContent("prcomp_textview")
  crs$text$kmeans <- getTextviewContent("kmeans_textview")
  crs$text$rpart <-  getTextviewContent("rpart_textview")
  crs$text$rf <-  getTextviewContent("rf_textview")
  crs$text$esvm <-  getTextviewContent("esvm_textview")
  crs$text$ksvm <-  getTextviewContent("ksvm_textview")
  crs$text$glm <-  getTextviewContent("glm_textview")
  crs$text$ada <-  getTextviewContent("ada_textview")
  #crs$text$gbm <-  getTextviewContent("gbm_textview")
  crs$text$risk <- getTextviewContent("risk_textview")
  crs$text$roc <- getTextviewContent("roc_textview")
  crs$text$log <- getTextviewContent("log_textview")

  # Save Transform variables selections - not sure it is really
  # needed, but follow the template!

  crs$zero <- getSelectedVariables("zero")
  
  # Save Distribution variable selections

  crs$boxplot <- getSelectedVariables("boxplot")
  crs$hisplot <- getSelectedVariables("hisplot")
  crs$cumplot <- getSelectedVariables("cumplot")
  crs$benplot <- getSelectedVariables("benplot")
  crs$barplot <- getSelectedVariables("barplot")
  crs$dotplot <- getSelectedVariables("dotplot")

  # Save seed information

  crs$sample.seed <- theWidget("sample_seed_spinbutton")$getValue()
  crs$kmeans.seed <- theWidget("kmeans_seed_spinbutton")$getValue()
  
  # Save Model options

  if (not.null(crs$rpart))
  {
    crs$rpart.opt$priors <- theWidget("model_tree_priors_entry")$getText()
    crs$rpart.opt$loss   <- theWidget("model_tree_loss_entry")$getText()
    crs$rpart.opt$split  <- theWidget("rpart_minsplit_spinbutton")$getValue()
    crs$rpart.opt$depth  <- theWidget("rpart_maxdepth_spinbutton")$getValue()
    crs$rpart.opt$cp     <- theWidget("model_tree_cp_spinbutton")$getValue()
    crs$rpart.opt$bucket <- theWidget("rpart_minbucket_spinbutton")$getValue()
  }

  if (not.null(crs$rf))
  {
    crs$rf.opt$trees     <- theWidget("rf_ntree_spinbutton")$getValue()
    crs$rf.opt$vars      <- theWidget("rf_mtry_spinbutton")$getValue()
    crs$rf.opt$sample    <- theWidget("rf_sampsize_entry")$getText()
    crs$rf.opt$proximity <- theWidget("rf_proximity_checkbutton")$getActive()
  }
    
  crs$svm.opt$kernel <- theWidget("svm_kernel_comboboxentry")$getActive()

  set.cursor("watch")
  startLog()
  appendLog("Saved the project data (variable crs) to file.",
           sprintf('save(crs, file="%s", compress=TRUE)', save.name))
  save(crs, file=save.name, compress=TRUE)
  set.cursor()

  # Record the cwd for projects.
  
  crs$pwd <- dirname(save.name)
  
  setStatusBar("The current project has been saved to", save.name)
}

loadProject <- function()
{
  # Check if crs exists and if so warn about losing the current project.

  if ( not.null(listBuiltModels()) )
  {
    if (! questionDialog("You have chosen to load a project.\n\n",
                         "This will clear the old project (dataset and",
                         "models) which may not have been saved.",
                         "If you choose not to continue you can save",
                         "the project, and then load the new project.",
                         "\n\n",
                         "Do you wish to continue, and overwrite the",
                         "current project?"
                         ))
        
      return()
  }

  # Request the rattle filename to be loaded

  dialog <- gtkFileChooserDialog("Open Project", NULL, "load",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-open", GtkResponseType["accept"])

  if (! is.null(crs$pwd)) dialog$setCurrentFolder(crs$pwd)

  if (length(crv$project.extensions))
  {
    ff <- gtkFileFilterNew()
    ff$setName("Projects")
    lapply(paste("*.", crv$project.extensions, sep=""), ff$addPattern)
    dialog$addFilter(ff)
  }
  
  ff <- gtkFileFilterNew()
  ff$setName("RData Files")
  ff$addPattern("*.Rdata")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    load.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (!file.exists(load.name))
    if (! questionDialog("The project file", load.name,
                         "does not exist?"))
      return()
  
  # Load the file

  set.cursor("watch")

  crv$NOTEBOOK$setCurrentPage(0)
  load(load.name)

  # Record the cwd for projects.
  
  crs$pwd <- dirname(load.name)
  
  # Now update all appropriate textviews and associated data.

  resetRattle()  # Seems appropriate to clear out the crs
  setMainTitle(basename(load.name))
  crv$DATA.DISPLAY.NOTEBOOK$setCurrentPage(crv$DATA.DISPLAY.TREEVIEW.TAB)
  
  # DATA

  # Reset the file chooser button. 090211 What should happen if the
  # file does not exist? This is most likely when the project comes
  # from another user or another system. Leaving it set as empty seems
  # to work in that we can make changes to the roles and they get
  # effected.

  if (file.exists(crs$filename))
    theWidget("data_filechooserbutton")$setUri(crs$filename)
  
  crs$dataname <- crs$dataname
  crs$dataset <- crs$dataset

  # Restore the filename options.
  
  if (file.exists(crs$filename))
      crs$filename <- crs$filename
  else
  {
    crs$filename <- basename(crs$filename)
    crs$filename <- basename(crs$filename)
  }

  if (file.exists(crs$dwd))
    crs$dwd <- crs$dwd
  else
  {
    crs$dwd <- ""
    crs$dwd <- ""
  }

  # Make sure we don't attempt to reload the file on executing the
  # Data tab, and thereby overwriting the current data, losing all of
  # the work already done on it. Set the modified time for the dataset
  # to be apparently now.

  crs$mtime <- Sys.time()

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     crs$input, crs$target, crs$risk, crs$ident, crs$ignore,
                     crs$zero,
                     crs$boxplot, crs$hisplot, crs$cumplot, crs$benplot,
                     crs$barplot, crs$dotplot, autoroles=FALSE)
  executeSelectTab()
  resetTestTab()
  
  if (not.null(crs$risk))
    theWidget("evaluate_risk_label")$setText(crs$risk)

  # 080707 Ensure data sources are disabled so that we can not,
  # logically, load a dataset after we have loaded a project. This
  # also helps to ensure when we press theExecute button, a data set
  # is not thought to be needed to be loaded.

  enableDataSourceFunctions(FALSE)
  
  # VARIABLES

  if (not.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    theWidget("weight_entry")$setText(weights.display)
    the.weight <- sprintf("Weights: %s",weights.display)
    theWidget("model_tree_rpart_weights_label")$setText(the.weight)
    crs$weights <- crs$weights
  }

  # SAMPLE

  crs$sample      <- crs$sample
  crs$sample.seed <- crs$sample.seed

  if (not.null(crs$sample))
  {
    nrows <- nrow(crs$dataset)
    srows <- length(crs$sample)
    per <- 100*srows/nrows
    theWidget("sample_checkbutton")$setActive(TRUE)
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    if (not.null(crs$sample.seed))
      theWidget("sample_seed_spinbutton")$setValue(crs$sample.seed)
    else
      theWidget("sample_seed_spinbutton")$setValue(123)
    theWidget("sample_percentage_spinbutton")$setValue(per)
  }
  
  # EXPLORE
  
  setTextviewContents("summary_textview", crs$text$summary)
  setTextviewContents("correlation_textview", crs$text$correlation)
  setTextviewContents("prcomp_textview", crs$text$prcomp)

  # TRANSFORM

  crs$transforms <- crs$transforms
  
  # CLUSTER
  
  crs$kmeans      <- crs$kmeans
  crs$kmeans.seed <- crs$kmeans.seed
  if (not.null(crs$kmeans.seed))
    theWidget("kmeans_seed_spinbutton")$setValue(crs$kmeans.seed)
  else
    theWidget("kmeans_seed_spinbutton")$setValue(123)
  setTextviewContents("kmeans_textview", crs$text$kmeans)

  crs$hclust   <- crs$hclust

  ## MODELS - Ensure libraries are loaded.

  crs$page     <- crs$page
  crs$smodel   <- crs$smodel

  crs$rpart    <- crs$rpart
  setTextviewContents("rpart_textview", crs$text$rpart)
  if (not.null(crs$rpart)) require(rpart, quietly=TRUE)
  
  crs$rf       <- crs$rf
  setTextviewContents("rf_textview", crs$text$rf)
  if (not.null(crs$rf)) require(randomForest, quietly=TRUE)

  crs$svm      <- crs$svm
  setTextviewContents("esvm_textview", crs$text$esvm)
  if (not.null(crs$svm)) require(e1071, quietly=TRUE)

  crs$ksvm     <- crs$ksvm
  setTextviewContents("ksvm_textview", crs$text$ksvm)
  if (not.null(crs$ksvm)) require(kernlab, quietly=TRUE)

  crs$glm      <- crs$glm
  setTextviewContents("glm_textview", crs$text$glm)

  crs$ada      <- crs$ada
  setTextviewContents("ada_textview", crs$text$ada)
  if (not.null(crs$ada)) require(ada, quietly=TRUE)

  #REMOVE crs$gbm      <- crs$gbm
  #REMOVE setTextviewContents("gbm_textview", crs$text$gbm)

  if (not.null(crs$rpart.opt$priors))
    theWidget("model_tree_priors_entry")$setText(crs$rpart.opt$priors)
  if (not.null(crs$rpart.opt$loss))
    theWidget("model_tree_loss_entry")$setText(crs$rpart.opt$loss)
  if (not.null(crs$rpart.opt$split))
    theWidget("rpart_minsplit_spinbutton")$setValue(crs$rpart.opt$split)
  if (not.null(crs$rpart.opt$depth))
    theWidget("rpart_maxdepth_spinbutton")$setValue(crs$rpart.opt$depth)
  if (not.null(crs$rpart.opt$cp))
    theWidget("model_tree_cp_spinbutton")$setValue(crs$rpart.opt$cp)
  if (not.null(crs$rpart.opt$bucket))
    theWidget("rpart_minbucket_spinbutton")$setValue(crs$rpart.opt$bucket)

  # Make buttons sensitive for MODEL:RPART if there is an RPART model
  
  showModelRPartExists()

  ## Make buttons sensitive for MODEL:ADA if there is an ADA model
  
  showModelAdaExists()

  if (not.null(crs$rf.opt$trees))
    theWidget("rf_ntree_spinbutton")$setValue(crs$rf.opt$trees)
  if (not.null(crs$rf.opt$vars))
    theWidget("rf_mtry_spinbutton")$setValue(crs$rf.opt$vars)
  if (not.null(crs$rf.opt$sample))
    theWidget("rf_sampsize_entry")$setText(crs$rf.opt$sample)
  if (not.null(crs$rf.opt$proximity))
    theWidget("rf_proximity_checkbutton")$setActive(crs$rf.opt$proximity)

  ## Make buttons sensitive for MODEL:RF if there is an RF model

  showModelRFExists()
  
  if (not.null(crs$svm))
    theWidget("e1071_radiobutton")$setActive(TRUE)

  if (not.null(crs$svm.opt$kernel))
    theWidget("svm_kernel_comboboxentry")$setActive(crs$svm.opt$kernel)

  # EVALUATE

  setTextviewContents("risk_textview", crs$text$risk)

  setTextviewContents("roc_textview", crs$text$roc )

  
  crs$perf     <- crs$perf
  crs$eval     <- crs$eval
  crs$testset  <- crs$testset
  crs$testname <- crs$testname
  
  # LOG
  
  setTextviewContents("log_textview", crs$text$log)
  startLog()
  appendLog("Reloaded the project data (variable crs) from file.",
           sprintf('load("%s")', load.name))
  set.cursor()
  setStatusBar("Project loaded from", load.name)

}

