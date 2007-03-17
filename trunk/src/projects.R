## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-01-17 21:35:09 Graham>
##
## Project functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

on_new_activate <- function(action, window)     { newProject()  }
on_open_activate <- function(action, window)    { loadProject() }
on_save_as_activate <- function(action, window) { saveProject() }

on_new_button_clicked <- function(action, window)  { newProject() }
on_open_button_clicked <- function(action, window) { loadProject() }
on_save_button_clicked <- function(action, window) { saveProject() }

newProject <- function()
{
  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have requested to start a new project.",
                               "This will clear the current project (dataset",
                               "and models).",
                               "If you choose not to continue you can save",
                               "the project, and then start a new project.",
                               "\n\n",
                               "Do you wish to continue, and overwrite the",
                               "current project?"
                               )))
      return()
  }
  resetRattle()
  ## TODO Plenty of other things that should be reset as well.
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.DATA.NAME))
  switchToPage(.NOTEBOOK.DATA.NAME)
  
}
  
saveProject <- function()
{

  ## Pre-conditions
  
  if (noDatasetLoaded()) return()
  if (variablesHaveChanged("saving the project")) return()

  ## Obtain filename to save to
  
  dialog <- gtkFileChooserDialog("Save Project", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  dialog$setCurrentName(get.stem(crs$dataname))

  ff <- gtkFileFilterNew()
  ff$setName("Rattle Files")
  ff$addPattern("*.rattle")
  dialog$addFilter(ff)

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
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (get.extension(save.name) != "rattle")
    save.name <- sprintf("%s.rattle", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("The rattle project file", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()

  ## Save all of the text views to be restored on a load.
  ## Put the following into a function and call for each textview.

  crs$text$data <<- getTextviewContent("data_textview")
  crs$text$summary <<- getTextviewContent("summary_textview")
  crs$text$correlation <<- getTextviewContent("correlation_textview")
  crs$text$prcomp <<- getTextviewContent("prcomp_textview")
  crs$text$kmeans <<- getTextviewContent("kmeans_textview")
  crs$text$rpart <<-  getTextviewContent("rpart_textview")
  crs$text$rf <<-  getTextviewContent("rf_textview")
  crs$text$esvm <<-  getTextviewContent("esvm_textview")
  crs$text$ksvm <<-  getTextviewContent("ksvm_textview")
  crs$text$glm <<-  getTextviewContent("glm_textview")
  crs$text$ada <<-  getTextviewContent("ada_textview")
  #crs$text$gbm <<-  getTextviewContent("gbm_textview")
  crs$text$risk <<- getTextviewContent("risk_textview")
  crs$text$roc <<- getTextviewContent("roc_textview")
  crs$text$log <<- getTextviewContent("log_textview")

  ## Save Transform variables selections - not sure it is really
  ## needed, but follow the template!

  crs$zero <<- getSelectedVariables("zero")
  
  ## Save Distribution variable selections

  crs$boxplot <<- getSelectedVariables("boxplot")
  crs$hisplot <<- getSelectedVariables("hisplot")
  crs$cumplot <<- getSelectedVariables("cumplot")
  crs$benplot <<- getSelectedVariables("benplot")
  crs$barplot <<- getSelectedVariables("barplot")
  crs$dotplot <<- getSelectedVariables("dotplot")

  ## Save seed information

  crs$sample.seed <<- theWidget("sample_seed_spinbutton")$getValue()
  crs$kmeans.seed <<- theWidget("kmeans_seed_spinbutton")$getValue()
  
  ## Save Model options

  if (not.null(crs$rpart))
  {
    crs$rpart.opt$priors <<- theWidget("rpart_priors_entry")$getText()
    crs$rpart.opt$loss   <<- theWidget("rpart_loss_entry")$getText()
    crs$rpart.opt$split  <<- theWidget("rpart_minsplit_spinbutton")$getValue()
    crs$rpart.opt$depth  <<- theWidget("rpart_maxdepth_spinbutton")$getValue()
    crs$rpart.opt$cp     <<- theWidget("rpart_cp_spinbutton")$getValue()
    crs$rpart.opt$bucket <<- theWidget("rpart_minbucket_spinbutton")$getValue()
  }

  if (not.null(crs$rf))
  {
    crs$rf.opt$trees     <<- theWidget("rf_ntree_spinbutton")$getValue()
    crs$rf.opt$vars      <<- theWidget("rf_mtry_spinbutton")$getValue()
    crs$rf.opt$sample    <<- theWidget("rf_sampsize_entry")$getText()
    crs$rf.opt$proximity <<- theWidget("rf_proximity_checkbutton")$getActive()
  }
    
  crs$glm.opt$family   <<- theWidget("glm_family_comboboxentry")$getActive()

  set.cursor("watch")
  startLog()
  appendLog("Saved the project data (variable crs) to file.",
           sprintf('save(crs, file="%s", compress=TRUE)', save.name))
  save(crs, file=save.name, compress=TRUE)
  set.cursor()
  setDefaultPath(save.name)
  
  setStatusBar("The current project has been saved to", save.name)
}

loadProject <- function()
{
  ## Check if crs exists and if so warn about losing the current project.

  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a project.",
                               "This will clear the old project (dataset and",
                               "models) which may not have been saved.",
                               "If you choose not to continue you can save",
                               "the project, and then load the new project.",
                               "\n\n",
                               "Do you wish to continue, and overwrite the",
                               "current project?"
                               )))
        
      return()
  }

  ## Request the rattle filename to be loaded

  dialog <- gtkFileChooserDialog("Open Project", NULL, "load",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-open", GtkResponseType["accept"])

  ff <- gtkFileFilterNew()
  ff$setName("Rattle Files")
  ff$addPattern("*.rattle")
  dialog$addFilter(ff)

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
    if (is.null(questionDialog("The rattle project file", load.name,
                                "does not exist?")))
      return()
  
  ## Load the file

  set.cursor("watch")

  .NOTEBOOK$setCurrentPage(0)
  load(load.name)
  setDefaultPath(load.name)
  
  ## Now update all appropriate textviews and associated data.

  resetRattle()  # Seems appropriate to clear out the crs
  setRattleTitle(basename(load.name))

  ## DATA

  theWidget("csv_filechooserbutton")$setFilename("")
  
  setTextviewContents("data_textview", crs$text$data)
  
  crs$dataname <<- crs$dataname
  crs$dataset <<- crs$dataset

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     crs$input, crs$target, crs$risk, crs$ident, crs$ignore,
                     crs$zero,
                     crs$boxplot, crs$hisplot, crs$cumplot, crs$benplot,
                     crs$barplot, crs$dotplot)
   executeSelectTab()
  
  if (not.null(crs$risk))
    theWidget("evaluate_risk_label")$setText(crs$risk)
  
  ## VARIABLES

  if (not.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    theWidget("weight_entry")$setText(weights.display)
    the.weight <- sprintf("Weights: %s",weights.display)
    theWidget("rpart_weights_label")$setText(the.weight)
    crs$weights <<- crs$weights
  }

  ## SAMPLE

  crs$sample      <<- crs$sample
  crs$sample.seed <<- crs$sample.seed

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
  
  ## EXPLORE
  
  setTextviewContents("summary_textview", crs$text$summary)
  setTextviewContents("correlation_textview", crs$text$correlation)
  setTextviewContents("prcomp_textview", crs$text$prcomp)

  ## CLUSTER
  
  crs$kmeans      <<- crs$kmeans
  crs$kmeans.seed <<- crs$kmeans.seed
  if (not.null(crs$kmeans.seed))
    theWidget("kmeans_seed_spinbutton")$setValue(crs$kmeans.seed)
  else
    theWidget("kmeans_seed_spinbutton")$setValue(123)
  setTextviewContents("kmeans_textview", crs$text$kmeans)

  crs$hclust   <<- crs$hclust

  ## MODELS - Ensure libraries are loaded.

  crs$page     <<- crs$page
  crs$smodel   <<- crs$smodel

  crs$rpart    <<- crs$rpart
  setTextviewContents("rpart_textview", crs$text$rpart)
  if (not.null(crs$rpart)) require(rpart, quietly=TRUE)
  
  crs$rf       <<- crs$rf
  setTextviewContents("rf_textview", crs$text$rf)
  if (not.null(crs$rf)) require(randomForest, quietly=TRUE)

  crs$svm      <<- crs$svm
  setTextviewContents("esvm_textview", crs$text$esvm)
  if (not.null(crs$svm)) require(e1071, quietly=TRUE)

  crs$ksvm     <<- crs$ksvm
  setTextviewContents("ksvm_textview", crs$text$ksvm)
  if (not.null(crs$ksvm)) require(kernlab, quietly=TRUE)

  crs$glm      <<- crs$glm
  setTextviewContents("glm_textview", crs$text$glm)

  crs$ada      <<- crs$ada
  setTextviewContents("ada_textview", crs$text$ada)
  if (not.null(crs$ada)) require(ada, quietly=TRUE)

  #REMOVE crs$gbm      <<- crs$gbm
  #REMOVE setTextviewContents("gbm_textview", crs$text$gbm)

  if (not.null(crs$rpart.opt$priors))
    theWidget("rpart_priors_entry")$setText(crs$rpart.opt$priors)
  if (not.null(crs$rpart.opt$loss))
    theWidget("rpart_loss_entry")$setText(crs$rpart.opt$loss)
  if (not.null(crs$rpart.opt$split))
    theWidget("rpart_minsplit_spinbutton")$setValue(crs$rpart.opt$split)
  if (not.null(crs$rpart.opt$depth))
    theWidget("rpart_maxdepth_spinbutton")$setValue(crs$rpart.opt$depth)
  if (not.null(crs$rpart.opt$cp))
    theWidget("rpart_cp_spinbutton")$setValue(crs$rpart.opt$cp)
  if (not.null(crs$rpart.opt$bucket))
    theWidget("rpart_minbucket_spinbutton")$setValue(crs$rpart.opt$bucket)

  ## Make buttons sensitive for MODEL:RPART if there is an RPART model
  
  if (not.null(crs$rpart)) makeRPartSensitive()

  ## Make buttons sensitive for MODEL:ADA if there is an ADA model
  
  if (not.null(crs$ada)) makeSensitiveAda()

  if (not.null(crs$rf.opt$trees))
    theWidget("rf_ntree_spinbutton")$setValue(crs$rf.opt$trees)
  if (not.null(crs$rf.opt$vars))
    theWidget("rf_mtry_spinbutton")$setValue(crs$rf.opt$vars)
  if (not.null(crs$rf.opt$sample))
    theWidget("rf_sampsize_entry")$setText(crs$rf.opt$sample)
  if (not.null(crs$rf.opt$proximity))
    theWidget("rf_proximity_checkbutton")$setActive(crs$rf.opt$proximity)

  ## Make buttons sensitive for MODEL:RF if there is an RF model

  if (not.null(crs$rf)) makeRandomForestSensitive()
  
  if (not.null(crs$svm))
    theWidget("e1071_radiobutton")$setActive(TRUE)

  if (not.null(crs$glm.opt$family))
    theWidget("glm_family_comboboxentry")$setActive(crs$glm.opt$family)

  ## EVALUATE

  setTextviewContents("risk_textview", crs$text$risk)

  setTextviewContents("roc_textview", crs$text$roc )

  
  crs$perf     <<- crs$perf
  crs$eval     <<- crs$eval
  crs$testset  <<- crs$testset
  crs$testname <<- crs$testname
  
  ## LOG
  
  setTextviewContents("log_textview", crs$text$log)
  startLog()
  appendLog("Reloaded the project data (variable crs) from file.",
           sprintf('load("%s")', load.name))
  set.cursor()
  setStatusBar("Project loaded from", load.name)

}

