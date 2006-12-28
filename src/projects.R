## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-12-28 21:29:44 Graham>
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
  if ( ! is.null(listBuiltModels()) )
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
  NOTEBOOK$setCurrentPage(getNotebookPage(NOTEBOOK, NOTEBOOK.DATA.NAME))
  switchToPage(NOTEBOOK.DATA.NAME)
  
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
  crs$text$gbm <<-  getTextviewContent("gbm_textview")
  crs$text$risk <<- getTextviewContent("risk_textview")
  crs$text$roc <<- getTextviewContent("roc_textview")
  crs$text$log <<- getTextviewContent("log_textview")

  ## Save Distribution variable selections

  crs$boxplots <<- getSelectedVariables("boxplot")
  crs$hisplots <<- getSelectedVariables("hisplot")
  crs$cumplots <<- getSelectedVariables("cumplot")
  crs$benplots <<- getSelectedVariables("benplot")
  crs$barplots <<- getSelectedVariables("barplot")
  crs$dotplots <<- getSelectedVariables("dotplot")

  ## Save seed information

  crs$sample.seed <<- rattleWidget("sample_seed_spinbutton")$getValue()
  crs$kmeans.seed <<- rattleWidget("kmeans_seed_spinbutton")$getValue()
  
  ## Save Model options

  crs$rpart.opt$priors <<- rattleWidget("rpart_priors_entry")$getText()
  crs$rpart.opt$loss   <<- rattleWidget("rpart_loss_entry")$getText()
  crs$rpart.opt$split  <<- rattleWidget("rpart_minsplit_spinbutton")$getValue()
  crs$rpart.opt$depth  <<- rattleWidget("rpart_maxdepth_spinbutton")$getValue()
  crs$rpart.opt$cp     <<- rattleWidget("rpart_cp_spinbutton")$getValue()
  crs$rpart.opt$bucket <<- rattleWidget("rpart_minbucket_spinbutton")$getValue()

  crs$rf.opt$trees     <<- rattleWidget("rf_ntree_spinbutton")$getValue()
  crs$rf.opt$vars      <<- rattleWidget("rf_mtry_spinbutton")$getValue()
  crs$rf.opt$sample    <<- rattleWidget("rf_sampsize_entry")$getText()
  crs$rf.opt$proximity <<- rattleWidget("rf_proximity_checkbutton")$getActive()

  crs$glm.opt$family   <<- rattleWidget("glm_family_comboboxentry")$getActive()
  
  save(crs, file=save.name, compress=TRUE)
  setDefaultPath(save.name)
  
  setStatusBar("The current project has been saved to", save.name)
}

loadProject <- function()
{
  ## Check if crs exists and if so warn about losing the current project.

  if ( ! is.null(listBuiltModels()) )
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

  NOTEBOOK$setCurrentPage(0)
  load(load.name)
  setDefaultPath(load.name)
  
  ## Now update all appropriate textviews and associated data.

  resetRattle()  # Seems appropriate to clear out the crs
  setRattleTitle(basename(load.name))

  ## DATA

  rattleWidget("csv_filechooserbutton")$setFilename("")
  
  setTextviewContents("data_textview", crs$text$data)
  
  crs$dataname <<- crs$dataname
  crs$dataset <<- crs$dataset

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     crs$input, crs$target, crs$risk, crs$ident, crs$ignore,
                     crs$boxplot, crs$hisplot, crs$cumplot, crs$benplot,
                     crs$barplot, crs$dotplot)
  executeVariablesTab()
  
  if (!is.null(crs$risk))
    rattleWidget("evaluate_risk_label")$setText(crs$risk)
  
  ## VARIABLES

  if (! is.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    rattleWidget("weight_entry")$setText(weights.display)
    the.weight <- sprintf("Weights: %s",weights.display)
    rattleWidget("rpart_weights_label")$setText(the.weight)
    crs$weights <<- crs$weights
  }

  ## SAMPLE

  crs$sample      <<- crs$sample
  crs$sample.seed <<- crs$sample.seed

  if (!is.null(crs$sample))
  {
    nrows <- nrow(crs$dataset)
    srows <- length(crs$sample)
    per <- 100*srows/nrows
    rattleWidget("sample_checkbutton")$setActive(TRUE)
    rattleWidget("sample_count_spinbutton")$setRange(1,nrows)
    rattleWidget("sample_count_spinbutton")$setValue(srows)
    if (! is.null(crs$sample.seed))
      rattleWidget("sample_seed_spinbutton")$setValue(crs$sample.seed)
    else
      rattleWidget("sample_seed_spinbutton")$setValue(123)
    rattleWidget("sample_percentage_spinbutton")$setValue(per)
  }
  
  ## EXPLORE
  
  setTextviewContents("summary_textview", crs$text$summary)
  setTextviewContents("correlation_textview", crs$text$correlation)
  setTextviewContents("prcomp_textview", crs$text$prcomp)

  ## CLUSTER
  
  crs$kmeans      <<- crs$kmeans
  crs$kmeans.seed <<- crs$kmeans.seed
  if (! is.null(crs$kmeans.seed))
    rattleWidget("kmeans_seed_spinbutton")$setValue(crs$kmeans.seed)
  else
    rattleWidget("kmeans_seed_spinbutton")$setValue(123)
  setTextviewContents("kmeans_textview", crs$text$kmeans)

  crs$hclust   <<- crs$hclust

  ## MODELS

  crs$page     <<- crs$page
  crs$smodel   <<- crs$smodel
  crs$rpart    <<- crs$rpart
  setTextviewContents("rpart_textview", crs$text$rpart)
  crs$rf       <<- crs$rf
  setTextviewContents("rf_textview", crs$text$rf)
  crs$svm      <<- crs$svm
  setTextviewContents("esvm_textview", crs$text$esvm)
  crs$ksvm     <<- crs$ksvm
  setTextviewContents("ksvm_textview", crs$text$ksvm)
  crs$glm      <<- crs$glm
  setTextviewContents("glm_textview", crs$text$glm)
  crs$gbm      <<- crs$gbm
  setTextviewContents("gbm_textview", crs$text$gbm)

  if (! is.null(crs$rpart.opt$priors))
    rattleWidget("rpart_priors_entry")$setText(crs$rpart.opt$priors)
  if (! is.null(crs$rpart.opt$loss))
    rattleWidget("rpart_loss_entry")$setText(crs$rpart.opt$loss)
  if (! is.null(crs$rpart.opt$split))
    rattleWidget("rpart_minsplit_spinbutton")$setValue(crs$rpart.opt$split)
  if (! is.null(crs$rpart.opt$depth))
    rattleWidget("rpart_maxdepth_spinbutton")$setValue(crs$rpart.opt$depth)
  if (! is.null(crs$rpart.opt$cp))
    rattleWidget("rpart_cp_spinbutton")$setValue(crs$rpart.opt$cp)
  if (! is.null(crs$rpart.opt$bucket))
    rattleWidget("rpart_minbucket_spinbutton")$setValue(crs$rpart.opt$bucket)

  if (! is.null(crs$rf.opt$trees))
    rattleWidget("rf_ntree_spinbutton")$setValue(crs$rf.opt$trees)
  if (! is.null(crs$rf.opt$vars))
    rattleWidget("rf_mtry_spinbutton")$setValue(crs$rf.opt$vars)
  if (! is.null(crs$rf.opt$sample))
    rattleWidget("rf_sampsize_entry")$setText(crs$rf.opt$sample)
  if (! is.null(crs$rf.opt$proximity))
    rattleWidget("rf_proximity_checkbutton")$setActive(crs$rf.opt$proximity)

  if (! is.null(crs$svm))
    rattleWidget("e1071_radiobutton")$setActive(TRUE)

  if (! is.null(crs$glm.opt$family))
    rattleWidget("glm_family_comboboxentry")$setActive(crs$glm.opt$family)

  ## EVALUATE

  setTextviewContents("risk_textview", crs$text$risk)

  setTextviewContents("roc_textview", crs$text$roc )

  
  crs$perf     <<- crs$perf
  crs$eval     <<- crs$eval
  crs$testset  <<- crs$testset
  crs$testname <<- crs$testname
  
  ## LOG
  
  setTextviewContents("log_textview", crs$text$log)
  addLogSeparator(paste("Reloaded the project from", load.name))

  set.cursor()
  
  setStatusBar("Project loaded from", load.name)

}

