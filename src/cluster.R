## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-21 11:38:30 Graham Williams>
##
## Implement cluster functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## Interface Actions
##
## When a radio button is selected, display the appropriate tab page.

on_kmeans_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    CLUSTER$setCurrentPage(CLUSTER.KMEANS.TAB)
  setStatusBar()
}

on_hclust_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    CLUSTER$setCurrentPage(CLUSTER.HCLUST.TAB)
  setStatusBar()
}

########################################################################
##
## Execution
##

executeClusterTab <- function()
{
  ## Can not cluster without a dataset.

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (length(crs$input) != length(getSelectedVariables("input")) ||
      length(crs$ident) != length(getSelectedVariables("ident")) ||
      length(crs$ignore) != length(getSelectedVariables("ignore")))
  {
    errorDialog("You seem to have changed some selections in the",
                 "Variables tab, but have not Executed the Variables tab.",
                 "Please do so before clustering.")
    return()
  }

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## Kmeans only works for numeric data, so identify variables to
  ## include.  Only work with the INPUT/TARGET/RISK
  ## variables. That is, only exclude the IGNORE and IDENT variables.

  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }
  
  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## Dispatch.

  if (rattleWidget("kmeans_radiobutton")$getActive())
    executeClusterKMeans(include)
  else if (rattleWidget("hclust_radiobutton")$getActive())
    executeClusterHClust(include)
}

########################################################################
##
## KMEANS
##

executeClusterKMeans <- function(include)
{
  TV <- "kmeans_textview"
  sampling  <- ! is.null(crs$sample)

  ## Obtain interface information.
  
  nclust <- rattleWidget("kmeans_clusters_spinbutton")$getValue()
  seed <- rattleWidget("kmeans_seed_spinbutton")$getValue()
  
  addLogSeparator()

  ## SEED: Log the R command and execute.

  seed.cmd <- sprintf('set.seed(%d)', seed)
  addToLog("Set the seed to get the same clusters each time.", seed.cmd)
  eval(parse(text=seed.cmd))

  ## KMEANS: Log the R command and execute.

  kmeans.cmd <- sprintf('crs$kmeans <<- kmeans(crs$dataset[%s,%s], %d)',
                        ifelse(sampling, "crs$sample", ""), include, nclust)
  addToLog("Generate a kmeans cluster of size 10.",
           gsub("<<-", "<-", kmeans.cmd))
  eval(parse(text=kmeans.cmd))

  ## Show the resulting model.

  clearTextview(TV)
  appendTextview(TV, "Cluster Sizes\n\n",
                 collectOutput("paste(crs$kmeans$size, collapse=' ')", TRUE),
                 "\n\nCluster centroids.\n\n",
                 collectOutput("crs$kmeans$centers", TRUE),
                 "\n\nWithin cluster sum of squares.\n\n",
                 collectOutput("crs$kmeans$withinss", TRUE))

  ## Ensure the kmeans buttons are now active

  rattleWidget("kmeans_stats_button")$setSensitive(TRUE)
  rattleWidget("kmeans_plot_button")$setSensitive(TRUE)
  
  setStatusBar("K Means cluster has been generated.")
  
}

on_kmeans_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  rattleWidget("kmeans_seed_spinbutton")$setValue(rseed)
}

on_help_kmeans_activate <- function(action, window)
{
  if (further.help("KMeans is a traditional approach to clustering.
In addition to building a cluster, a discriminant coordinates plot
can be generated, using tha package fpc, as a display of the clusters."))
  {
    popupTextviewHelpWindow("kmeans")
    if (packageIsAvailable("fpc", "view documentation for plotcluster"))
    {
      require(fpc, quietly=TRUE)
      popupTextviewHelpWindow("plotcluster")
    }
  }
}

on_kmeans_stats_button_clicked <- function(button)
{
  ## Make sure there is a cluster first.
  
  if (is.null(crs$kmeans))
  {
    errorDialog("SHOULD NOT BE HERE. REPORT TO",
                "Graham.Williams@togaware.com")
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  addToLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  TV <- "kmeans_textview"
  sampling  <- ! is.null(crs$sample)
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## STATS: Log the R command and execute.

  stats.cmd <- sprintf(paste("cluster.stats(dist(crs$dataset[%s,%s]),",
                             "crs$kmeans$cluster)\n"),
                       ifelse(sampling, "crs$sample", ""), include)
  addToLog("Generate cluster statistics using the fpc package.", stats.cmd)
  appendTextview(TV, "General cluster statistics:\n\n",
                 collectOutput(stats.cmd, use.print=TRUE))

  setStatusBar("K Means cluster statistics have been generated.")
}

on_kmeans_plot_button_clicked <- function(button)
{

  ## Make sure there is a cluster first.

  if (is.null(crs$kmeans))
  {
    errorDialog("SHOULD NOT BE HERE. REPORT TO",
                "Graham.Williams@togaware.com")
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  addToLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  sampling  <- ! is.null(crs$sample)
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A discriminant coordinates plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  ## PLOT: Log the R command and execute.

  plot.cmd <- sprintf(paste("plotcluster(crs$dataset[%s,%s], ",
                            "crs$kmeans$cluster)\n",
                            genPlotTitleCmd("Discriminant Coordinates",
                                            crs$dataname), sep=""),
                      ifelse(sampling, "crs$sample", ""), include)
  addToLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

########################################################################
##
## HCLUST
##

executeClusterHClust <- function(include)
{

  TV <- "hclust_textview"
  
  ## TODO : If data is larg put up a question about wanting to continue?
  
  library.cmd <- "require(cba, quietly=TRUE)"

  sampling  <- ! is.null(crs$sample)

  hclust.cmd <- paste("crs$hclust <<- ",
                      sprintf('hclust(dist(crs$dataset[%s,%s]), "%s")',
                              ifelse(sampling, "crs$sample", ""),
                              include, "ave"),
                      sep="")

  ## USE PLOT, NOT AS.DENDROGRAM - IS QUICKER

  ## IF NUMBER OF ENTITIES > 100 THEN USE
  ##
  ## plot(crs$hclust, labels=FALSE, hang=0)
  ##
  ## For the book, illustrate
  ##
##   hc <- hclust(dist(crs$dataset[1:17,c(2,7,9:10,12)]), "ave")
##   plot(hc)
##   This looks like 4 clusters.
##   km <- kmeans(crs$dataset[1:17,c(2,7,9:10,12)], 4)
##   km$cluster
##   This gives the same clusters!
  
  plot.cmd <- paste("plot(as.dendrogram(crs$hclust))\n",
                    genPlotTitleCmd("Cluster Dendrogram", crs$dataname),
                    sep="")

##   seriation.cmd <- paste("d <- dist(as.matrix(crs$dataset",
##                          sprintf("[%s,%s]",
##                                  ifelse(sampling, "crs$sample", ""),
##                                  include),
##                          "))\n",
##                          "l <- pam(d, 10, cluster.only = TRUE)\n",
##                          "res <- cluproxplot(d, l, method = ",
##                          'c("Optimal", "Optimal"), plot = FALSE)\n',
##                          'plot(res, plotOptions = list(main = "PAM + ',
##                          'Seriation (Optimal Leaf ordering)", ',
##                          'col = terrain.colors(64)))', sep="")

  ## Log the R command

  addLogSeparator()
  addToLog("Generate a hierarchical cluster of the data.",
          gsub("<<-", "<-", hclust.cmd))
  
  ## Perform the commands.

  result <- try(eval(parse(text=hclust.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      errorDialog("The call to hclust appears to have failed.",
                   "This is often due, as in this case,",
                   "to running out of memory",
                   "as hclust is rather memory hungry.",
                   "A quick solution is to sample the dataset, through the",
                   "Sample tab. On 32 bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to hclust appears to have failed.",
                   "The error message was:", result,
                   "I am not familiar with this error, and you may",
                   "want to report it to the Rattle author",
                   "at Graham.Williams@togaware.com")
    return()
  }

##  addToLog("Plot the Hierarchical Dedogram.", plot.cmd)
##   if (packageIsAvailable("cba", "plot seriation charts"))
##   {
##     addToLog("We use the cba package for seriation.", library.cmd)
##     addToLog("Plot the Seriation (Experimental).", seriation.cmd)
##   }

  clearTextview(TV)
  appendTextview(TV,
                  "Hiearchical Cluster\n\n",
                  collectOutput("crs$hclust", TRUE),
                  eval(parse(text=plot.cmd)))

## VERY SLOW - PERHAPS NEED A CHECKBOX TO TURN IT ON
##
##   if (packageIsAvailable("cba"))
##   {
##     eval(parse(text=library.cmd))
##     newPlot()
##     appendTextview(TV,
##                     "\n\nNote that seriation is still experimental",
##                     eval(parse(text=seriation.cmd)))
##                                         #"\n\n",
##                #"Cluster centroids.\n\n",
##                #collectOutput("crs$kmeans$centers", TRUE),
##   }
  
  setStatusBar("Hierarchical cluster has been generated.")
  
}

