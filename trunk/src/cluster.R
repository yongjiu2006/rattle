## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-12-22 13:08:14 Graham>
##
## Implement cluster functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS
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
## EXECUTION
##

executeClusterTab <- function()
{
  ## Can not cluster without a dataset.

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## Kmeans only works for numeric data, so identify variables to
  ## include.  Only work with the INPUT/TARGET/RISK
  ## variables. That is, only exclude the IGNORE and IDENT variables.

  include <- getNumericVariables()
  if (length(include) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## Dispatch.

  if (theWidget("kmeans_radiobutton")$getActive())
    executeClusterKMeans(include)
  else if (theWidget("hclust_radiobutton")$getActive())
    executeClusterHClust(include)
}

##----------------------------------------------------------------------
##
## KMEANS
##

executeClusterKMeans <- function(include)
{
  TV <- "kmeans_textview"
  sampling  <- not.null(crs$sample)

  ## Obtain interface information.
  
  nclust <- theWidget("kmeans_clusters_spinbutton")$getValue()
  seed <- theWidget("kmeans_seed_spinbutton")$getValue()
  
  addLogSeparator("KMEANS CLUSTER")

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

  theWidget("kmeans_stats_button")$setSensitive(TRUE)
  theWidget("kmeans_plot_button")$setSensitive(TRUE)
  
  setStatusBar("K Means cluster has been generated.",
               "You may need to scroll the textview to see them." )
  
}

on_kmeans_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  theWidget("kmeans_seed_spinbutton")$setValue(rseed)
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
    errorDialog("E124: Should not be here. Plaes report to",
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
  sampling  <- not.null(crs$sample)

  include <- getNumericVariables()
  if (length(include) == 0)
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
    errorDialog("E125: Should not be here. Please report to",
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

  sampling  <- not.null(crs$sample)
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

##------------------------------------------------------------------------
##
## HCLUST
##

executeClusterHClust <- function(include)
{

  TV <- "hclust_textview"
  
  ## TODO : If data is large put up a question about wanting to
  ## continue?
  
  lib.cmd <- "require(cba, quietly=TRUE)"

  sampling  <- not.null(crs$sample)

  hclust.cmd <- paste("crs$hclust <<- ",
                      sprintf('hclust(dist(crs$dataset[%s,%s]), "%s")',
                              ifelse(sampling, "crs$sample", ""),
                              include, "ave"),
                      sep="")

  ## Log the R command

  addLogSeparator("HIERARCHICAL CLUSTER")
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

  clearTextview(TV)
  appendTextview(TV, "Hiearchical Cluster\n",
                 collectOutput("crs$hclust", TRUE))

  theWidget("hclust_dendrogram_button")$setSensitive(TRUE)
  theWidget("hclust_clusters_label")$setSensitive(TRUE)
  theWidget("hclust_clusters_spinbutton")$setSensitive(TRUE)
  theWidget("hclust_stats_button")$setSensitive(TRUE)
  theWidget("hclust_plot_button")$setSensitive(TRUE)

  setStatusBar("Hierarchical cluster has been generated.")
  
}

on_hclust_dendrogram_button_clicked <- function(button)
{

  ## Make sure there is a hclust object first.

  if (is.null(crs$hclust))
  {
    errorDialog("E126: Should not be here. Please report to",
                "Graham.Williams@togaware.com")
    return()
  }

  ## The library, cba, should already be loaded. But check anyhow.

  lib.cmd <- "require(cba, quietly=TRUE)"
  addToLog("The plot functionality is provided by the cba package.", lib.cmd)
  eval(parse(text=lib.cmd))
  
  ## PLOT: Generate the plot command to not print the xaxis labels if
  ## there are too many entities, the log the R command and execute.

  if (length(crs$hclust$order) > 100)
    limit <- ", labels=FALSE, hang=0"
  else
    limit <- ""
  plot.cmd <- paste(sprintf('plot(crs$hclust, main="", sub="", xlab=""%s)\n',
                            limit),
                    genPlotTitleCmd("Cluster Dendrogram", crs$dataname),
                    sep="")
  addToLog("Generate a dendrogram plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))
  
  setStatusBar("Dendrogram plot completed.")
}

on_hclust_stats_button_clicked <- function(button)
{
  ## Make sure there is a cluster first.
  
  if (is.null(crs$hclust))
  {
    errorDialog("E127: Should not be here. Please report to",
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

  TV <- "hclust_textview"
  num.clusters <- theWidget("hclust_clusters_spinbutton")$getValue()
  sampling  <- not.null(crs$sample)
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
                             "cutree(crs$hclust, %d))\n"),
                       ifelse(sampling, "crs$sample", ""), include,
                       num.clusters)
  addToLog("Generate cluster statistics using the fpc package.", stats.cmd)
  appendTextview(TV, "General cluster statistics:\n\n",
                 collectOutput(stats.cmd, use.print=TRUE))

  setStatusBar("HClust cluster statistics have been generated.")
}

on_hclust_plot_button_clicked <- function(button)
{

  ## Make sure there is a cluster first.

  if (is.null(crs$hclust))
  {
    errorDialog("E128: Should not be here. Please report to",
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

  sampling  <- not.null(crs$sample)
  num.clusters <- theWidget("hclust_clusters_spinbutton")$getValue()
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
                            "cutree(crs$hclust, %d))\n",
                            genPlotTitleCmd("Discriminant Coordinates",
                                            crs$dataname), sep=""),
                      ifelse(sampling, "crs$sample", ""), include,
                      num.clusters)
  addToLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

## THIS IS NOT EVEN RELATED TO hclust!!!! USES PAM

## on_hclust_seriation_button_clicked <- function(button)
## {

##   ## Make sure there is a hclust object first.

##   if (is.null(crs$hclust))
##   {
##     errorDialog("SHOULD NOT BE HERE. REPORT TO",
##                 "Graham.Williams@togaware.com")
##     return()
##   }

##   ## The library, cba, should already be loaded. But check anyhow. I
##   ## think this is required for the seriation. Need to check.

##   lib.cmd <- "require(cba, quietly=TRUE)"
##   if (! packageIsAvailable("cba", "generate a seriation plot")) return()
##   addToLog("Seriation is provided by the cba package.", lib.cmd)
##   eval(parse(text=lib.cmd))
  
##   ## Some background information.

##   sampling  <- not.null(crs$sample)
##   nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
##   if (length(nums) > 0)
##   {
##     indicies <- getVariableIndicies(crs$input)
##     include <- simplifyNumberList(intersect(nums, indicies))
##   }

##   plot.cmd <- paste("d <- dist(as.matrix(crs$dataset",
##                     sprintf("[%s,%s]",
##                             ifelse(sampling, "crs$sample", ""),
##                             include),
##                     "))\n",
##                     "l <- pam(d, 10, cluster.only = TRUE)\n",
##                     "res <- cluproxplot(d, l, method = ",
##                     'c("Optimal", "Optimal"), plot = FALSE)\n',
##                     'plot(res, plotOptions = list(main = "PAM + ',
##                     'Seriation (Optimal Leaf ordering)", ',
##                     'col = terrain.colors(64)))', sep="")

##   addToLog("Generate a seriation plot.", plot.cmd)
##   newPlot()
##   eval(parse(text=plot.cmd))
  
##   setStatusBar("Seriation plot completed.")
## }

########################################################################
##
## EXPORT
##

exportClusterTab <- function()
{

  ## Background information

  cluster.type <- ""
  if (theWidget("kmeans_radiobutton")$getActive())
    cluster.type <- "kmeans"
  else if (theWidget("hclust_radiobutton")$getActive())
    cluster.type <- "hclust"
  
  ## Obtain filename to write the clusters to.
  
  dialog <- gtkFileChooserDialog("Export Cluster", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  default.name <- paste(get.stem(crs$dataname), "_", cluster.type, sep="")
  if(not.null(crs$dataname)) dialog$setCurrentName(default.name)

  ff <- gtkFileFilterNew()
  ff$setName("R Files")
  ff$addPattern("*.csv")
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

  if (get.extension(save.name) != "csv")
    save.name <- sprintf("%s.csv", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("A file of the same name as", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()

  if (theWidget("kmeans_radiobutton")$getActive())
    exportKMeans(save.name)
  else if (theWidget("hclust_radiobutton")$getActive())
    exportHClust(save.name)

  return()
  
  setStatusBar("The cluster has been exported to", save.name)

  infoDialog("The cluster has been exported to", save.name)

}

exportKMeans <- function(file)
{
  infoDialog("The export of the KMeans clusters is under development.",
             "Please check back later.")
}

exportHClust <- function(file)
{
  infoDialog("The export of the HClust clusters is under development.",
             "Please check back later.")
}


## EXPORT TO PMML

##     if (noDatasetLoaded()) return()
##     require(XML, quietly=TRUE)
##     if (theWidget("kmeans_radiobutton")$getActive())
##     {
##       if (is.null(crs$kmeans))
##       {
##         errorDialog("No KMeans cluster is available. Be sure to build",
##                      "a cluster before trying to export it! You will need",
##                      "to press the Execute button (F5) in order to build the",
##                      "KMeans cluster.")
##         return()
##       }
##       else
##       {
##         write(collectOutput("pmml.kmeans(crs$kmeans)", TRUE),
##               file=sprintf("%s-kmeans.pmml", gsub(".csv", "", crs$dataname)))
##         infoDialog("The PMML file",
##                     sprintf('"%s-kmeans.pmml"', gsub(".csv", "", crs$dataname)),
##                     "has been written.")
##       }
##     }
##     else if (theWidget("hclust_radiobutton")$getActive())
##     {
##       errorDialog("PMML export for hierarchical clustering is not yet",
##                    "implemented.")
##       return()
##     }

pmml.kmeans <- function(cl)
{
  
  ## First collect the required information

  number.of.fields <- ncol(cl$centers)
  field.names <-  colnames(cl$centers)
  number.of.clusters <- length(cl$size)
  cluster.names <- rownames(cl$centers)

  ## Root node

  pmml <- xmlNode("PMML")

  ## DataDictionary child node

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i]))
  }
  data.dictionary$children <- data.fields
  pmml$children[[1]] <- data.dictionary
  #
  # ClusteringModel root node
  #
  clustering.model <- xmlNode("ClusteringModel",
                              attrs=c(algorithmName="KMeans",
                                numberOfClusters=number.of.clusters))
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    clusters[[i]] <- xmlNode("Cluster",
                             attrs=c(name=cluster.names[i],
                               size=cl$size[i]),
                             xmlNode("Array",
                                     attrs=c(n=number.of.fields),
                                     paste(cl$centers[i,], collapse=" ")))
  }
  clustering.model$children <- clusters
  pmml$children[[2]] <- clustering.model
  #
  # All done
  #
  return(pmml)
}

