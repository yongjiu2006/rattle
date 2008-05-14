## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2008-05-14 06:23:16 Graham Williams>
##
## RANDOM FOREST TAB
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS
##

on_rf_importance_button_clicked <- function(button)
{
  plotRandomForestImportance()
}

on_rf_errors_button_clicked <- function(button)
{
  plotRandomForestError()
}

on_rf_print_tree_button_clicked <- function(button)
{
  displayRandomForestTree()
}

on_help_randomForest_activate <- function(action, window)
{
  if (showHelpPlus("The randomForest algorithm builds multiple
decision trees from different samples of the dataset, and while
building each tree, random subsets of the available variables are
considered for splitting the data at each node of the tree. A simple
majority vote is then used for prediction in the case of
classificaiton (and average for regression).
RandomForest's are generally robust against overfitting.
<<>>
The default is to build 500 trees and to select the square root of the
number of variables as the subset to choose from at each node. The
resulting model is generally not very sensitive to the choice of these
parameters.
<<>>
Any entity with missing values will be ignored, which may lead to some
suprises, like many fewer entities to model when many missing values
exist. It can also lead to losing all examples of a particular class!
<<>>
An estimate of the error rate is provided as the out-of-bag (OOB)
estimate. This applies each tree to the data that was not used in
building the tree to give a quite accurate estimate of the error
rate.
<<>>
The Sample Size can be used to down-sample larger classes.
For a two-class problem with, for example, 5000 in class 0 and 250 in class 1,
a Sample Size of \"250, 250\" will usually give a more \"balanced\" classifier.
<<>>
The R package for building Random Forests is called randomForest."))
    {
      require(randomForest, quietly=TRUE)
      popupTextviewHelpWindow("randomForest")
    }
}

########################################################################
#
# MODEL RF - RANDOM FOREST
#

executeModelRF <- function()
{
  if (! packageIsAvailable("randomForest", "build a random forest model"))
      return(FALSE)
  
  ## Initial setup 
  
  TV <- "rf_textview"
  
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
  parms <- ""

  ## Make sure there are no included variables which have more than 32
  ## levels, which can not be handled by randomForest (perhaps a limit
  ## only on 32 bit machines)
  
  categoricals <- crs$input[unlist(lapply(crs$input,
                            function(x) is.factor(crs$dataset[,x])))]

  for (i in categoricals)
    if (length(levels(crs$dataset[,i])) > 32)
    {
      errorDialog("This implementation of randomForest does not handle",
                   "categorical variables with more than 32 levels.",
                   "The variable", i, "has", length(levels(crs$dataset[,i])),
                   "levels. Please choose to ignore it in the",
                   "Select tab if you wish to build a randomForest model.")
      return(FALSE)
    }

  ## Retrieve options and set up parms.

  ntree <- theWidget("rf_ntree_spinbutton")$getValue()
  if (ntree != .RF.NTREE.DEFAULT)
    parms <- sprintf("%s, ntree=%d", parms, ntree)
  
  mtry <- theWidget("rf_mtry_spinbutton")$getValue()
  if (mtry != .RF.MTRY.DEFAULT)
    parms <- sprintf("%s, mtry=%d", parms, mtry)

  sampsize <- theWidget("rf_sampsize_entry")$getText()
  if (nchar(sampsize) > 0)
  {
    ss <- as.numeric(unlist(strsplit(sampsize, ",")))
    if (length(ss) != num.classes)
      {
        errorDialog(sprintf("The supplied sample sizes (%s)", sampsize),
                     "needs to correspond to the number of classes",
                     sprintf("found in the target variable '%s'.",crs$target),
                     sprintf("Please supply exactly %d sample sizes.",
                             num.classes))
        return(FALSE)
      }
    ## TODO Check if sample sizes are larger than the classes!
    parms <- sprintf("%s, sampsize=c(%s)", parms, sampsize)
  }

  ## By default the MeanDecreaseGini is available for plotting. With
  ## importance MeanDecreaseAccuracy is also available, and it seems
  ## to also print the relative importance with regard class. So by
  ## default, generate them both.
  
  parms <- sprintf("%s, importance=TRUE", parms)

  ## Proximity is for unsupervised - not sure why I originally put it
  ## in here?
  
  ##if (theWidget("rf_proximity_checkbutton")$getActive())
  ##  parms <- sprintf("%s, proximity=TRUE", parms)
  
  ## Build the formula for the model. TODO We assume we will always do
  ## classification rather than regression, at least for now.

  frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                       crs$target,
                       sprintf("as.factor(%s)", crs$target)),
                "~ .")

  ## List, as a string of indicies, the variables to be included. 

  included <- getIncludedVariables()
  
  ## Some convenience booleans

  sampling <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling || including

  ## Ensure we have some data - i.e., not all records will be removed
  ## because they have missing values.

  dataset <- paste("crs$dataset",
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ",",
                   if (including) included,
                   if (subsetting) "]",
                   sep="")
  missing.cmd <- sprintf('length(attr((na.omit(%s)), "na.action"))', dataset)
  result <- try(missing <- eval(parse(text=missing.cmd)), silent=TRUE)
  if (inherits(result, "try-error")) missing <- 0
  dsrow.cmd <- sprintf("nrow(%s)", dataset)
  result <- try(dsrow <- eval(parse(text=dsrow.cmd)), silent=TRUE)
  if (inherits(result, "try-error")) dsrow <- 0
  if (missing == dsrow)
  {
    errorDialog("A review of the dataset has found all entities have one",
                "or more missing values amongst the vriables selected.",
                "A random forest can not be built from data with missing",
                "values, and thus there are no entities left to model.",
                "To fix this problem, you can, for example, Ignore any",
                "variable with many (or any) missing values (NAs).",
                "Or else employ imputation to fill in default or modelled",
                "values for the missing cells.")
    return()
  }
    
  ## Start the log
  
  startLog("RANDOM FOREST")

  ## Load the required library.

  library.cmd <- "require(randomForest, quietly=TRUE)"

  appendLog("The randomForest package supplies the randomForest function.",
          library.cmd)
  eval(parse(text=library.cmd))

  ## Build the model.

  rf.cmd <- paste("set.seed(123)\n",
                  "crs$rf <<- randomForest(", frml, ", data=crs$dataset",
                  if (subsetting) "[",
                  if (sampling) "crs$sample",
                  if (subsetting) ",",
                  if (including) included,
                  if (subsetting) "]",
                  parms,
                  ", na.action=na.omit",
                  ")", sep="")

  appendLog("Build a randomForest model.", gsub("<<-", "<-", rf.cmd))
  start.time <- Sys.time()
  result <- try(eval(parse(text=rf.cmd)), silent=TRUE)

  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      errorDialog("The call to randomForest appears to have failed.",
                   "This is often due, as in this case,",
                   "to running out of memory",
                   "as randomForest is rather memory hungry.",
                   "A quick solution is to sample the dataset, through the",
                   "Transform tab. On 32 bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to randomForest appears to have failed.",
                  "The error message was:", result,
                  "This is an unexpected error, and you may",
                  "want to report it to support@togaware.com")
    return(FALSE)
  }

  ## Display the resulting model.

  summary.cmd <- "crs$rf"
  appendLog("Generate textual output of randomForest model.", summary.cmd)

  importance.cmd <- "round(importance(crs$rf), 2)"
  appendLog("List the importance of the variables.", importance.cmd)

  resetTextview(TV)
  addTextview(TV, "Summary of the randomForest model:\n\n",
              collectOutput(summary.cmd, TRUE))

  addTextview(TV, "\n\nVARIABLE IMPORTANCE:\n\n",
              collectOutput(importance.cmd, TRUE))

  addTextview(TV, "\n\nDISPLAY THE MODEL\n\n",
              "To view model 5, for example, run ",
              "printRandomForests(crs$rf, 5)",
              "\nin the R console. Generating all 500 models takes ",
              "quite some time.\n")

  if (sampling) crs$smodel <<- union(crs$smodel, .RF)

  # Now that we have a model, make sure the buttons are sensitive.

  showModelRFExists()

  # Finish up.

  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("A randomForest model has been generated.", time.msg)
  return(TRUE)
}

showModelRFExists <- function(state=!is.null(crs$rf))
{
  # If an rf model exists then show the various buttons on the Model
  # tab.
  
  if (state)
  {
    theWidget("rf_importance_button")$show()
    theWidget("rf_importance_button")$setSensitive(TRUE)
    theWidget("rf_errors_button")$show()
    theWidget("rf_errors_button")$setSensitive(TRUE)
    theWidget("rf_print_tree_button")$show()
    theWidget("rf_print_tree_button")$setSensitive(TRUE)
    theWidget("rf_print_tree_spinbutton")$show()
    theWidget("rf_print_tree_spinbutton")$setSensitive(TRUE)
  }
  else
  {
    theWidget("rf_importance_button")$hide()
    theWidget("rf_errors_button")$hide()
    theWidget("rf_print_tree_button")$hide()
    theWidget("rf_print_tree_spinbutton")$hide()
  }
}

plotRandomForestImportance <- function()
{

  ## Make sure there is an rf object first.

  if (is.null(crs$rf))
  {
    errorDialog("E123: This is an unexpected error.",
                "There is no RF and attempting to plot importance.",
                "Please report this error to support@togaware.com")
    return()
  }
  
  newPlot()
  plot.cmd <- paste('varImpPlot(crs$rf, main="")\n',
                    genPlotTitleCmd("Variable Importance rf", crs$dataname),
                    sep="")
  appendLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("Random Forest Importance has been plotted.")
}
  
plotRandomForestError <- function()
{

  ## Make sure there is an rf object first.

  if (is.null(crs$rf))
  {
    errorDialog("E129: This is an unexpected error.",
                "There is no RF and attempting to plot errors.",
                "Please report to support@togaware.com")
    return()
  }
  
  newPlot()
  plot.cmd <- paste('plot(crs$rf, main="")\n',
                    genPlotTitleCmd("Error Rates rf", crs$dataname),
                    sep="")

  appendLog("Plot error rate as we increase the number of trees.", plot.cmd)
  eval(parse(text=plot.cmd))
  
  setStatusBar("Random Forest Errors has been plotted.")
}

displayRandomForestTree <- function()
{
  ## Initial setup 
  
  TV <- "rf_textview"

  ## Obtain which tree to display.
  
  tree.num <- theWidget("rf_print_tree_spinbutton")$getValue()

  ## Command to run.

  display.cmd <- sprintf("printRandomForests(crs$rf, %d)", tree.num)

  ## Perform the action.

  appendLog(sprintf("Display tree number %d.", tree.num), display.cmd)
  set.cursor("watch")
  addTextview(TV, collectOutput(display.cmd, TRUE), textviewSeparator())
  set.cursor()
  setStatusBar(paste("Tree", tree.num, "has been added to the textview.",
                     "You may need to scroll the textview to see it."))
}

printRandomForests <- function(model, models=NULL, include.class=NULL,
                               format="")
{
  # format=
  #    "VB"	Generate code that looks like VisualBasic
  
  if (! packageIsAvailable("randomForest", "print the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (is.null(models)) models <- 1:model$ntree

  for (i in models)
    printRandomForest(model, i, include.class, format)
}

## Move to using the more generic functions

treeset.randomForest <- function(model, n=1, root=1, format="R")
{
  ## Return a string listing the decision tree form of the chosen tree
  ## from the random forest.
  
  tree <- getTree(model, n)
  if (format == "R")
  {
    cassign <- "<-"
    cif <- "if"
    cthen <- ""
    celse <- "else"
    cendif <- ""
    cin <- "%in%"
  }
  else if (format == "VB")
  {
    cassign <- "="
    cif <- "If"
    cthen <- "Then"
    celse <- "Else"
    cendif <- "End If"
    cin <- "In"
  }

  ## Traverse the tree

  tr.vars <- attr(model$terms, "dataClasses")[-1]
  var.names <- names(tr.vars)
  
  result <- ""
  if (tree[root, 'status'] == -1) # Terminal node
  {
    result <- sprintf("Result %s %s", cassign,
                      levels(model$y)[tree[root,'prediction']])
  }
  else
  {
    var.class <- tr.vars[tree[root, 'split var']]
    node.var <- var.names[tree[root,'split var']]
    if(var.class == "character" | var.class == "factor")
    {
      ## Convert the binary split point to a 0/1 list for the levels.
      
      var.levels <- levels(eval(model$call$data)[[tree[root,'split var']]])
      bins <- sdecimal2binary(tree[root, 'split point'])
      bins <- c(bins, rep(0, length(var.levels)-length(bins)))
      node.value <- var.levels[bins==1]
      node.value <- sprintf('("%s")', paste(node.value, collapse='", "'))
      condition <- sprintf("%s %s %s%s", node.var, cin,
                           ifelse(format=="R", "c", ""), node.value)
    }
    else if (var.class == "integer" | var.class == "numeric")
    {
      ## Assume spliting to the left means "<=", and right ">",
      ## which is not what the man page for getTree claims!

      node.value <- tree[root, 'split point']
      condition <- sprintf("%s <= %s", node.var, node.value)

    }
    else
    {
      stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                   var.class))
    }
    

    condition <- sprintf("%s (%s)", cif, condition)
    
    lresult <- treeset.randomForest(model, n, tree[root,'left daughter'],
                                    format=format)
    if (cthen == "")
      lresult <- c(condition, lresult)
    else
      lresult <- c(condition, cthen, lresult)
    rresult <- treeset.randomForest(model, n, tree[root,'right daughter'],
                                    format=format)
    rresult <- c(celse, rresult)
    result <- c(lresult, rresult)
    if (cendif != "") result <- c(result, cendif)
  }
  return(result)
}

ruleset.randomForest <- function(model, n=1, include.class=NULL)
{

  ## NOT YET WORKING
  
  ## Same as printRandomForest, for now, but returns the string rather
  ## than printing it. Perhaps it is a list of vectors of strings,
  ## each in the list is one rule, and each string in the vector
  ## represents a single test. I HAVE SINCE FINISHED
  ## treeset.randomForest AND PERHAPS CAN USE THAT AS A TEMPLATE.
  
  # include.class	Vector of predictions to include
  
  if (! packageIsAvailable("randomForest"))
    stop("randomForest package is required to generate rule sets")

  require(randomForest, quietly=TRUE)

  if (!inherits(model, "randomForest"))
    stop("model not of class randomForest")

  tr <- getTree(model, n)
  tr.paths <- getRFPathNodesTraverse(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]

  ruleset <- list()
  nameset <- c()
  
  ## Generate a simple form for each rule.

  for (i in 1:length(tr.paths))
  {
    tr.path <- tr.paths[[i]]
    nodenum <- as.integer(names(tr.paths[i]))
    target <- levels(model$y)[tr[nodenum,'prediction']]

    if (! is.null(include.class) && target %notin% include.class) next()

    rule <- c()
    
    #cat(sprintf("%sTree %d Rule %d Node %d Decision %s\n\n",
    #            comment, n, i, nodenum, target))

    #nrules <- nrules + 1
    
    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
    for (j in 1:(length(tr.path)-1))
    {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor")
      {
        node.op <- "IN"

        ## Convert the binary to a 0/1 list for the levels.
        
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels)-length(bins)))
        if (tr.path[j] > 0)
          node.value <- var.levels[bins==1]
        else
          node.value <- var.levels[bins==0]
        node.value <- sprintf('%s', paste(node.value, collapse=', '))
      }
      else if (var.class == "integer" | var.class == "numeric")
      {
        ## Assume spliting to the left means "<=", and right ">",
        ## which is not what the man page for getTree claims!
        if (tr.path[j]>0)
          node.op <- "<="
        else
          node.op <- ">"
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      rule <- c(rule, sprintf("%s %s %s\n", var.names[j], node.op, node.value))
    }
    ruleset <- c(ruleset, rule)
    nameset <- c(nameset, nodenum)
    break()
  }
  names(ruleset) <- nameset
  return(ruleset)
}


printRandomForest <- function(model, n=1, include.class=NULL,
                              format="", comment="")
{
  # include.class	Vector of predictions to include
  
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (!inherits(model, "randomForest")) stop("model not of class randomForest")

  if (format=="VB") comment="'"
  
  tr <- getTree(model, n)
  tr.paths <- getRFPathNodesTraverse(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  
  ## Initialise the output

  cat(sprintf("%sRandom Forest Model %d\n\n", comment, n))

  ## Generate a simple form for each rule.

  cat(paste(comment,
            "-------------------------------------------------------------\n",
            sep=""))

  if (format=="VB")
    cat("IF FALSE THEN\n' This is a No Op to simplify the code\n\n")
  
  ## Number of rules generated

  nrules <- 0
  
  for (i in 1:length(tr.paths))
  {
    tr.path <- tr.paths[[i]]
    nodenum <- as.integer(names(tr.paths[i]))
    target <- levels(model$y)[tr[nodenum,'prediction']]

    if (! is.null(include.class) && target %notin% include.class) next()
    
    cat(sprintf("%sTree %d Rule %d Node %d Decision %s\n\n",
                comment, n, i, nodenum, target))

    if (format=="VB") cat("ELSE IF TRUE\n")

    nrules <- nrules + 1
    
    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
    for (j in 1:(length(tr.path)-1))
    {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor")
      {
        node.op <- "IN"

        ## Convert the binary to a 0/1 list for the levels.
        
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels)-length(bins)))
        if (tr.path[j] > 0)
          node.value <- var.levels[bins==1]
        else
          node.value <- var.levels[bins==0]
        node.value <- sprintf('("%s")', paste(node.value, collapse='", "'))
      }
      else if (var.class == "integer" | var.class == "numeric")
      {
        ## Assume spliting to the left means "<", and right ">=",
        ## which is not what the man page for getTree claims!
        if (tr.path[j]>0)
          node.op <- "<="
        else
          node.op <- ">"
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      if (format=="VB")
        cat(sprintf("AND\n%s %s %s\n", var.names[j], node.op, node.value))
      else
        cat(sprintf("%d: %s %s %s\n", j, var.names[j], node.op, node.value))
    }
    if (format=="VB") cat("THEN Count = Count + 1\n")
    cat("-----------------------------------------------------------------\n")
  }
  if (format=="VB") cat("END IF\n\n")
  cat(sprintf("%sNumber of rules in Tree %d: %d\n\n", comment, n, nrules))
}

randomForest2Rules <- function(model, models=NULL)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (is.null(models)) models <- 1:model$ntree

  ## Obtain information we need about the data
  
  vars <- attr(model$terms, "dataClasses")[-1]

  ruleset <- list()
  
  for (i in models)
  {
    ruleset[[i]] <- list(ruleset=getRFRuleSet(model, i))
  }
  return(ruleset)
}

## Generate a list of rules (conditions and outcomes) for RF MODEL
## number N.

getRFRuleSet <- function(model, n)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  tr <- getTree(model, n)
  tr.paths <- getRFPathNodes(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  
  ## Initialise the output
  
  rules <- list()

  ## Generate rpart form for each rule.

  for (i in 1:length(tr.paths))
  {
    tr.path <- tr.paths[[i]]

    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
    tr.rule <- c("root")

    for (j in 1:length(tr.path))
    {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor")
      {
        node.op <- "="

        ## Convert the binary to a 0/1 list for the levels.
        
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels)-length(bins)))
        if (tr.path[j] > 0)
          node.value <- var.levels[bins==1]
        else
          node.value <- var.levels[bins==0]
        node.value <- paste(node.value, collapse=",")
      }
      else if (var.class == "integer" | var.class == "numeric")
      {
        ## Assume spliting to the left means "<", and right ">=",
        ## which is not what the man page for getTree claims!
        if (tr.path[j]>0)
          node.op <- "<="
        else
          node.op <- ">"
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      tr.rule <- c(tr.rule, paste(var.names[j], node.op, node.value))
    }
    
    ## TODO Walk through tr.rule and remove all but the last "VAR<="
    ## and "VAR>" conditions.
    
    rules[[i]] <- list(rule=tr.rule)
  }
  return(rules)
}

getRFPathNodesTraverse <- function(tree, root=1)
{
  # Traverse the paths through the binary decision tree represented as
  # a matrix.
  #
  # The columns in the RF tree matrix are:
  #   1. left daughter;
  #   2. right daughter;
  #   3. split var;
  #   4. split point;
  #   5. status;
  #   6. prediction
  
  paths <- list()
  if (tree[root,'status'] == -1) # Terminal node
  {
    paths <- list(root)
    names(paths) <- root
  }
  else
  {
    lpaths <- getRFPathNodesTraverse(tree, tree[root,'left daughter'])
    lpaths <- lapply(lpaths, append, root, 0)
    rpaths <- getRFPathNodesTraverse(tree, tree[root,'right daughter'])
    rpaths <- lapply(rpaths, append, -root, 0)
    paths <- c(lpaths, rpaths)
  }
  return(paths)
}

getRFPathNodes <- function(tree.matrix)
{

  ## I am now using Traverse as above, as it is a more logical
  ## ordering of the rules. This function can disappear some day.
  
  # The columns in the RF tree matrix are:
  #   1. left daughter;
  #   2. right daughter;
  #   3. split var;
  #   4. split point;
  #   5. status;
  #   6. prediction
  #
  # The traversal is essentially from the shortest paths to the longer
  # paths, since we simply list the leaf nodes in the order they
  # appear in the matrix, and then list the rules in this same
  # order. For each rule we work backwards to build the path. An
  # alternative might be to order the rules in a recursive left
  # traversal manner, in which case we get a more logical ordering to
  # the rules.

  # Number of nodes in the tree
  
  nnodes <- dim(tree.matrix)[1] 

  # Leaf node indices: leaf (or terminal) nodes have a value of -1 for
  # the fifth column (status) of the tree matrix. 

  lnodes <- which(tree.matrix[,5] == -1)
  
  # Initial the paths, being a list, each element is a vector of the
  # indices of a path from the root to the leaf.
  
  paths <- list() 

  # Process each leaf's path back to the root node.
  
  for (i in 1:length(lnodes))
  {
    # Initialise the node to the leaf index
    
    node <- lnodes[[i]]
    pathI <- node
    repeat
    {
      leftV <- 1:nnodes * as.integer(tree.matrix[,1]==abs(node))
      leftNode <- leftV[leftV!=0]
      if (length(leftNode)!= 0)
      {
        node <- leftNode
      }
      else # else must not be in the next line
      {
        rightV <- 1:nnodes * as.integer(tree.matrix[,2]==abs(node))
        node <- -rightV[rightV!=0] # rhs node identified with negative index
      }

      pathI <-c(node, pathI)

      ## If the node is the root node (first row in the matrix), then
      ## the path is complete.
      
      if (abs(node)==1) break
    }
    paths[[i]] <- pathI
  }

  ## Each path is named after its leaf index number
  
  names(paths) <- as.character(lnodes)

  return(paths)
}

sdecimal2binary <- function(x)
{
  return(rev(sdecimal2binary.smallEndian(x)))
}
	
sdecimal2binary.smallEndian <- function(x)
{
  if (x==0) return(0)
  if (x<0) stop("Sorry, the input must be positive")
  dec <- x
	 
  n <- floor(log(x)/log(2))
  bin <- c(1)
  dec <- dec - 2 ^ n
	  
  while(n > 0)
  {
    if (dec >= 2 ^ (n-1)) {bin <- c(bin,1); dec <- dec - 2 ^ (n-1)}
    else bin <- c(bin,0)
    n <- n - 1
  }
  return(bin)
}
	
