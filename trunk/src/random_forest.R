##------------------------------------------------------------------------
##
## MODEL RF - RANDOM FOREST
##

executeModelRF <- function()
{
  TV <- "rf_textview"
  setStatusBar()
  
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
                   "Variables tab if you wish to build a randomForest model.")
      return()
    }

  ## Retrieve options and set up parms.

  ntree <- rattleWidget("rf_ntree_spinbutton")$getValue()
  if (ntree != RF.NTREE.DEFAULT)
    parms <- sprintf("%s, ntree=%d", parms, ntree)
  
  mtry <- rattleWidget("rf_mtry_spinbutton")$getValue()
  if (mtry != RF.MTRY.DEFAULT)
    parms <- sprintf("%s, mtry=%d", parms, mtry)

  sampsize <- rattleWidget("rf_sampsize_entry")$getText()
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
        return()
      }
    ## TODO Check if sample sizes are larger than the classes!
    parms <- sprintf("%s, sampsize=c(%s)", parms, sampsize)
  }

  ## By default the MeanDecreaseGini is available for plotting. With
  ## importance we, MeanDecreaseAccuracy is also available, and it
  ## seems to also print the relative importance with regard class.
  
  importance <- rattleWidget("rf_importance_checkbutton")$getActive()
  if (importance)
    parms <- sprintf("%s, importance=TRUE", parms)
  
  if (rattleWidget("rf_proximity_checkbutton")$getActive())
    parms <- sprintf("%s, proximity=TRUE", parms)
  
  ## Build the formula for the model. TODO We assume we will always do
  ## classification rather than regression, at least for now.

  frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                       crs$target,
                       sprintf("as.factor(%s)", crs$target)),
                "~ .")

  ## List, as a string of indicies, the variables to be included. 

  included <- getIncludedVariables()
  
  ## Some convenience booleans

  sampling <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including

  ## Start the log
  
  addLogSeparator()

  ## Load the required library.

  library.cmd <- "require(randomForest, quietly=TRUE)"

  addToLog("The randomForest package supplies the randomForest function.",
          library.cmd)
  eval(parse(text=library.cmd))

  ## Build the model.

  rf.cmd <- paste("crs$rf <<- randomForest(", frml, ", data=crs$dataset",
                  if (subsetting) "[",
                  if (sampling) "crs$sample",
                  if (subsetting) ",",
                  if (including) included,
                  if (subsetting) "]",
                  parms,
                  ", na.action=na.omit",
                  ")", sep="")

  addToLog("Build a randomForest model.", gsub("<<-", "<-", rf.cmd))
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
                   "Sample tab. On 32 bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to randomForest appears to have failed.",
                   "The error message was:", result,
                   "I am not familiar with this error, and you may",
                   "want to report it to the Rattle author",
                   "at Graham.Williams@togaware.com")
    return()
  }

  ## Display the resulting model.

  if (importance)
  {
    newPlot()
    plot.cmd <- paste('varImpPlot(crs$rf,',
                      'main="Relative Importance of Variables")')
    addToLog("Plot the relative importance of the variables.", plot.cmd)
    eval(parse(text=plot.cmd))
  }
  
  summary.cmd <- "crs$rf"
  addToLog("Generate textual output of randomForest model.", summary.cmd)

  importance.cmd <- "round(importance(crs$rf), 2)"
  addToLog("List the importance of the variables.", importance.cmd)

  clearTextview(TV)
  appendTextview(TV, "Summary of the randomForest model:\n\n",
                 collectOutput(summary.cmd, TRUE))

  appendTextview(TV, "Variable importance:\n\n",
                   collectOutput(importance.cmd, TRUE))

  appendTextview(TV, "To view model 5, for example, run",
                 "printRandomForests(crs$rf, 5)\n",
                 "in the R conosle. Generating all 500 models takes",
                 "quite some time.")

  if (sampling) crs$smodel <<- union(crs$smodel, RF)

  setStatusBar("A randomForest model has been generated.")
}

printRandomForests <- function(model, models=NULL)
{
  if (! packageIsAvailable("randomForest", "print the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (is.null(models)) models <- 1:model$ntree

  strings <- c()
  
  for (i in models)
    printRandomForest(model, i)
  
  return(strings)
}

printRandomForest <- function(model, n)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  tr <- getTree(model, n)
  tr.paths <- getRFPathNodes(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  
  ## Initialise the output

  cat("===================================================================\n")
  cat(sprintf("Random Forest Model %d\n\n", n))

  ## Generate rpart form for each rule.

  cat("-----------------------------------------------------------------\n")
  
  for (i in 1:length(tr.paths))
  {
    cat(sprintf("Rule Number %d of Forest %d\n\n", i, n))
    
    tr.path <- tr.paths[[i]]

    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
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
        ## Assume spliting to the left means "<", and right ">="
        if (tr.path[j]>0)
          node.op <- "<"
        else
          node.op <- ">="
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      cat(sprintf("%s %s %s\n", var.names[j], node.op, node.value))
    }
    cat("-----------------------------------------------------------------\n")
  }
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
        ## Assume spliting to the left means "<", and right ">="
        if (tr.path[j]>0)
          node.op <- "<"
        else
          node.op <- ">="
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      tr.rule <- c(tr.rule, paste(var.names[j], node.op, node.value))
    }
    
    ## TODO Walk through tr.rule and remove all but the last "VAR<"
    ## and "VAR>=" conditions.
    
    rules[[i]] <- list(rule=tr.rule)
  }
  return(rules)
}

getRFPathNodes <- function(treeMat)
{
  ## The columns in the RF tree matrix are:
  ##   1. left daughter;
  ##   2. right daughter;
  ##   3. split var;
  ##   4. split point;
  ##   5. status;
  ##   6. prediction

  ## Number of nodes in the tree
  
  nnodes <- dim(treeMat)[1] 

  ## Leaf node indices

  leafIndex <- 1:nnodes * as.integer(treeMat[,5]== -1) # Non-leaf index is 0
  leafIndex <- leafIndex[leafIndex!=0] # Remove non-zeros (non-leafs)
	  
  paths <- list() # A list, each element a vector of the indices of a path

  ## Process each leaf's path
  
  for (i in 1:length(leafIndex))
  {
    ## Initialise the node to the leaf index
    
    node <- leafIndex[i] # e.g. i=1, node=3
    pathI <- c()
    repeat
    {
      leftV <- 1:nnodes * as.integer(treeMat[,1]==abs(node))
      leftNode <- leftV[leftV!=0]
      if (length(leftNode)!= 0)
      {
        node <- leftNode
      }
      else # else must not be in the next line
      {
        rightV <- 1:nnodes * as.integer(treeMat[,2]==abs(node))
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
  
  names(paths) <- as.character(leafIndex)

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
	
