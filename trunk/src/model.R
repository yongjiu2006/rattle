## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-10-22 09:02:13 Graham Williams>
##
## MODEL TAB
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS
##

## When radio button is selected, display appropriate tab page

on_regression_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GLM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_dtree_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RPART.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_boost_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GBM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_rf_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RF.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_svm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.SVM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_e1071_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.ESVM.TAB)
  }
  setStatusBar()
}

on_kernlab_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.KSVM.TAB)
  }
  setStatusBar()
}

on_priors_entry_changed <- function(action, window)
{
  setStatusBar()
}

## When the rpart loss combobox is entered, retrieve the list of
## matrix objects in R and populate the choices. This is the simplest
## approach to handling the loss matrix at present and may be
## sufficient.

on_rpart_loss_comboboxentry_set_focus_child <- function(action, window)
{
  ## Generate a list of suitable (matrix) R objects

  ml <- unlist(sapply(ls(sys.frame(0)),
                      function(x)
                      {
                        cmd <- sprintf("is.matrix(%s)",x)
                        var <- try(ifelse(eval(parse(text=cmd), sys.frame(0)),
                                          x, NULL), silent=TRUE)
                        if (inherits(var, "try-error"))
                          var <- NULL
                        return(var)
                      }))
  if (! is.null(ml))
  {
    action$getModel()$clear()
    lapply(ml, action$appendText)
  }
}

########################################################################
##
## GENERAL SUPPORT
##

currentModelTab <- function()
{
  lb <- getCurrentPageLabel(MODEL)
  if (lb == SVM && rattleWidget("kernlab_radiobutton")$getActive())
    lb <- KSVM
  return(lb)
}

########################################################################
##
## EXECUTION
##

executeModelTab <- function()
{
  ## Can not build a model without a dataset.

  if (noDatasetLoaded()) return()

  ## If VARIABLES has some ignores but crs$ignore is NULL, complain.

  if (variablesHaveChanged("building a model")) return()

  ## If WeightCalculator has changed but not same as crs$weight,
  ## complain. This doesn't work any more since we add crs$dataset to
  ## the variable names in the Weights Calculator, so they are
  ## different! But, let's remove the crs$dataset and compare.

  weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)

  if (! is.null(crs$weights)
      && weights.display != rattleWidget("weight_entry")$getText())
  {
    errorDialog("You appear to have changed the formula for calculating the",
                 "weights on the Variables tab, but have not executed the tab.",
                 "The previous formula",
                 sprintf('was "%s" and is now "%s".', crs$weights,
                         rattleWidget("weight_entry")$getText()),
                 "Please be sure to execute the tab before continuing.")
    return()
  }
    
  ## Retrieve the target and make sure there is one.

  if (length(crs$target) == 0)
  {
    errorDialog("No target has been specified.",
                 "Please identify the target using the Variables tab.",
                 "Be sure to Execute the tab once the target has",
                 "been identified.")
    return()
  }

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## If the target has more than 2 levels, disable the ROCR and Risk
  ## plots, and place a message on the first textview of the Evaluate
  ## tab. We make this word wrap here and then turn that off once the
  ## tab is Executed.
  
  if (length(levels(as.factor(crs$dataset[[crs$target]]))) > 2)
  {
    deactivate.rocr.plots()
    rattleWidget("confusion_textview")$setWrapMode("word")
    clearTextview("confusion_textview")
    appendTextview("confusion_textview",
                   "Note that the target you have chosen has more than",
                   "2 classes. Some functionality on the Evaluate tab",
                   "will not be available. In particular, the ROCR",
                   "package (Lift, ROC, Precision, and Sensitivity",
                   "charts) and the Risk Chart only handle binary",
                   "classification.")
  }
  else
  {
    activate.rocr.plots()
    setTextview("confusion_textview") # Clear any confusion table
  }

  ## DISPATCH

  if (currentModelTab() == GLM)
    executeModelGLM()
  else if (currentModelTab() == RPART)
    executeModelRPart()
  else if (currentModelTab() == GBM)
    executeModelGBM()
  else if (currentModelTab() == RF)
    executeModelRF()
  else if (is.element(currentModelTab(), c(SVM, KSVM)))
    executeModelSVM()
}

##----------------------------------------------------------------------
##
## MODEL GLM
##

executeModelGLM <- function()
{

  ## Currently only handling binary classification.
  
##   num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
##   if (num.classes > 2)
##   {
##     errorDialog("Currently Rattle only handles logistic regression for",
##                  "binary classification.",
##                  sprintf("The %s dataset has %d classes.",
##                          crs$dataname, num.classes))
##     return()
##   }

  ## Obtain the family

  family <- rattleWidget("glm_family_comboboxentry")$getActiveText()
  
  ## Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  ## List, as a string, the variables to be included. 
  
  included <- getIncludedVariables()
  
  ## Some convenience booleans.

  sampling  <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including
  
  ## Assume logistic regression for binary classification for now.
  
  glm.cmd <- paste("crs$glm <<- glm(", frml, ", data=crs$dataset",
                       if (subsetting) "[",
                       if (sampling) "crs$sample",
                       if (subsetting) ",",
                       if (including) included,
                       if (subsetting) "]",
                       ", family=", family,
                       ")", sep="")

  summary.cmd <- "summary(crs$glm)"
  
  ## Build the model.

  addLogSeparator()
  addToLog("Build a logistic regression model using glm.",
          gsub("<<-", "<-", glm.cmd), sep="")

  eval(parse(text=glm.cmd))
  
  ## Summarise the model.

  addToLog("Summary of the resulting GLM model", summary.cmd)
          
  clearTextview("glm_textview")
  setTextview("glm_textview",
              "Summary of the model built using glm.\n",
              collectOutput(summary.cmd, TRUE),
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, GLM)
  
  setStatusBar("GLM model has been generated.")
}

##----------------------------------------------------------------------
##
## MODEL RPART
##

on_rpart_plot_button_clicked <- function(button)
{

  ## Make sure there is an rpart object first.

  if (is.null(crs$rpart))
  {
    errorDialog("E122: Should not be here. Please report to",
                "Graham.Williams@togaware.com")
    return()
  }
  
  ## If there is only a root node then there is nothing to plot.

  if (nrow(crs$rpart$frame) == 1)
  {
    errorDialog("The tree consists just of a root node. Thus there is",
                "nothing to plot.")
    return()
  }

  ## PLOT: Log the R command and execute.

  plot.cmd <- paste("drawTreeNodes(crs$rpart)\n",
                    genPlotTitleCmd("Decision Tree",
                                    crs$dataname, "$", crs$target),
                    sep="")
  
  ##   plotcp.cmd <- paste("\n\n## Plot the cross validation results.\n\n",
  ##                           "plotcp(crs$rpart)\n",
  ##                           genPlotTitleCmd("Cross Validated Error",
  ##                                              crs$dataname, "$", crs$target))
  addToLog(paste("Plot the resulting rpart tree using Rattle",
                  "and maptools support functions."), plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))
  
  ## newPlot()
  ## addToLog(plotcp.command)
  ## eval(parse(text=plotcp.command))

  setStatusBar("Decision tree has been plotted.")
}

executeModelRPart <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
  control <- NULL
  parms <- NULL
  
  ## Retrieve the Priors, and check there is the right number and that
  ## they add up to 1.
  
  priors <- rattleWidget("rpart_priors_entry")$getText()
  if (nchar(priors) > 0)
  {
    pr <- as.numeric(unlist(strsplit(priors, ",")))
    if (length(pr) != num.classes)
      {
        errorDialog(sprintf("The supplied priors (%s)", priors),
                     "need to correspond to the number of classes",
                     sprintf("found in the target variable '%s'.",crs$target),
                     sprintf("Please supply exactly %d priors.", num.classes))
        return()
      }
    if (sum(pr) != 1)
      {
        errorDialog(sprintf("The supplied priors (%s)", priors),
                     sprintf("add up to %0.2f whereas", sum(pr)),
                     "they need to add up 1.00")
        return()
      }
    if (is.null(parms))
      parms <- sprintf(", parms=list(prior=c(%s))", priors)
    else
      parms <- gsub(")$", sprintf(", prior=c(%s)", priors), parms)
  }

  ## Retrieve the Min Split and check if it is different from the
  ## default, and if so then use it.

  minsplit <- rattleWidget("rpart_minsplit_spinbutton")$getValue()
  if (minsplit != RPART.MINSPLIT.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(minsplit=%d)", minsplit)
    else
      control <- gsub(")$", sprintf(", minsplit=%d)", minsplit), control)
  }

  ## Retrieve the Min Bucket and check if it is different from the
  ## default, and if so then use it.

  minbucket <- rattleWidget("rpart_minbucket_spinbutton")$getValue()
  if (minbucket != RPART.MINBUCKET.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(minbucket=%d)", minbucket)
    else
      control <- gsub(")$", sprintf(", minbucket=%d)", minbucket), control)
  }

  ## Retrieve the Max Depth and check if it is different from the
  ## default, and if so then use it.

  maxdepth <- rattleWidget("rpart_maxdepth_spinbutton")$getValue()
  if (maxdepth != RPART.MAXDEPTH.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(maxdepth=%d)", maxdepth)
    else
      control <- gsub(")$", sprintf(", maxdepth=%d)", maxdepth), control)
  }

  ## Retrieve the Complexity and check if it is different from the
  ## default, and if so then use it.

  cp <- rattleWidget("rpart_cp_spinbutton")$getValue()

  if (abs(cp-RPART.CP.DEFAULT) > 0.00001) ## Diff when same is 2.2352e-10!!!
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(cp=%f)", cp)
    else
      control <- gsub(")$", sprintf(", cp=%f)", cp), control)
  }

  ## Retrieve the Cross Validation value and if different from
  ## default, use it. No longer. Common wisdom is that 10 is right, so
  ## in Rattle just go with that.
  
  ## xval <- rattleWidget("rpart_xval_spinbutton")$getValue()
  ## if (xval != RPART.XVAL.DEFAULT)
  ## {
  ##  if (is.null(control))
  ##    control <- sprintf(", control=rpart.control(xval=%d)", xval)
  ##  else
  ##    control <- gsub(")$", sprintf(", xval=%d)", xval), control)
  ## }

  ## Retrieve the loss matrix and ensure it matches the shape of the
  ## data.

  loss <- rattleWidget("rpart_loss_entry")$getText()
  if (nchar(loss) > 0)
  {
    lo <- as.numeric(unlist(strsplit(loss, ",")))
    if (length(lo) != num.classes * num.classes)
    {
      errorDialog(sprintf("The supplied loss matrix values (%s)", loss),
                   sprintf("need to have %d values.", num.classes*num.classes),
                   "Please enter that many values, comma separated.")
      return()
    }
      
    ## TODO: Perform other checks on the matrix here.  The loss matrix
    ## must have zeros on the diagonal and positive numbers
    ## elsewhere. It must be the same dimensions as the number of
    ## classes.

    lo <- sprintf("matrix(c(%s), byrow=TRUE, nrow=%d)", loss, num.classes) 
    
    if (is.null(parms))
      parms <- sprintf(", parms=list(loss=%s)", lo)
    else
      parms <- gsub(")$", sprintf(", loss=%s)", lo), parms)
  }

  ## Build the formula for the model, noting that rpart has only a
  ## formula interface.

  frml <- paste(crs$target, "~ .")

  ## List, as a string of indicies, the variables to be included. 
  
  included <- getIncludedVariables()
  
  ## Some convenience booleans

  sampling  <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including

  ## Commands.
  
  lib.cmd <- "require(rpart, quietly=TRUE)"
  if (! packageIsAvailable("rpart", "build decision trees")) return()
    
  rpart.cmd <- paste("crs$rpart <<- rpart(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     ifelse(is.null(crs$weights), "",
                            sprintf(", weights=(%s)%s",
                                    crs$weights,
                                    ifelse(sampling, "[crs$sample]", ""))),
                     ', method="class"',
                     ifelse(is.null(parms), "", parms),
                     ifelse(is.null(control), "", control),
                     ")", sep="")

  print.cmd <- paste("print(crs$rpart)", "printcp(crs$rpart)", sep="\n")
  listrules.cmd <- "list.rules.rpart(crs$rpart)"
                             
  ## Load the required library.

  addLogSeparator()
  addToLog("Build a decision tree using the rpart package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Build the model.

  addToLog("Build an rpart model.", gsub("<<-", "<-", rpart.cmd))
  result <- try(eval(parse(text=rpart.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("syntax error.*weights", result)))
      errorDialog("The call to rpart has a syntax error. This may be due",
                   "to a syntax error in the weights formula if you have",
                   "specified one. The error message was:", result)
    else
      errorDialog("An error occured in the call to rpart.",
                   "the error was:", result)
    return()
  }

  ## Display the resulting model.

  addToLog("Generate textual output of the rpart model.", print.cmd)
  addToLog("List the rules from the tree using a Rattle support function.",
          listrules.cmd)
          
  clearTextview("rpart_textview")
  setTextview("rpart_textview",
              "Summary of the rpart model:\n\n",
              collectOutput(print.cmd),
              textviewSeparator(),
              "Tree as rules:\n\n",
              collectOutput(listrules.cmd, TRUE),
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, RPART)

  ## Now that we have a model, make sure the plot button is sensitive.
  
  rattleWidget("rpart_plot_button")$setSensitive(TRUE)

  setStatusBar("An RPart model has been generated.")
}

##----------------------------------------------------------------------
#
# Print out RPart Rules
#
list.rules.rpart <- function(model, compact=FALSE)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  # if (model$method != "class")) stop("Model method needs to be class")
  #
  # Get some information.
  #
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <-  model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  ordered <- rev(sort(frm$yval2[,5], index=TRUE)$ix)
  for (i in ordered)
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired and works on one example....
      yval <- ylevels[frm[i,]$yval]
      cover <- frm[i,]$n
      pcover <- round(100*cover/ds.size)
      prob <- frm[i,]$yval2[,5]
      cat("\n")
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      pth <- unlist(pth)[-1]
      if (length(pth) == 0) pth <- "True"
      if (compact)
      {
        cat(sprintf("R%03s ", names[i]))
        cat(sprintf("[%2.0f%%,%0.2f]",
                    pcover, prob))
        cat(sprintf(" %s", pth), sep="")
      }
      else
      {
        cat(sprintf(" Rule number: %s ", names[i]))
        cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                    yval, cover, pcover, prob))
        cat(sprintf("   %s\n", pth), sep="")
      }
    }
  }
  cat("\n")
  invisible(ordered)
}

list.rule.nodes.rpart <- function(model)
{
  # The information we need is in the rpart frame
  frm <- model$frame
  # Obtain the probabilities
  nodes <- frm$yval2[,5]
  # Get the probability ordered index of leaf nodes
  ordered <- sort(frm$yval2[,5][frm[,1] == "<leaf>"],
                  decr=TRUE, index=TRUE)
  # Return the list of node numbers
  return(row.names(frm)[which(frm[,1] == "<leaf>")][ordered$ix])
}

#
# Modify draw.tree from maptree to draw rule nodes rather than sequential
# numbers to label the leaves.
#
drawTreeNodes <- function (tree, cex = par("cex"), pch = par("pch"),
                           size = 2.5 * cex, col = NULL, nodeinfo = FALSE,
                           units = "", cases = "cases", 
                           digits = getOption("digits"),
                           decimals = 2,
                           print.levels = TRUE, new = TRUE) 
{
  #if (!packageIsAvailable("maptree", "draw a decision tree"))
  #  return
  #require(maptree)
  if (new) 
    plot.new()
  rtree <- length(attr(tree, "ylevels")) == 0
  tframe <- tree$frame
  rptree <- length(tframe$complexity) > 0
  node <- as.numeric(row.names(tframe))
  leafnode <- node[tframe$var == "<leaf>"]
  proportions <- sprintf("%0.2f", tframe$yval2[,5])
  depth <- floor(log(node, base = 2) + 1e-07)
  depth <- as.vector(depth - min(depth))
  maxdepth <- max(depth)
  x <- -depth
  y <- x
  leaves <- tframe$var == "<leaf>"
  x[leaves] <- seq(sum(leaves))
  depth <- split(seq(node)[!leaves], depth[!leaves])
  parent <- match(node%/%2, node)
  left.child <- match(node * 2, node)
  right.child <- match(node * 2 + 1, node)
  for (i in rev(depth)) x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
  nleaves <- sum(leaves)
  nnodes <- length(node)
  if (rtree) {
    dev <- tframe$dev
    pcor <- rep(0, nnodes)
    for (i in 1:nnodes) if (!leaves[i]) {
      l <- dev[node == (node[i] * 2)]
      r <- dev[node == (node[i] * 2 + 1)]
      pcor[i] <- dev[i] - l - r
    }
    pcor <- round(pcor/dev[1], 3) * 100
  }
  else {
    crate <- rep(0, nnodes)
    trate <- 0
    if (!rptree) {
      for (i in 1:nnodes) {
        yval <- tframe$yval[i]
        string <- paste("tframe$yprob[,\"", as.character(yval), 
                        "\"]", sep = "")
        crate[i] <- eval(parse(text = string))[i]
        if (leaves[i]) 
          trate <- trate + tframe$n[i] * crate[i]
      }
    }
    else {
      for (i in 1:nnodes) {
        yval <- tframe$yval[i]
        nlv <- floor(ncol(tframe$yval2)/2)
        index <- rev(order(tframe$yval2[i, 2:(nlv + 1)]))[1]
        crate[i] <- tframe$yval2[i, (nlv + 1 + index)]
        if (leaves[i]) 
          trate <- trate + tframe$n[i] * crate[i]
      }
    }
    crate <- round(crate, 3) * 100
    trate <- round(trate/tframe$n[1], 3) * 100
  }
  if (is.null(col)) 
    kol <- rainbow(nleaves)
  else if (col == "gray" | col == "grey") 
    kol <- gray(seq(0.8, 0.2, length = nleaves))
  else kol <- col
  xmax <- max(x)
  xmin <- min(x)
  ymax <- max(y)
  ymin <- min(y)
  pinx <- par("pin")[1]
  piny <- par("pin")[2]
  xscale <- (xmax - xmin)/pinx
  box <- size * par("cin")[1]
  if (box == 0) 
    xbh <- xscale * 0.2
  else xbh <- xscale * box/2
  chr <- cex * par("cin")[2]
  tail <- box + chr
  yscale <- (ymax - ymin)/(piny - tail)
  ytail <- yscale * tail
  if (box == 0) 
    ybx <- yscale * 0.2
  else ybx <- yscale * box
  ychr <- yscale * chr
  ymin <- ymin - ytail
  xf <- 0.1 * (xmax - xmin)
  yf <- 0.1 * (ymax - ymin)
  x1 <- xmin - xf
  x2 <- xmax + xf
  y1 <- ymin - yf
  y2 <- ymax + yf
  par(usr = c(x1, x2, y1, y2))
    v <- as.character(tframe$var[1])
    if (rptree) {
        sp <- tree$splits[1, ]
        val <- sp["index"]
        if (sp["ncat"] > 1) {
            r <- sp["index"]
            string <- "attributes(tree)$xlevels$"
            string <- paste(string, v, sep = "")
            xl <- eval(parse(text = string))
            lf <- rf <- ""
            for (k in 1:sp["ncat"]) if (tree$csplit[r, k] == 
                1) 
                lf <- paste(lf, xl[k], sep = ",")
            else rf <- paste(rf, xl[k], sep = ",")
            if (!print.levels) 
                string <- v
            else if (nchar(lf) + nchar(rf) > 30) # Avoid too long
              string <- v
            else
              string <- paste(lf, "=", v, "=", rf)
            
        }
        else {
            if (sp["ncat"] < 0) 
                op <- "< - >"
            else op <- "> - <"
            string <- paste(v, op, round(val, decimals))
        }
    }
    else {
        val <- substring(as.character(tframe$splits[1, 1]), 2)
        string <- paste(as.character(v), "< - >", round(val, decimals))
    }
    text.default(x[1], y[1], string, cex = cex)
    if (nodeinfo) {
        n <- tframe$n[1]
        if (rtree) {
            z <- round(tframe$yval[1], digits)
            r <- pcor[1]
            string <- paste(z, " ", units, "; ", n, " ", cases, 
                "; ", r, "%", sep = "")
        }
        else {
            z <- attr(tree, "ylevels")[tframe$yval[1]]
            r <- crate[1]
            string <- paste(z, "; ", n, " ", cases, "; ", r, 
                "%", sep = "")
        }
        text.default(x[1], y[1] - ychr, string, cex = cex)
    }
    for (i in 2:nnodes) {
        ytop <- ychr * (as.integer(nodeinfo) + 1)
        if (y[i] < y[i - 1]) {
            lines(c(x[i - 1], x[i]), c(y[i - 1] - ytop, y[i - 
                1] - ytop))
            lines(c(x[i], x[i]), c(y[i - 1] - ytop, y[i] + ychr))
        }
        else {
            lines(c(x[parent[i]], x[i]), c(y[parent[i]] - ytop, 
                y[parent[i]] - ytop))
            lines(c(x[i], x[i]), c(y[parent[i]] - ytop, y[i] + 
                ychr))
        }
        if (!leaves[i]) {
            v <- as.character(tframe$var[i])
            if (rptree) {
                k <- 1
                for (j in 1:(i - 1)) {
                  m <- tframe$ncompete[j]
                  if (m > 0) 
                    k <- k + m + 1
                  m <- tframe$nsurrogate[j]
                  if (m > 0) 
                    k <- k + m
                }
                sp <- tree$splits[k, ]
                val <- sp["index"]
                if (sp["ncat"] > 1) {
                  r <- sp["index"]
                  string <- "attributes(tree)$xlevels$"
                  string <- paste(string, v, sep = "")
                  xl <- eval(parse(text = string))
                  lf <- rf <- ""
                  for (k in 1:sp["ncat"]) if (tree$csplit[r, 
                    k] == 1) 
                    lf <- paste(lf, xl[k], sep = ",")
                  else rf <- paste(rf, xl[k], sep = ",")
                  if (!print.levels) 
                    string <- v
                  else if (nchar(lf) + nchar(rf) > 10) # Avoid too long
                    string <- v
                  else string <- paste(lf, "=", v, "=", rf)
                }
                else {
                  if (sp["ncat"] < 0) 
                    op <- "< - >"
                  else op <- "> - <"
                  string <- paste(v, op, round(val, decimals))
                }
            }
            else {
                val <- substring(as.character(tframe$splits[i, 
                  1]), 2)
                string <- paste(as.character(v), "< - >", round(val, decimals))
            }
            text.default(x[i], y[i], string, cex = cex)
            if (nodeinfo) {
                n <- tframe$n[i]
                if (rtree) {
                  z <- round(tframe$yval[i], digits)
                  r <- pcor[i]
                  string <- paste(z, " ", units, "; ", n, " ", 
                    cases, "; ", r, "%", sep = "")
                }
                else {
                  z <- attr(tree, "ylevels")[tframe$yval[i]]
                  r <- crate[i]
                  string <- paste(z, "; ", n, " ", cases, "; ", 
                    r, "%", sep = "")
                }
                text.default(x[i], y[i] - ychr, string, cex = cex)
            }
        }
        else {
            if (box == 0) {
                lines(c(x[i], x[i]), c(y[i], y[i] + ychr))
                lines(c(x[i] - xbh, x[i] + xbh), c(y[i], y[i]))
            }
            else {
                # points(x[i], y[i], pch = pch, cex = size, col = kol[x[i]])
            }
            if (rtree) {
                z <- round(tframe$yval[i], digits)
                text.default(x[i], y[i] - ybx, paste(z, units, 
                  sep = " "), cex = cex)
            }
            else {
                z <- attr(tree, "ylevels")[tframe$yval[i]]
                text.default(x[i], y[i] - ybx, z, cex = cex)
            }
            n <- tframe$n[i]
            text.default(x[i], y[i] - ybx - ychr, paste(n, cases,
                sep = " "), cex = cex)
            text.default(x[i], y[i] - 1.6*ybx - ychr,
                         paste(crate[i], "%", sep = ""), cex = cex)
                         #paste(crate[i], "%/", proportions[i], sep = ""), cex = cex)
            if (box != 0)
            {
                # ORIG text.default(x[i], y[i], as.character(x[i]), 
                text.default(x[i], y[i], as.character(leafnode[x[i]]),
                  cex = cex, col=kol[x[i]], font=2)
            }    
        }
    }
    if (nodeinfo) {
        if (rtree) 
            string <- paste("Total deviance explained =", sum(pcor), 
                "%")
        else string <- paste("Total classified correct =", trate, 
            "%")
        if (box == 0) 
            text.default(mean(x), ymin - 3.5 * ychr, string, cex = 1.2 * 
                cex)
        else text.default(mean(x), ymin - 2.2 * ybx, string, 
            cex = 1.2 * cex)
    }
}

##------------------------------------------------------------------------
##
## GBM - BOOSTING
##

executeModelGBM <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))

  ## Check for ignored variables

  indicies <- NULL
  if (! is.null(crs$input))
    indicies <- getVariableIndicies(crs$input)

  ## Build the formula for the model - why can't GBM use "."?

  target.index <- which(colnames(crs$dataset)==crs$target)
##   frml <- paste(target, "~",
##                 paste(colnames(crs$dataset)[-c(indicies, target.index)],
##                       collapse="+"))

  ## Some convenience booleans

  sampling <- ! is.null(crs$sample)
  subsetting <- sampling

  ## Greg Ridgway's advice is to use bernoulli, not adaboost or gaussian

  if (is.factor(crs$dataset[[crs$target]]) || num.classes > 2)
    distribution <- "gaussian"
  else
    #distribution <- "adaboost"
    distribution <- "bernoulli"
  
  ## Required library

  if (! packageIsAvailable("gbm", "build an AdaBoost model"))
    return()
  
  lib.cmd <- paste(sprintf("\n\n## Build a GBM (%s) model.",
                                   distribution),
                           "\n\nrequire(gbm, quietly=TRUE)")

  ## Boost command

  ## Use gbm.fit rather than gbm for more efficiency.

  included <- simplifyNumberList(indicies)
  
  boost.cmd <- paste("crs$gbm <<- gbm.fit(crs$dataset[",
                         if (sampling) "crs$sample",
                         ",", included, "], ",
                         "crs$dataset$", crs$target,
                         if (sampling) "[crs$sample]",
                         ", ",
                         'distribution="', distribution, '"',
                         ")", sep="")

  ## Summary command

  summary.cmd <- "summary(crs$gbm, cBars=5)"
  show.cmd <- "gbmShowRules(crs$gbm)"
 
  ## Log

  addToLog(lib.cmd, "\n",
          gsub("<<-", "<-", boost.cmd), "\n",
          summary.cmd, "\n",
          show.cmd, sep="")

  ## Run model and show results.
  eval(parse(text=lib.cmd))
  clearTextview("gbm_textview")
  setTextview("gbm_textview",
               "Output from GBM model builder:\n\n",
               collectOutput(boost.cmd),
               "\n\nSummary of relative influence of each variable:\n\n",
               collectOutput(paste("print(",summary.cmd, ")")),
               "\n\nRules making up the model:\n\n",
               collectOutput(show.cmd),
               sep="")

  if (sampling) crs$smodel <<- union(crs$smodel, GBM)
  
  setStatusBar("Boosted model has been generated.")
}

gbmShowRules <- function(object, rules=1:object$n.trees)
{
  stopifnot(require(gbm, quietly=TRUE))
  cat(sprintf("Number of models: %d\n", object$n.trees))
  for (i in rules)
  {
    cat(sprintf("\nTree %d: \n", i))

    tmp.frame <- data.frame(object$trees[[i]])
    split.var <- tmp.frame[1,1]
    split.var.name <- object$var.names[split.var+1]
    left.predict <- if (tmp.frame[2,8] < 0) 0 else 1
    right.predict <- if (tmp.frame[3,8] < 0) 0 else 1
    miss.predict <- if  (tmp.frame[4,8] < 0) 0 else 1

    ## TODO Should get if it is a factor from object - perhaps saver to do so.
    ## object$var.names is the variable names. object$var.type != 0 => factor
    if (is.factor(crs$dataset[[split.var.name]]))
    {
      val.index <- tmp.frame[1,2]
      categories <- levels(crs$dataset[[split.var.name]])
      lf <- paste(categories[object$c.split[[val.index+1]]==-1], collapse=",")
      rh <- paste(categories[object$c.split[[val.index+1]]==1], collapse=",")

      ## TODO Replace the predict values with object$data$y if Factor.
      cat(sprintf("  %s == %s : %.0f \n",
                  split.var.name, lf, left.predict))
      cat(sprintf("  %s == %s : %.0f \n",
                  split.var.name, rh, right.predict))
      cat(sprintf("  %s missing : %.0f \n",
                  split.var.name, miss.predict))
    }
    else
    {
      split.val <- tmp.frame[1,2]

      ## TODO Replace the predict values with object$data$y if Factor.
      cat(sprintf("  %s < %.2f : %.0f \n",
                  split.var.name, split.val, left.predict))
      cat(sprintf("  %s >= %.2f : %.0f \n",
                  split.var.name,split.val,right.predict))
      cat(sprintf("  %s missing : %.0f \n",
                  split.var.name, miss.predict))
    }
  }
}

##------------------------------------------------------------------------
##
## MODEL SVM - SUPPORT VECTOR MACHINE
##

executeModelSVM <- function()
{
  ## DESCRIPTION
  ## Build a support vector machine predictor.
  ##
  ## RETURNS
  ## Ignored.
  ##
  ## DETAILS There are two model builders for SVMs: The e1071 version
  ## is older and is supported by tune, and the kernlab version is
  ## much more extensive. I did move back to e1071 because I thought
  ## issues around the handling of NAs in kernlab a problem, but
  ## essentially I think it is an issue with svm using all variables,
  ## so I had to clean up my handling of NAs.
  
  useKernlab <- rattleWidget("kernlab_radiobutton")$getActive()

  TV <- ifelse(useKernlab, "ksvm_textview", "esvm_textview")
  
  addLogSeparator()

  ## Library.

  if (useKernlab)
  {
    if (packageIsAvailable("kernlab", "build an SVM model using ksvm"))
    {
      libCmd <- "require(kernlab, quietly=TRUE)"
      addToLog("The kernlab package supplies the ksvm function.", libCmd)
    }
    else
      return()
  }
  else
  {
    if (packageIsAvailable("e1071", "build an SVM model using svm"))
    {
      libCmd <- "require(e1071, quietly=TRUE)"
      addToLog("The e1071 package supplies the svm function.", libCmd)
    }
    else
      return()
   }
  eval(parse(text=libCmd))

  ## Formula. TODO For kernlab we assume we will always do
  ## classification rather than regression, at least for now.

  if (useKernlab)
    frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                         crs$target,
                         sprintf("as.factor(%s)", crs$target)),
                  "~ .")
  else
    frml <- paste(crs$target, "~ .")

  ## Included variables.

  included <- getIncludedVariables()
  
  ## Convenience booleans.

  sampling   <- ! is.null(crs$sample)
  including  <- ! is.null(included)
  subsetting <- sampling || including

  ## Parameters.

  parms <- ""
  
  ## Build the model.

  if (useKernlab)
    svmCmd <- paste("crs$ksvm <<- ksvm(", frml, ", data=crs$dataset", sep="")
  else
    svmCmd <- paste("crs$svm <<- svm(", frml, ", data=crs$dataset", sep="")
  svmCmd <- paste(svmCmd,
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ",",
                   if (including) included,
                   if (subsetting) "]",
                   parms, sep="")
  if (useKernlab)
    svmCmd <- paste(svmCmd, ", prob.model=TRUE", sep="")  # Probabilities
  else
    svmCmd <- paste(svmCmd, ", probability=TRUE", sep="")  # Probabilities
  svmCmd <- paste(svmCmd, ")", sep="")
  addToLog("Build a support vector machine model.", gsub("<<-", "<-", svmCmd))
  result <- try(eval(parse(text=svmCmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("The call to svm appears to have failed.",
                 "The error message was:", result,
                 "I am not familiar with this error, and you may",
                 "want to report it to the Rattle author",
                 "at Graham.Williams@togaware.com")
    return()
  }

  ## Display the resulting model.

  if (useKernlab)
    summaryCmd <- "crs$ksvm"
  else
    summaryCmd <- "crs$svm"
  addToLog("Generate textual output of the svm model.", summaryCmd)
  clearTextview(TV)
  setTextview(TV,
              "Summary of the svm model:\n\n",
              collectOutput(summaryCmd, TRUE),
              textviewSeparator())

  if (sampling)
    if (useKernlab)
      crs$smodel <<- union(crs$smodel, KSVM)
    else
      crs$smodel <<- union(crs$smodel, SVM)

  setStatusBar(sprintf("A %s model has been generated.",
                       ifelse(useKernlab, KSVM, SVM)))
}

##----------------------------------------------------------------------
##
## MARS
##
## y <- pchresp[, c(1)]
## x <- pchresp[, -c(1)]
##
## m1 <- mars(x, y)
##
## showcuts <- function(obj)
## {
##   tmp <- obj$cuts[obj$sel, ]
##   dimnames(tmp) <- list(NULL, dimnames(x)[[2]])
##   tmp
## }
##
## m2 <- mars(x, y, degree=2)

##----------------------------------------------------------------------
##
## SVM
##
## > >
## > > I am using the "svm" command in the e1071 package.
## > >
## > > Does it have an automatic way of setting the "cost" parameter?
## >
## > See ?best.svm in that package.
## >
## > > I changed a few values for the "cost" parameter but I hope there is a
## > > systematic way of obtaining the best "cost" value.
## > >
## > > I noticed that there is a "cross" (Cross validation)
## > > parameter in the "svm"
## > > function.
## > >
## > > But I did not see how it can be used to optimize the "cost" parameter.
## > >
## > > By the way, what does a 0 training error and a high testing
## > > error mean?
## > > Varying "cross=5", or "cross=10", etc. does not change the
## > > training error
## > > and testing error at all. How to improve?
## >
## > Overfitting, which varying different validation method will not solve.


## You might find http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
## <http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf>  helpful.

## Parameter tuning is essential for avoiding overfitting.


########################################################################
##
## EXPORT
##

exportModelTab <- function()
{

  if (noDatasetLoaded()) return()
  require(XML, quietly=TRUE)
  if (rattleWidget("rpart_radiobutton")$getActive())
  {
    if (is.null(crs$rpart))
    {
      errorDialog("No decision tree model is available. Be sure to build",
                  "the model before trying to export it! You will need",
                  "to press the Execute button (F5) in order to build the",
                  "model.")
      return()
    }
    else
    {
      write(collectOutput("pmml.rpart(crs$rpart)", TRUE),
            file=sprintf("%s_rpart.pmml", gsub(".csv", "", crs$dataname)))
      infoDialog("The PMML file",
                    sprintf('"%s_rpart.pmml"', gsub(".csv", "", crs$dataname)),
                    "has been written.")
    }
  }
  else
  {
    errorDialog("PMML export for this model is not yet implemented.")
    return()
  }
}

pmml.rpart <- function(rp)
{
  if (!inherits(rp, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field.names <- as.character(rp$frame$var) # GET UNIQUE LIST.....
  field.names <- setdiff(union(field.names, field.names), "<leaf>")
  number.of.fields <- length(field.names)
  tree.nodes <- rownames(rp$frame)
  rule.paths <- path.rpart(rp, node=c(tree.nodes), print.it=FALSE)
  
  ## Root node
  
  pmml <- xmlNode("PMML", attrs=c(version="3.0"))

  ## Header

  header <- xmlNode("Header",
                    attrs=c(copyright=(paste("Copyright (c) Togaware, 2006.",
                      "All Rights Reserved."))))
  header[[1]] <- xmlNode("Application",
                         attrs=c(name="Rattle",
                           version=REVISION,
                           timestamp=sprintf("%s", Sys.time()),
                           username=sprintf("%s", Sys.info()["user"])))

  pmml$children[[1]] <- header
  
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
  pmml$children[[2]] <- data.dictionary

  ## Tree Node: Generate a rule set for now - simpler that a decision
  ## tree.
  
##   tree.model <- xmlNode("TreeModel",
##                         attrs=c(modelName=crs$dataname,
##                           functionName="classification",
##                           splitCharacteristic="binary",
##                           algorithmName="rpart"))

  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=crs$dataname,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  ## Mining Schema
  
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field.names[i],
                                    usageType="active"))
  }
  target <- attr(rp$terms,"variables")[[2]]
  mining.fields[[i+1]] <- xmlNode("MiningField",
                                  attrs=c(name=target,
                                    usageType="predicted"))

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  tree.model[[1]] <- mining.schema

  ## Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set$children[[1]] <- xmlNode("RuleSelectionMethod",
                                    attrs=c(criterion="firstHit"))
  
  ## Visit each leaf node to generate a rule.

  ordered <- rev(sort(rp$frame$yval2[,5], index=TRUE)$ix)
  names <- row.names(rp$frame)
  next.child <- 2
  for (i in ordered)
  {
    if (rp$frame[i,1] == "<leaf>")
    {
      simple.rule <- xmlNode("SimpleRule",
                             attrs=c(id=sprintf("R%03d", as.integer(names[i])),
                               recordCount=rp$frame[i,]$n))
      pth <- path.rpart(rp, nodes=as.numeric(names[i]), print.it=FALSE)
      pth <- unlist(pth)[-1]
      if (length(pth) != 0)
      {
        predicate <- xmlNode("CompoundPredicate",
                             attrs=c(booleanOperator="and"))
        for (p in (1:length(pth)))
        {
          f <- unlist(strsplit(pth[p], "<|>=|="))[[1]]
          o <- ifelse(length(grep("<", pth[p]))>0, "lessThen",
               ifelse(length(grep(">=", pth[p]))>0, "greaterOrEqual",
               ifelse(length(grep("=", pth[p]))>0, "equal", "DONTKNOW")))
          v <- unlist(strsplit(pth[p], "<|>=|="))[[2]]
          predicate$children[[p]] <- xmlNode("SimplePredicate",
                                             attrs=c(field=f,
                                               operator=o,
                                               value=v))
        }
      }
      simple.rule$children[[1]] <- predicate
      rule.set$children[[next.child]] <- simple.rule
      next.child <- next.child + 1
    }
  }

  tree.model[[2]] <- rule.set
  
  ## Add to the top level structure.
  
  pmml$children[[3]] <- tree.model
  
  return(pmml)
}

