#MAJOR <- "2"
#MINOR <- "1"
#REVISION <- unlist(strsplit("$Revision: 118 $", split=" "))[2]
#VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "Copyright (c) 2007, Togaware. All Rights Reserved. GPL."

pmml.rpart <- function(rp, model.name="RPart Model", app.name="Rattle",
                       copyright=COPYRIGHT)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)

  if (! inherits(rp, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information. We list all variables,
  ## irrespective of whether they appear in the final model. This
  ## seems to be the standard thing to do. It also adds extra
  ## information - i.e., the model did not need these extra variables!

  field.names <- as.character(rp$terms@variables)[-1]
  target <- field.names[1]
  number.of.fields <- length(field.names)

  tree.nodes <- rownames(rp$frame)
  rule.paths <- path.rpart(rp, node=c(tree.nodes), print.it=FALSE)

  ## PMML
  
  pmml <- xmlNode("PMML",
                  attrs=c(version="3.1",
                    xmlns="http://www.dmg.org/PMML-3_1", 
                    "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance"))

  ## PMML -> Header

  header <- xmlNode("Header",
                    attrs=c(copyright=copyright,
                      description="RPart decision tree model"))
  header[[1]] <- xmlNode("Application",
                         attrs=c(name=app.name,
                           version=VERSION))

  header[[2]] <- xmlNode("Annotation",
                         "Export of PMML for RPart models is experimental.")
  header[[2]][[2]] <- xmlNode("Extension", sprintf("%s", Sys.time()),
                              attrs=c(description="timestamp"))
  header[[2]][[3]] <- xmlNode("Extension", sprintf("%s", Sys.info()["user"]),
                              attrs=c(description="username"))
  
  pmml$children[[1]] <- header

  ## PMML -> DataDictionary
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type

    optype <- "UNKNOWN"
    values <- NULL

    if (rp$terms@dataClasses[[field.names[i]]] == "numeric")
      optype <- "continuous"
    else if (rp$terms@dataClasses[[field.names[i]]] == "factor")
      optype <- "categorical"

    ## PMML -> DataDictionary -> DataField
    
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i],
                                  optype=optype))

    ## PMML -> DataDictionary -> DataField -> Value
    
    if (optype == "categorical")
    {
      if (field.names[i] == target)
        clevels <- rp@ylevels
      else
        clevels <- rp@xlevels[[field.names[i]]]
      for (j in 1:length(clevels))
        data.fields[[i]][[j]] <- xmlNode("Value", attrs=c(value=clevels[j]))
    }
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## PMML -> TreeModel

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="rpart",
                          splitCharacteristic="binarySplit"))
  
  
  ## PMML -> TreeModel -> MiningSchema
  
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    usage <- ifelse(field.names[i] == target, "predicted", "active") 
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field.names[i],
                                    usageType=usage))
  }

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  tree.model[[1]] <- mining.schema

  ## PMML -> TreeModel -> Node

  node <- xmlNode("Node", attrs=c(score=rp@ylevels[rp$frame[1,]$yval],
                            recordCount=rp$frame[1,]$n))
  node[[1]] <- xmlNode("True")

  tree.model[[2]] <- node
  
  ## Add to the top level structure.
  
  pmml$children[[3]] <- tree.model

  return(pmml)
}

pmml.rpart.asrules <- function(rp, model.name="RPart Model", app.name="RPart",
                               copyright=COPYRIGHT)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)
  
  if (! inherits(rp, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field.names <- as.character(rp$frame$var) # GET UNIQUE LIST.....
  field.names <- setdiff(union(field.names, field.names), "<leaf>")
  number.of.fields <- length(field.names)
  tree.nodes <- rownames(rp$frame)
  rule.paths <- path.rpart(rp, node=c(tree.nodes), print.it=FALSE)
  
  ## Root node
  
  pmml <- xmlNode("PMML", attrs=c(version="3.1"))

  ## Header

  header <- xmlNode("Header",
                    attrs=c(copyright=copyright))
  header[[1]] <- xmlNode("Application",
                         attrs=c(name=app.name,
                           version=VERSION,
                           timestamp=sprintf("%s", Sys.time()),
                           username=sprintf("%s", Sys.info()["user"])))

  header[[2]] <- xmlNode("Annotation",
                         "Export of PMML for RPart models is experimental.")
  
  pmml$children[[1]] <- header
  
  ## DataDictionary child node
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    ## Determine the operation type

    optype <- "UNKNOWN"
    values <- NULL

    if (rp$terms@dataClasses[[field.names[i]]] == "numeric")
      optype <- "continuous"
    else if (rp$terms@dataClasses[[field.names[i]]] == "factor")
    {
      optype <- "categorical"
      for (j in 1:length(rp@xlevels[[field.names[i]]]))
      {
        ## Build up the Values list of elements!
        values <- rp@xlevels[[field.names[i]]][j]
      }
    }
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i],
                                  optype=optype))
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## Tree Node: Generate a rule set for now - simpler that a decision
  ## tree.
  
##   tree.model <- xmlNode("TreeModel",
##                         attrs=c(modelName=model.name,
##                           functionName="classification",
##                           splitCharacteristic="binary",
##                           algorithmName="rpart"))

  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=model.name,
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

