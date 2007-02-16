### PMML: Predictive Modelling Markup Language
##
## Part of the Rattle package for Data Mining
##
## Time-stamp: <2007-02-17 04:27:17 Graham>
##
## Copyright (c) 2007 Graham Williams, Togaware.com, GPL Version 2
##
## TODO
##
##	Extract the DataDictionary stuff to a separate function to
##	share between pmml.rpat and pmml.kmeans.

pmmlRootNode <- function()
{
  ## PMML
  
  PMML.VERSION <- "3.1"
  return(xmlNode("PMML",
                 attrs=c(version=PMML.VERSION,
                   xmlns="http://www.dmg.org/PMML-3_1", 
                   "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance")))
}

pmmlHeader <- function(description, copyright, app.name)
{
  ## Header
  
  VERSION <- "1.0.2"

  if (is.null(copyright))
    header <- xmlNode("Header", attrs=c(description=description))
  else
    header <- xmlNode("Header",
                      attrs=c(copyright=copyright, description=description))

  ## Header -> Extension
  
  header <- append.XMLNode(header,
                           xmlNode("Extension", sprintf("%s", Sys.time()),
                                       attrs=c(description="timestamp")))
  
  header <- append.XMLNode(header, xmlNode("Extension",
                                           sprintf("%s", Sys.info()["user"]),
                                           attrs=c(description="username")))

  ## Header -> Application

  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                             version=VERSION)))

  return(header)
}
  
pmmlDataDictionary <- function(field)
{
  ## field$name is a vector of strings, and includes target
  ## field$class is indexed by fields$names
  ## field$levels is indexed by fields$names
  
  number.of.fields <- length(field$name)

  ## DataDictionary
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    # Determine the operation type

    optype <- "UNKNOWN"
    values <- NULL

    if (field$class[[field$name[i]]] == "numeric")
      optype <- "continuous"
    else if (field$class[[field$name[i]]] == "factor")
      optype <- "categorical"

    ## DataDictionary -> DataField
    
     data.fields[[i]] <- xmlNode("DataField", attrs=c(name=field$name[i],
                                                optype=optype))

    ## DataDictionary -> DataField -> Value
    
    if (optype == "categorical")
      for (j in 1:length(field$levels[[field$name[i]]]))
        data.fields[[i]][[j]] <- xmlNode("Value",
                                         attrs=c(value=
                                           field$levels[[field$name[i]]][j]))
  }
  data.dictionary$children <- data.fields

  return(data.dictionary)
    
}

pmmlMiningSchema <- function(field, target=NULL)
{
  number.of.fields <- length(field$name)
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    if (is.null(target))
      usage <- "active"
    else
      usage <- ifelse(field$name[i] == target, "predicted", "active")
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field$name[i],
                                    usageType=usage))
  }
  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  return(mining.schema)
}


########################################################################

pmml.rpart <- function(model,
                       model.name="RPart_Model",
                       app.name="Rattle/PMML",
                       description="RPart decision tree model",
                       copyright=NULL)
{
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart object")

  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)

  ## Collect the required information. We list all variables,
  ## irrespective of whether they appear in the final model. This
  ## seems to be the standard thing to do with PMML. It also adds
  ## extra information - i.e., the model did not need these extra
  ## variables!

  field <- NULL
  field$name <- as.character(model$terms@variables)[-1]
  number.of.fields <- length(field$name)
  field$class <- model$terms@dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model@ylevels
      else
        field$levels[[field$name[i]]] <- model@xlevels[[field$name[i]]]
  }
  
  ## PMML

  pmml <- pmmlRootNode()

  ## PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  ## PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> TreeModel

  tree.model <- xmlNode("TreeModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          algorithmName="rpart",
                          splitCharacteristic="binarySplit"))
  
  
  ## PMML -> TreeModel -> MiningSchema
  
  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  ## PMML -> TreeModel -> Node

  depth <- rpart:::tree.depth(as.numeric(row.names(model$frame)))
  count <- model$frame$n
  score <- model@ylevels[model$frame$yval]
  label <- labels(model, pretty=0)

  field <- label[1]
  operator <- ""
  value <- "" #list("")
  for (i in 2:length(label))
  {
    field <-  c(field, strsplit(label[i], '>|<|=')[[1]][1])
    op <- substr(label[i], nchar(field[i])+1, nchar(field[i])+2)
    if (op == ">=")
    {
      operator <- c(operator, "greaterOrEqual")
      value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
    }
    else if (op == "< ")
    {
      operator <- c(operator, "lessThan")
      value <- c(value, substr(label[i], nchar(field[i])+3, nchar(label[i])))
    }
    else if (substr(op, 1, 1) == "=")
    {
      operator <- c(operator, "isIn")
      value <- c(value, substr(label[i], nchar(field[i])+2, nchar(label[i])))
    }
  }
  
  node <- genBinaryTreeNodes(depth, count, score, field, operator, value)

  ## tree.model[[2]] <- node
  tree.model <- append.XMLNode(tree.model, node)

  ## Add to the top level structure.
  
  ## pmml$children[[3]] <- tree.model
  pmml <- append.XMLNode(pmml, tree.model)

  return(pmml)
}

genBinaryTreeNodes <- function(depths, counts, scores, fields, ops, values)
{
  depth <- depths[1]
  count <- counts[1]
  score <- scores[1]
  field <- fields[1]
  op <- ops[1]
  value <- values[1]

  node <- xmlNode("Node", attrs=c(score=score, recordCount=count))
  if (field == "root")
    predicate <- xmlNode("True")
  else
  {
    # Create the SimplePredicate or SimpeSetPredicate node.

    if (op[1] %in% c("greaterOrEqual", "lessThan"))
      predicate <- xmlNode("SimplePredicate", attrs=c(field=field,
                                                operator=op, value=value))
    else if (op[1] == "isIn")
    {
      predicate <- xmlNode("SimpleSetPredicate", attrs=c(field=field,
                                                   operator=op))
      value <- strsplit(value[[1]], ",")[[1]]
      # Do we need quotes around the values?
      # vals <- paste('"', value, '"', collapse=" ", sep="")
      vals <- paste(value, collapse=" ", sep="")
      ##predicate[[1]] <- xmlNode("Array",
      ##                          vals,
      ##                          attrs=c(n=length(value), type="string"))
      predicate <- append.XMLNode(predicate,
                                  xmlNode("Array",
                                          vals,
                                          attrs=c(n=length(value),
                                            type="string")))
    }
  }
  if (length(depths) == 1)
  {
    left <- NULL
    right <- NULL
  }
  else
  {
    split.point <- which(depths[c(-1,-2)] == depths[2]) + 1 # Binary tree
    lb <- 2:split.point
    rb <- (split.point + 1):length(depths)
    left <- genBinaryTreeNodes(depths[lb], counts[lb], scores[lb],
                               fields[lb], ops[lb], values[lb])
    right <- genBinaryTreeNodes(depths[rb], counts[rb], scores[rb],
                                fields[rb], ops[rb], values[rb])
  }
  ## node[[1]] <- predicate
  node <- append.XMLNode(node, predicate)
  if (!is.null(left))
  {
    ##node[[2]] <- left
    ##node[[3]] <- right
    node <- append.XMLNode(node, left)
    node <- append.XMLNode(node, right)
  }
  return(node)
}

########################################################################

pmml.rpart.as.rules <- function(model,
                                model.name="RPart_Model",
                                app.name="RPart",
                                description="RPart model as rules",
                                copyright=NULL)
{
  require(XML, quietly=TRUE)
  require(rpart, quietly=TRUE)
  
  if (! inherits(model, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field <- NULL
  field$name <- as.character(model$terms@variables)[-1]
  number.of.fields <- length(field$name)
  field$class <- model$terms@dataClasses
  target <- field$name[1]

  for (i in 1:number.of.fields)
  {
    if (field$class[[field$name[i]]] == "factor")
      if (field$name[i] == target)
        field$levels[[field$name[i]]] <- model@ylevels
      else
        field$levels[[field$name[i]]] <- model@xlevels[[field$name[i]]]
  }

  ## PMML
  
  pmml <- pmmlRootNode()

  ## PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))
  
  ## PMML -> DataDictionary
  
  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> RuleSetModel
  
  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=model.name,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  ## MiningSchema
  
  tree.model <- append.XMLNode(tree.model, pmmlMiningSchema(field, target))

  ## Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set <- append.XMLNode(rule.set,
                             xmlNode("RuleSelectionMethod",
                                     attrs=c(criterion="firstHit")))
  
  ## Visit each leaf node to generate a rule.

  ordered <- rev(sort(model$frame$yval2[,5], index=TRUE)$ix)
  names <- row.names(model$frame)
  next.child <- 2
  for (i in ordered)
  {
    if (model$frame[i,1] == "<leaf>")
    {
      simple.rule <- xmlNode("SimpleRule",
                             attrs=c(id=sprintf("R%03d", as.integer(names[i])),
                               recordCount=model$frame[i,]$n))
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
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

  tree.model <- append.XMLNode(tree.model, rule.set)
  
  ## Add to the top level structure.
  
  pmml <- append.XMLNode(pmml, tree.model)
  
  return(pmml)
}

########################################################################

pmml.kmeans <- function(model,
                        model.name="KMeans_Model",
                        app.name="Rattle/PMML",
                        description="KMeans cluster model",
                        copyright=NULL)
{
  require(XML, quietly=TRUE)
  
  if (! inherits(model, "kmeans")) stop("Not a legitimate kmeans object")

  ## Collect the required information.

  field <- NULL
  field$name <-  colnames(model$centers)
  number.of.fields <- length(field$name)
  field$class <- rep("numeric", number.of.fields) # All fields ar numeric
  names(field$class) <- field$name
  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  ## PMML

  pmml <- pmmlRootNode()

  ## PMML -> Header

  header <- pmmlHeader(description, copyright, app.name)
  pmml <- append.XMLNode(pmml, header)

  ## PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  ## PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering",
                        algorithmName="KMeans",
                        modelClass="centerBased",
                        numberOfClusters=number.of.clusters))

  ## PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, pmmlMiningSchema(field))

  ## PMML -> ClusteringModel -> ComparisonMeasure
  
  cl.model <- append.XMLNode(cl.model, xmlNode("ComparisonMeasure",
                                               attrs=c(kind="distance")))
  
  ## PMML -> ClusteringModel -> Cluster -> Array
  
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    cl.model <- append.XMLNode(cl.model,
                               xmlNode("Cluster",
                                       attrs=c(name=cluster.names[i],
                                         size=model$size[i]),
                                       xmlNode("Array",
                                               attrs=c(n=number.of.fields),
                                               paste(model$centers[i,],
                                                     collapse=" "))))
  }
  pmml <- append.XMLNode(pmml, cl.model)

  return(pmml)
}

