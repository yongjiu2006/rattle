# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-03-03 21:17:01 Graham Williams>
#
# Help Menu
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

popupTextviewHelpWindow <- function(topic)
{
  collectOutput(sprintf("help(%s, htmlhelp=TRUE)", topic), TRUE)
}

showHelpPlus <- function(msg)
{
  if (! questionDialog(paste(gsub(" <<>> ", "\n\n",
                                  gsub("\n", " ", msg)),
                             "Would you like to view the R help?",
                             sep="\n\n")))
    return(FALSE)
  else
    return(TRUE)
}

showHelp <- function(msg)
{
  infoDialog(paste(gsub(" <<>> ", "\n\n", gsub("\n", " ", msg))))
}

on_help_general_activate <- function(action, window)
{
  showHelp("Rattle is a graphical user interface for data mining
written in GNOME and R. R is an environment for statistical computing.
They are all free software licensed under the GNU General
Public License (GPL).
<<>>
Interaction with Rattle logically proceeds by progressing through the Tabs:
first load in some Data, select Variables for exploring and mining,
possibly Sample the data, Explore the data, build your Models,
and Evaluate them. For any tab, the modus operandi is to configure
the options available and then click the Execute button (or F2) to perform
the appropriate tasks. Note that the tasks are NOT performed until
the Execute button (or F2 or the Execute menu item under Tools) is clicked.
<<>>
The Status Bar indicates when the action
is completed. Messages from R (e.g., error messages. although I do attempt
to catch them first) will appear in the R console
from where you started Rattle. The corresponding R Code will
appear in the Log tab.
This allows you to review the R commands
that perform the corresponding data mining tasks. Even better though,
you can copy the text from here and paste it into the same R Console
from which Rattle is running, and execute the commands directly.
This allows you to use Rattle to do the basics, and then where you
need more sophistication, go into R directly. Rattle uses a variable called
crs to store its current state, and you can modify this directly.
<<>>
Rattle is being extensively tested
on binary classification problems (with 0/1 or a two level variable
as the outcomes for the Target variable). It is less well tested on
mulitnomial classification and regression tasks. but is become stable
in those areas also, over time.
<<>>
The most we can guarantee about this
code is that there are bugs! When you find one, or a misfeature or
something else you would like Rattle to do, please do email
support@togaware.com.
<<>>
Enjoy.")
}

########################################################################
# DATA

on_help_nomenclature_data_activate <- function(action, window)
{
  showHelp("There are many
different nomenclatures being used in data mining, deriving from the many
different contributory fields. Here, we attempt to stay with a single,
consistent nomenclature.
<<>>
A dataset consists of entities described using variables,
which might consist of a mixture of input variables and output variables,
either of which may be categoric or numeric.
<<>>
dataset = A collection of data.
<<>>
entity = An object of interest, descibed by variables.
Also called a record, object, row or observation.
<<>>
variable = The data items used to describe an enitity.
Also called an attribute, feature or column.
<<>>
input variable = A measured or preset data item.
Also called predictor, independent variable, observed variable,
or descriptive variable.
<<>>
output variable = A variable possibly influenced by the input variables.
Also called response or dependent variable.
<<>>
categoric variable = A variable that takes on a value from a fixed
set of values. In R these are called factors and the possible values
are refered to as the levels of the factor.
<<>>
numeric variable = A variable that has values that are integers or real
numbers.")
}

on_help_data_fex_activate <- function(action, window)
{
  showHelp("The FEX option of the Data tab provides a direct linkage to Web Focus
as the source of our dataset for analysis.")
}

on_help_csv_activate <- function(action, window)
{
  if (showHelpPlus("Data can be loaded from
a comma separated value (CSV) file, as might be generated
by spreadsheets and databases,
including Excel, Gnumeric, SAS/EM, QueryMan, and many other applications.
This is a good option for importing data.
<<>>
The CSV file is assumed to begin with a header row, listing the names
of the variables. 
The remainder of the file is expected to consist of rows of data that record
information about the entities, with fields generally separated by commas
recording the values of the variables for this entity.
<<>>
Use the Separator box to choose a separator other than the default comma.
A common alternative is a tab (\\t), or simply leave it blank to have
any white space act as a separator.
<<>>
A URL can be supplied in the Location: text box so that a CSV file can be
loaded from the network.
<<>>
The corresponding R code uses the simple read.csv() function."))
    popupTextviewHelpWindow("read.csv") }

on_help_arff_activate <- function(action, window)
{
  if (showHelpPlus("Data can be loaded from
an Attribute-Relation File Format, or ARFF, file
(beginning with version 2.5.0 of R).
ARFF is an ASCII text file format
that is essentially a CSV file with a header that describes the
meta-data. ARFF was developed for use in the Weka machine learning
software and there are quite a few datasets in this format now.
<<>>
The corresponding R code uses the read.arff() function from the
foreign package."))
  {
    require(foreign, quietly=TRUE)
    popupTextviewHelpWindow("read.arff")
  }
}

on_help_rdata_file_activate <- function(action, window)
{
  showHelp("Choose this if you have data stored in an R dataset
(usually with a filename extension of .Rdata).
The named file will be loaded and any data frames found in there will
be listed for selection.")
}

on_help_data_library_activate <- function(action, window)
{
  showHelp("The Library option of the Data tab provides access to all sample datasets
provided by the various R packages in your installation. When selected, all of the
packages are scanned for the datasets they provide. You can then choose a dataset
from the drop down menu.")
}

on_help_rdataset_activate <- function(action, window)
{
  showHelp("Datasets already loaded into R can be used
(although a copy is taken, with memory implications).
Only data frames are currently supported, and 
the names of all of the available data frames will be lsited.
<<>>
The data frames need to be constructed in the same R session
that is running Rattle (i.e., the same R Console in which you
sourced the Rattle package). This provides much more flexibility in
loading data into Rattle, than is provided directly through the actual Rattle
interface. For example, you may want to use the SQLLite package to load
data from a database directly.")
}

on_help_odbc_activate <- function(action, window)
{
  if(showHelpPlus("Rattle can establish a connection to a database
through the RODBC package. Tables avilable in the database will then be
listed for selection."))
  {
    require(RODBC, quietly=TRUE)
    popupTextviewHelpWindow("RODBC")
  }
}

on_help_data_corpus_activate <- function(action, window)
{
  showHelp("The Corpus option of the Data tab will load and process a folder of
documents, referred to as a corpus. Each document will be processed according to the
options specified, to end up with a dataset with, in the simplest case, variables
corresponding to the keywords identified in the documents, and the entities
corresponding to each documents. The full functionality of Rattle can then be deployed
on this dataset.
<<>>
The tm package of R provides the underlying functionality to process a corpus.")
}

on_help_roles_activate <- function(action, window)
{
  showHelp("The Data tab allows you to select roles for the
variables.
<<>>
By default, all variables have an Input role, except for any variables
that have constant value, or categorics with as many values as there
are rows (identifier variables). These will be marked as Ignore.
<<>>
One variable may also be identified as the Target (the first or last
categoric by default).
<<>>
Modify the roles as appropriate for each variable.
<<>>
    Input: Used for modelling.
<<>>
    Target: Output for modelling.
<<>>
    Risk: A variable used in the Risk Chart
<<>>
    Ident: An identify for unique entities in the data set.
<<>>
    Ignore: Do not use this variable
<<>>
The Input and Ignore buttons can be used to operate on a selection.
A Shift-Click in the variable list will select all variables from the
last click to current variable. A Ctrl-Click will add the current
variable to those selected.")
}

on_help_weight_calculator_activate <- function(action, window)
{
  showHelp("Weights are used by variable modellers to identify
some entities as more important than others. The Weights Calculator
can be used to specify a formula in terms of the variables in the dataset.
You can list just a variable name, or
any formula can be used, as long as it is a valid R formula - R will be
asked to evaluate the formula.
<<>>
An example might be 'abs(Rev)/max(Rev)*10+1' which takes the absolute
value of a variable called Rev, divides it by the maximum value of Rev in the
dataset, times 10, adding 1 to it to give numbers from 1 up.")
}

on_help_sample_activate <- function(action, window)
{
  showHelp("Sampling is activated by default, randomly choosing 70%% of the
data for a training dataset and 30%% for a test dataset. The training dataset
is used to build models whilst the test dataset is used to evaluate the
models on otherwise unseen data.
<<>>
A new random sample is extracted each time the tab is executed. However,
you will get the same random sample each time, for a given seed. Changing the
seed allows different random samples to be extracted. This could be useful in
testing the sensitivity of modelling with different training sets.")
}

########################################################################
# EXPLORE TAB

on_help_summary_activate <- function(action, window)
{
  if (showHelpPlus("A summary of the dataset includes various pieces of
information about each of the variables of the dataset.
<<>>
For numeric data, this
can include the minimum, maximum, median (the value of the variable at the
midpoint of the dataset), mean (the average value of the variable),
and the first and third quartiles (25 percent of the data has values
below the first
quartile, and another 25 percent of the data has values above the third quartile).
<<>>
For categoric data the frequency distribution across the values is listed.
If there are too many possible values, then only the top few are listed, with
the remainder counted as Other.
<<>>
The R function summary() is used for the summary.
<<>>
Additional or differently presented summary information is provided
through additional options. Describe produces a similar summary presented
differently. For numeric variables, the Basic statistics can be obtained,
including kurtosis and skewness.
<<>>
The kurtosis is a measure of the nature of the peaks
in the distribution of the data. A high kurtosis indicates a sharper peak
and fatter tails while a lower kurtosis indicates a more rounded peak
with wider shoulders.
<<>>
The skewness indicates the assymetry of the distribution. A positive skew
indicates that the tail to the right is longer, and a negative skew that the
tail to the left is longer.
<<>>
The fBasics package is used for the Basic summary and
the kurtosis and skewness."))
    {
      popupTextviewHelpWindow("summary")
      if (packageIsAvailable("Hmisc", "display help about describe"))
      {
        require(Hmisc, quietly=TRUE)
        popupTextviewHelpWindow("describe")
      }
      if (packageIsAvailable("fBasics", "display help about basic stats"))
      {
        require(fBasics, quietly=TRUE)
        popupTextviewHelpWindow("basicStats")
      }
    }
}

on_help_distributions_activate <- function(action, window)
{
  if (showHelpPlus( "Choose from various plot types to display
information about the distributions of data."))
    popupTextviewHelpWindow("boxplot")
}

on_help_ggobi_activate <- function(action, window)
{
  if (showHelpPlus( "Run the GGobi application to visually explore
your data. GGobi is a very powerful interactive visualiser.
You will need to have the separate GGobi application installed,
as well as the rggobi R package."))
    popupTextviewHelpWindow("ggobi")
}

on_help_correlation_activate <- function(action, window)
{
  if (showHelpPlus( "A pairwise correlation between each numeric variable
is calculated and displayed numerically in the text window whilst
a graphic plot is also generated. The plot uses circles and colour to
indicate the strength of any correlation.
<<>>
The R function cor() is used to produce the correlation data."))
    popupTextviewHelpWindow("cor")
}

on_help_hierarchical_correlation_activate <- function(action, window)
{
  if (showHelpPlus( "A hierarchical cluster
of the correlations between the variables of the dataset is generated, and
presented pictorially as a dendrogram.  From the dendrogram you can
see groups of variables that are highly correlated. The code uses the
cor() function to gnerate the correlations between the variables, the
hclust() function to perform the hierarchical clustering, and converts
the result to a dendrogram, using as.dendrogram(), for plotting."))
  {
    popupTextviewHelpWindow("cor")
    popupTextviewHelpWindow("hclust")
    popupTextviewHelpWindow("dendrogram")
  }
 
}

on_help_principal_components_activate <- function(action, window)
{
  if (showHelpPlus("Principal components analysis identifies
a collection of derived variables (expressed as a linear combination
of the other variables) that account for the variance
in the data. Often, the first few components account for the majority
of the variation.
<<>>
After performing the analysis two plots will appear. The bar chart or scree plot
shows the importance of each component. The importance is based on how much
variance each component explains. Generally, we can use this plot to how many
prinicple components we may want to keep if we were to use them in modelling.
<<>>
The second plot (a biplot) remaps the data points from their original
coordinates to coordinates of the first two principal coordinates.
The vectors drawn give an indication of how much
of a role each variable plays in each of the two components, showing their correlation
to the components. The axis are labelled with the correlation, to be
interpreted for the variables, and the values of the principal
components, to be interpreted for the data points.
<<>>
There will be as many components as there are (numeric) variables in
the dataset, but by discarding those components contributing very
little, you may end up with fewer variables for modelling. The textual information
provided can be used to guide the choice of which variables are included.
The final table will clearly identify how much of the variation in the data
is accounted for by each component.
<<>>
Interpretability may reduce through using the derived variables rather
than the original variables, so you may like to instead identify those
variables that contribute most to the first few principal components.
<<>>
The prcomp() function is used to generate the principal components
which are then displayed in the textview and the relative importance
of the components is plotted.
<<>>
Note that only numeric data is included in the analysis."))
    popupTextviewHelpWindow("prcomp")
}

########################################################################
# TEST TAB

on_help_test_kolmogorov_smirnov_activate <- function(action, window)
{
  if (showHelpPlus("The Kolmogorov-Smirnov test is a test to determine whether
the two samples are similarly distributed, without saying what kind of distriubtion
they have.
<<>>
The fBasics package provides the ks2Test function to perform the
Kolmogorov-Smirnov Test."))
  {
    if (packageIsAvailable("fBasics", "display help about Kolmogorov-Smirnov test"))
      popupTextviewHelpWindow("ks2Test")
  }
}

on_help_test_wilcoxon_activate <- function(action, window)
{
  if (showHelpPlus("The Wilcoxon test, also known as the Mann-Whitney test,
is analogous to the two-sample t-test, but performed on the rankings of the
combined data sets instead of on the actual measure. If the observations
rankings are not different, then the samples are not different. Because it is
performed on the rankings, it is more sensitive about the location of the
distribution, i.e. to the median (not the mean as in the T-Test).
<<>>
The fBasics package provides the wilcox.test function to perform the
Wilcoxon Test."))
  {
    if (packageIsAvailable("fBasics", "display help about the Wilcoxon test"))
      popupTextviewHelpWindow("wilcox.test")
  }
}

on_help_test_t_activate <- function(action, window)
{
  if (showHelpPlus("The T-test is the most commonly used test to determine whether
the means of two normally distributed samples are of equal sizes. The mean is a
measure of the location of the distribution. If the two populations are normal
(bell shaped) and their means are different, then the two bell shapes will be
offset from one another, indicating that the two samples are different. If the
means are equal the bell shapes will overlap.
<<>>
The fBasics package provides the locationTest function with the 't' method
to perform the T-test."))
  {
    if (packageIsAvailable("fBasics", "display help about the T-test"))
      popupTextviewHelpWindow("locationTest")
  }
}
on_help_test_f_activate <- function(action, window)
{
  if (showHelpPlus("The F-test is used to test whether the variance of the data
from two normally distributed samples is the same.
<<>>
The fBasics package provides the varianceTest function with the 'varf' method
to perform the F-test."))
  {
    if (packageIsAvailable("fBasics", "display help about the F-test"))
      popupTextviewHelpWindow("varianceTest")
  }
}

on_help_test_correlation_activate <- function(action, window)
{
  if (showHelpPlus("The Correlation test is used to test for the existence of
a linear relationship between the two variables. Only Pearson's Correlation Test
is performed in the Test tab.
<<>>
The fBasics package provides the correlationTest function
to perform the correlation test."))
  {
    if (packageIsAvailable("fBasics", "display help about the Correlation test"))
      popupTextviewHelpWindow("correlationTest")
  }
}

on_help_test_wilcoxon_signed_rank_activate <- function(action, window)
{
  if (showHelpPlus("The Wilcoxon signed rank test is used to test two related
samples, such as matched pairs or before and after tests, to determine if the
repeated measurements on the same individuals are the same.
<<>>
The fBasics package provides the wilcox.test function with the paired option
to perform the Wilcoxon signed rank test."))
  {
    if (packageIsAvailable("fBasics", "display help about the Correlation test"))
      popupTextviewHelpWindow("wilcox.test")
  }
}

########################################################################
# TRANSFORM TAB

on_help_normalise_activate <- function(action, window)
{
  if (showHelpPlus("Rescaling options transforms a variable by remapping its
values to another set of values, such as a set that has a mean of 0 and
standard deviation of 1. Often we do this so that all of our variables
have a very similar spread, and perhaps distribution. This can then avoid
biases in various algorithms, such as in clustering where a distance measure
is often used.
<<>>
Various rescalings are supported, with the rescaler function from the reshape
package used in a number of cases.
<<>>
The Nolan Transform segments and remaps a numeric variable to the 0-100 range.
"))
  {
    if (packageIsAvailable("reshape", "display information about rescaler"))
      popupTextviewHelpWindow("rescaler")
  }
}

on_help_transform_recenter_activate <- function(action, window)
{
  showHelp("Recenter performs a standard z-score transformation.
The variable's mean value is subtracted from each value, and each is then divided
by the standard deviation. The resulting variable will have a mean of 0 and a
standard deviation of 1. The new variable will have a prefix of RRC_.")
}

on_help_transform_scale01_activate <- function(action, window)
{
  showHelp("Scale [0-1] maps the variable into the 0-1 range.
The new variable will have a prefix of R01_.")
}

on_help_transform_medianmad_activate <- function(action, window)
{
  showHelp("-Median/MAD is a robust version of the standard z-score transform.
The variable's median value is subtracted from each value, and each is then
divided by the median absolute deviation, which is basically the median of
the residuals
or deivations from the data's median, as in Xi-median(X). The resulting
variable will have a median of 0.
It is more resilient to outliers than the normal z-score.
The new variable will have a prefix of RMD_.")
}

on_help_transform_rank_activate <- function(action, window)
{
  showHelp("The Rank transformation will sort the unique numeric values,
and then assign a rank instead of the value. A rank starts from 1 for the minimum
value and increases by 1 for each value up to the maximum value.")
}

on_help_transform_log_activate <- function(action, window)
{
  showHelp("The Log transformation maps the variable using the log function.")
}

on_help_transform_nolan_activate <- function(action, window)
{
  showHelp("The Nolan Groups transformation segments the selected
numeric variables
by a selected categoric variable, and then within each segment rescales the numeric
variable's range to the 0-100 range, using the range option of the rescale(rehsape)
function. This transform was proposed by Anthony Nolan.")
}

on_help_transform_matrix_activate <- function(action, window)
{
  showHelp("The Matrix transformation simply treats the chosen numeric columns
as a matrix. The total sum of the matrix is determined and each value is then
divided by the total.")
}

#------------------------------------------------------------------------

on_help_transform_impute_zero_activate <- function(action, window)
{
  showHelp('Imputation is used to fill in the missing values in the data.
The Zero/Missing imputation is a very simplle method.
It uses a constant value (0 for numeric data
and the new level, "missing" for categoric data)
to replace each missing value in the selected variable(s).
<<>>
The imputation of 0 is a good choice if the missing values are likely to
indicate a 0 rather than being unknown. Otherwise, the imputation of 0
may be misleading and is not recommended as it could change the
variables distribution, and hence result in poor models..')
}

on_help_transform_impute_mean_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Mean imputation uses the mean (i.e., the average value)
of the variable to replace each missing value in the selected variable(s).
<<>>
Note that this kind of imputation is not recommended as it could change the
variables distribution, and hence result in poor models.")
}

on_help_transform_impute_median_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Median imputation uses the median (the middle value)
of the variable to replace each missing value in the selected variable(s).
<<>>
Note that this kind of imputation is not recommended as it could change the
variables distribution, and hence result in poor models.")
}

on_help_transform_impute_mode_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Mode imputation uses the mode (the most frequently occuring value)
of the variable to replace each missing value in the selected variable(s).
<<>>
Note that this kind of imputation is not recommended as it could change the
variables distribution, and hence result in poor models.")
}


on_help_transform_impute_constant_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Constant imputation uses the supplied constant value
to replace each missing value in the selected variable(s).
<<>>
Generally, imputing
to a constant value, whether it is zero, the mean, median, or mode, or a user
specified value, is not recommended as it could change the
variables distribution, and hence result in poor models.")
}

#------------------------------------------------------------------------

on_help_transform_remap_quantiles_activate <- function(action, window)
{
  if (showHelpPlus("Binning based on quantiles will divide the population up
into nearly equally sized groups."))
  {
    popupTextviewHelpWindow("quantiles")
  }
}

on_help_transform_remap_equal_activate <- function(action, window)
{
  showHelp("Binning based on partitioning the numbers between the minimum and the
maximum value of the variable into equal width segments.")
}

on_help_transform_remap_kmeans_activate <- function(action, window)
{
  showHelp("Binning based on using kmeans clustering will divide the population up
into groups based on distances.")
}

on_help_transform_remap_indicator_activate <- function(action, window)
{
  showHelp("Convert a categoric variable into a collection of indicator variables,
with each new variable corresponding to one of the levels of the categoric variable.
The resulting set of indicator variables each has one of two possible values, 0 or 1.
Only one of the indicator variables, for a particular entity, will have a 1,
and all the rest are necessarily 0. The 1 corresponds to the actual value of the
original variable.")
}

on_help_transform_remap_joincat_activate <- function(action, window)
{
  showHelp("Join two or more categoric variables into a single new categoric variable
which has a new level for each possible value of the contributing levels.
Thus, joining Sex (Male and Female) with AgeGroup (Young, Middle, Old) will result in
JN_Sex_AgeGroup with levels MaleYoung, MaleMiddle, MaleOld, FemaleYoung,
FemaleMiddle, and FemaleOld.")
}

on_help_transform_remap_ascat_activate <- function(action, window)
{
  showHelp("Convert a numeric to a categoric, by turning each distinct value
of the numeric variable in a level for a categoric variable.")
}


on_help_transform_remap_asnum_activate <- function(action, window)
{
  showHelp("Convert a categoric to a numeric, by replacing each level with
the numeric index of the level.")
}

#-----------------------------------------------------------------------

on_help_transform_cleanup_ignored_activate <- function(action, window)
{
  showHelp("Remove any variable that is marked as Ignore.")
}

on_help_transform_cleanup_selected_activate <- function(action, window)
{
  showHelp("Remove all selected variables.")
}

on_help_transform_cleanup_missing_activate <- function(action, window)
{
  showHelp("Remove any variable that has any missing values.")
}

on_help_transform_cleanup_emissing_activate <- function(action, window)
{
  showHelp("Remove any entities (rows) that have any mising values.")
}

########################################################################
# CLUSTER TAB

on_help_kmeans_activate <- function(action, window)
{
  if (showHelpPlus("KMeans is a traditional approach to clustering.
In addition to building a cluster, a discriminant coordinates plot
can be generated, using the package fpc, as a display of the clusters."))
  {
    popupTextviewHelpWindow("kmeans")
    if (packageIsAvailable("fpc", "view documentation for plotcluster"))
    {
      require(fpc, quietly=TRUE)
      popupTextviewHelpWindow("plotcluster")
    }
  }
}

on_help_cluster_hclust_activate <- function(action, window)
{
  if (showHelpPlus("A hierarchical cluster is build as a hierachy of clusters,
starting with each entity defining its own cluster. The two closest clusters are then
cobined to generate a new cluster. This is repeated until all entities are in the
one cluster. Thus a hierachy is generated.
<<>>
A complete clustering is built. After building, we can choose a specific number of
clusters that appear 'right' by viewing the dendrogram. Statistics about the chosen
number of clusters can then be produced."))
  {
    popupTextviewHelpWindow("hclust")
  }
}

########################################################################
# MODEL TAB

on_help_glm_activate <- function(action, window)
{
  if (showHelpPlus("A tradition approach to model building is
regression. Logistic regression (using the binomial family) is used
to model binary outcomes. Linear regression (using the gaussian family)
is used to model a linear numeric outcome. For predicting where the
outcome is a count, the poisson family is used. Further families are
available, but for now require you to run the glm command directly.
Please see the additional documentation.
<<>>
The R function glm() is used for regression."))
  {
    popupTextviewHelpWindow("glm")
    popupTextviewHelpWindow("family")
  }
}

on_help_support_vector_machine_activate <- function(action, window)
{
  if (showHelpPlus("SVM (Support Vector Machine) is a modern approach
to modelling where the data is mapped to a higher dimensional space so
that it is more likely that we can find vectors separating the classes.
Rattle deploys ksvm from the kernlab package."))
  {
    #if (packageIsAvailable("e1071", "view documentation for e1071"))
    #{
    #  require(e1071, quietly=TRUE)
    #  popupTextviewHelpWindow("svm")
    #}
    if (packageIsAvailable("kernlab", "view documentation for kernlab"))
    {
      require(kernlab, quietly=TRUE)
      popupTextviewHelpWindow("ksvm")
    }
  }
}

on_help_confusion_table_activate <- function(action, window)
{
  if (showHelpPlus("An error matrix concisely reports the performance
of a model against a testing dataset. Generally, the number of entities
predicted by the model into each of the classes is presented against the
actual class to which that entity belongs. Rattle reports two error matricies.
The first is the raw entity counts whilst the second reports the
percentages."))
  {
    popupTextviewHelpWindow("table")
  }
}

on_help_risk_chart_activate <- function(action, window)
{
  showHelp("A risk chart plots population proportion along the X axis and a
perforamnce measure along the Y axis. If a Risk variable is identified in the
Data tab then the amount of risk covered is included in the chart (the red line).
The green line indicates the proportion of known targets that are identified
for any proportion of the population covered. The diagonal black line is a baseline,
indicating performance if cases were selected randomly.")
}

on_help_cost_curve_activate <- function(action, window)
{
  showHelp("A cost curve ...")
}

on_help_lift_activate <- function(action, window)
{
  showHelp("A lift chart ...")
}

on_help_sensitivity_activate <- function(action, window)
{
  if (showHelpPlus("The Sensitivity versus Specificity chart
is simply an alternative ROC curve, with Sensitivity being the
true positive rate (the count of true positives divided by the
count of positives) and Specificity being the true negative rate
(the count of true negatives divided by the count of negatives).
An ROC curve has the false positive rate instead of Specificity, which
is simply the count of false positives divided by the number of negatives
(1-fnr)."))
  {
    require(ROCR, quietly=TRUE)
    popupTextviewHelpWindow("performance")
  }
}

on_help_log_activate <- function(action, window)
{
  showHelp("The Log tab records the underlying commands that
Rattle generates and passes over to R to execute.
You can save the Log commands to file to run at a later stage,
or you can paste the commands into the current R console to do
more than Rattle can support.")
}
