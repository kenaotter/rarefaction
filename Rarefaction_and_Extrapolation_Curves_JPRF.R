#' ---
#' title: Rarefaction curves for JPRF avian diversity studies. 
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     toc: true
#' ---

#' # Initial Set Up
#' First - install iNEXT package from CRAN and several other packages that we need to run the analyses:
#' iNEXT, vegan, permute, lattice, dplyr, tidyr, & ggplot2.  

library(iNEXT)  # this is the package specifically for running Rarefaction/Interpolations and calculating diversity
library(vegan)  # this is a stats package for a lot of non-linear modeling, which interacts with iNEXT
library(dplyr)  # dplyr and tidyr are data management packages
library(tidyr)
library(ggplot2) # graphics
  theme_set(theme_bw())  # this sets the default theme for ggplot - it is optional.


#'---------------
#'# Load Data
#'---------------
#'I set this up as a Project, which automatically set up a working directory.
#' Put the data file into a folder, and set your working directory to point to it...


jprfpre <- read.csv("2019_detection_totals_jprf.csv") 

#' use the str() function to get a glimpse of the dataset....

str(jprfpre) 

#' to convert the first column to row headers, which is needed for later analysis, do the following:

jprf <- jprfpre[,-1]
rownames(jprf) <- jprfpre[,1]

#' Square brackets means you are specifying something in your dataframe to manipulate. 
#' Within the brackets, their are two positions specifying how your dataset is set up 
#' [rows, columns] separated by a common.   Technically this is [observations, variables], 
#' as each row should be an independent observation, and your variables are in columns.  
#' By leaving a blank before the comma, you are specifying to retain all observation (rows), 
#' and the -1 in the variables spot after the common means 'drop the first column'.
#' The above script is essentially saying in the first line "make a new dataframe 
#' called jprf by using the jprfpre dataframe and dropping the first column".  The second line
#' then says "assign the row names in your jprf dataset from the first column in the jprfpre dataset"
#' if you now view the dataframe, you should notice that the column X is now rownames.

str(jprf) 

#' this should now show you there are three variables, associated with our site types 
#' (clear.cuts, retention.sites and forest).  There should be no mention of $X and bird names.

head(jprf) 

#' 'll the bird names should be bolded as the row names now.  If you got this, we should be good to go.
#' We don't need a couple of the variables in this dataset for doing rarefaction, 
#' so will need to remove them from the dataframe.  Specifically, these are the 4th
#' (Habitat Classification) and 5th columns (Agreement between Classifiers).  
#' So, we are only going to keep variables 1-3, the numbers of observations in each
#' habitat.  To specify particular columns you want to subset into a new dataframe, 
#' simply use the square brackets, and the c() to list these; you can make a new 
#' dataframe with only the columns you want.

jprfTotals <- jprf[, c(1,2,3)]


#'  Next, you can get full info on rarefaction/extrapolation using the iNext function (main function of the program)
#' The default argument for the function is:

iNEXT(jprfTotals, q=0, datatype="abundance", size=NULL, endpoint=800, knots=40, se=TRUE, conf=0.95, nboot=50)

#' "q" is a number or vector specifying the diversity order(s) of Hill numbers.  default is zero.  
    #' q IS ASSOCIATED WITH DIFFERENT SPECIES INDICES - using q=0, the curve generated 
    #' is Species Richness, q=1 is Shannon Diversity, and q=2 is Simpson's Diversity.
    #' found this explanation in "intro to iNEXT: 
         #' "iNEXT focuses on three measures of Hill numbers of order q: species richness ( q = 0 ), 
         #' Shannon diversity ( q = 1 , the exponential of Shannon entropy) and 
         #' Simpson diversity ( q = 2 , the inverse of Simpson concentration)."
#' "datatype" can either be "abundance", “incidence_raw” or “incidence_freq” - 
    #' this depends on how the data is arranged in the initial file
#' "size" is sample sizes for which diversity estimates will be computed. 
    #'   If size is set at NULL, then diversity estimates will be determined by 
    #'   the specified/default endpoint and knots (see below).
#' "endpoint" specifies the sample size that is the endpoint for Rarefaction/Extrapolation (R/E) calculation; 
    #' If NULL, then endpoint=double the reference sample size.
#' "knots" is	an integer specifying the number of equally‐spaced knot- or points 
    #' on the x axis where the function will estimate species diversity- between size 1 and the endpoint.
#' "se"	is a logical variable to calculate the bootstrap standard error and conf 
    #' confidence interval.  It is listed as TRUE if you want it to calculate se, FALSE if you don't.
#' "conf" is a positive number < 1 specifying the level of confidence interval.  
    #' It will default to 0.95 unless you specify otherwise, but 95% is standard we want to use.
#' "nboot" is an integer specifying the number of bootstrap replications.  Default is 50.
    #' Interestingly, I tried changing this, as often random sampling will use 1000 - 
    #' this would typically make the estimated mean better, but would have a more 
    #' major impact on reducing the SE and confidence limits with more iterations.  
    #' However, in this package, it made NO EFFECT.   The output was the same if nboot is 50 vs 500.

iNEXT(jprfTotals, q=0, datatype="abundance", size=NULL, endpoint=800, knots=40, se=TRUE, conf=0.95, nboot=500)  

#' compare the output with first pass - this differs only by the number of bootstraps, but the output is virtually identical


#'----------------
#'' GRAPHIC DISPLAYS: FUNCTION ggiNEXT()
#'----------------

#'The function ggiNEXT(), which extends ggplot2 to the "iNEXT" object with default 
#' arguments, is described as follows:

#' ggiNEXT(x, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)  

#' Here x is an "iNEXT" object, so you have to specify this as "output <- ....". 
#' Three types of curves are allowed:
    #' Sample-size-based R/E curve (type=1). This curve plots diversity estimates with 
        #' confidence intervals (if se=TRUE) as a function of sample size up to double 
        #' the reference sample size, by default, or a user‐specified endpoint.
    #' Sample completeness curve (type=2) with confidence intervals (if se=TRUE). 
        #' This curve plots the sample coverage with respect to sample size for 
        #' the same range described in (1).
    #' Coverage-based R/E curve (type=3). This curve plots the diversity estimates 
        #' with confidence intervals (if se=TRUE) as a function of sample coverage 
        #' up to the maximum coverage obtained from the maximum size described in (1).
        
#' The argument facet.var=("none", "order", "site" or "both") is used to create a 
#' separate plot for each value of the specified variable.
#' 
#' You can calculate this with different Hill numbers, ranging from q=0, 1 or 2.  
#' If you do it individually, you can plot the individual numbers.  If you use 
#' the c(0,1,2) for q, it will plot all three on the same graph (which is the option 
#' we used).
#' 
#' If you don't specify the endpoint, it calculates it as 2x the greatest number 
#' of individuals sampled in any one site.  If we want all the plots to end at the 
#' same value, just set this higher than the totals for the biggest column in your dataset.
#' 
#' Specifically for this study, the largest number of individuals detected was in 
#' forest habitats, and is slightly over 400, with retention sites slightly below 400
#' so I suggest we set this to 800.

jprf_plotdata0 <- iNEXT(jprfTotals, q=0, datatype="abundance", endpoint = 800) # this will be species richness
jprf_plotdata1 <- iNEXT(jprfTotals, q=1, datatype="abundance", endpoint = 800) # this will be the Shannon Index
jprf_plotdata2 <- iNEXT(jprfTotals, q=2, datatype="abundance", endpoint = 800) # this will be the Simpson Index
jprf_plotdata_all <- iNEXT(jprfTotals, q=c(0,1,2), datatype="abundance", endpoint = 800) # this would be all three


# Sample‐size‐based R/E curves, separating by "site"
ggiNEXT(jprf_plotdata0, type=1, facet.var="site")
ggiNEXT(jprf_plotdata1, type=1, facet.var="site")
ggiNEXT(jprf_plotdata2, type=1, facet.var="site")

# if you don't separate it by site (facet.var), it sits the three estimates on top of one another.
ggiNEXT(jprf_plotdata0, type=1) # THIS IS SPECIES RICHNESS
ggiNEXT(jprf_plotdata1, type=1) # SHANNON INDEX
ggiNEXT(jprf_plotdata2, type=1) # SIMPSON INDEX

#' MANUSCRIPT FIGURES
#' 
#' q=0 Species Richness
ggiNEXT(jprf_plotdata0, type=1) +
  theme(legend.position = "right") +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Guides", values = c("CC" = "black", "RET" = "blue", "FOR" = "dark green")) +
  scale_fill_manual(name = "se", values = c("CC" = "black", "RET" = "blue", "FOR" = "dark green")) +
  labs(x="Number of Individuals Sampled", y="Species Richness") 


#' q=1 Shannon Diversity
ggiNEXT(jprf_plotdata1, type=1) +
  theme(legend.position = "right") +
  theme_bw(base_size = 14) +
  scale_colour_manual(name = "Guides", values = c("CC" = "black", "RET" = "dark green", "FOR" = "blue")) +
  scale_fill_manual(name = "se", values = c("CC" = "black", "RET" = "dark green", "FOR" = "blue")) +
  labs(x="Number of Individuals Sampled", y="Shannon Diversity")


#' ----------------------------------
#'# last, if you want to find out how to cite the packages you used in publications, use:

  citation(package = "iNEXT") 
  