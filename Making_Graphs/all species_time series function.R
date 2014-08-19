##### Xiaoya July 30, 2014 #####
### Function used to plot time-series data for the whole network for a given time period
### Different years are shown in different colors to compare with each other. 

# install.packages("RODBC")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("reshape2")

#------------------------------------------------------------
# 1. Multiplot function, called by allsp_timesr function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#-------------------------------------------------------------------------------
# CORE: all species_time series function

allsp_timesr = function(startdate, enddate, path){

#   Plot time-series data for the whole network for a given time period. 
#   Different years are shown in different colors to compare with each other. 
#   
#   Args:
#   startdate = the start date of the period to be ploted. In the form as "'2013-01-01'"  
#   enddate = the end date of the period to be ploted. In the form as "'2013-01-01'"
#   path = the directory to store the plot. In the form as 'C:/Users/xycheng/' or './'
#   
#   Returns:
#   time-series plots by percentile of each species for the given time period.
#   
#   Author:
#   Xiaoya Cheng
 
  
library(RODBC)
startdate.string <- paste("\'", startdate, "\'", sep='')
enddate.string <- paste("\'", enddate, "\'", sep='')

query = paste("select * 
               from AnalysisQA
               WHERE samdat between ", startdate.string, " and ", enddate.string,
               "and not(site like 'MEVES' or site like 'PHX1A' or site like 'PHX5A' or site like 'RICR1' 
                or site like 'ROMOS' or site like 'WICAS' or site like 'YELLS' )")
myconn = odbcConnect("CL-SQL")
original <- sqlQuery(myconn, query)

original[original == -999.00] <- NA

# Columns to be added

original$ec <- original$e1+original$e2+original$e3-original$op
original$oc <- original$o1 + original$o2 + original$o3 + original$o4 + original$op


# Columns to be excluded:

## If obtained the table from SQL, need to first mannually delete 
no_fields <-
  c(grep("^STRTIM$", names(original),ignore.case=TRUE),
    grep("^FLOW",names(original), ignore.case=TRUE),    # column name starts with "FLOW"
    grep("^TIME",names(original), ignore.case=TRUE),
    grep("^FLAG",names(original), ignore.case=TRUE), 
    grep("ERR$",names(original), ignore.case=TRUE),    # column name ends with...
    grep("MDL$", names(original), ignore.case=TRUE),
    grep("^CUXRFID$", names(original), ignore.case=TRUE),
    grep("^comments$", names(original), ignore.case=TRUE),    # not in TotalDBF but in SQL queries
    grep("^AnalysisQALK$", names(original), ignore.case=TRUE),  # not in TotalDBF but in SQL queries
    grep("^AnalysisQAID$", names(original), ignore.case=TRUE),
    grep("^H$", names(original), ignore.case=TRUE),
    grep("^OMH$", names(original), ignore.case=TRUE),
    grep("^RCMA$", names(original), ignore.case=TRUE),
    grep("^OMHC$", names(original), ignore.case=TRUE),
    grep("^RCMI$", names(original), ignore.case=TRUE),
    grep("^S3$", names(original), ignore.case=TRUE),
    grep("^NO2$", names(original), ignore.case=TRUE),
    grep("^O1$", names(original), ignore.case=TRUE),
    grep("^O2$", names(original), ignore.case=TRUE),
    grep("^O3$", names(original), ignore.case=TRUE),
    grep("^O4$", names(original), ignore.case=TRUE),
    grep("^E1$", names(original), ignore.case=TRUE),
    grep("^E2$", names(original), ignore.case=TRUE),
    grep("^E3$", names(original), ignore.case=TRUE),
    grep("^KNON$", names(original), ignore.case=TRUE),
    grep("^MOPT$", names(original), ignore.case=TRUE),
    grep("^LACN$", names(original), ignore.case=TRUE),
    grep("^OMCN$", names(original), ignore.case=TRUE))
    

## Exclude these columns
original <- original[, -no_fields]



# Add three columns "MM", YYYY" and "YYYYMM" to dataset 'original' ##
require(lubridate)

netdata <- cbind(MM = month(original$samdat), YYYY = year(original$samdat),YYYYMM = paste(year(original$samdat),month(original$samdat, label = TRUE)), original)

## Refine dataset
netdata[netdata == -999.00] <- NA

library(reshape2)
library(ggplot2)
 par(ask=FALSE)  ### plot each one after asking

# Convert to long format
netdata_long <- melt(netdata, id.vars = c("site","samdat","YYYYMM","YYYY","MM"), 
                     variable.name = "Species", value.name = "Conc", na.rm = TRUE)

netdata_long$YYYYMM = as.factor(netdata_long$YYYYMM)
netdata_long$YYYY = as.factor(netdata_long$YYYY)
netdata_long$MM = as.factor(netdata_long$MM)
netdata_long$Conc = as.numeric(netdata_long$Conc)


## Save plots in PDF
pdf(paste(path, startdate, "_", enddate, "_allspecies.pdf"), width=8.5, height=11)


## Make plots

#--- Use by(...) to apply a function to subsets of data (split by Species) ---#
by(netdata_long, netdata_long$Species, function(netdata_long)
  #--- Get quantiles of each subset of data ---#
  {Quan <- aggregate(netdata_long$Conc, by=list(YearMon=netdata_long$YYYYMM, Month=netdata_long$MM, Year=netdata_long$YYYY), 
                   FUN=function(x) quantile=quantile(x, c(0.1,0.5,0.9), na.rm=TRUE))
 
 p1 <- ggplot(Quan, aes(Month, x[,1], group=Year, color=Year)) +geom_line() + xlab("Month") + 
   ylab(netdata_long$Species) + geom_point(size=3) + ggtitle("10% Percentile") 
 p2 <- ggplot(Quan, aes(Month, x[,2], group=Year, color=Year)) +geom_line() + xlab("Month") + 
   ylab(netdata_long$Species) + geom_point(size=3) + ggtitle("Median") 
 p3 <- ggplot(Quan, aes(Month, x[,3], group=Year, color=Year)) +geom_line() + xlab("Month") + 
   ylab(netdata_long$Species) + geom_point(size=3) + ggtitle("90% Percentile")
 
 print(multiplot(p1,p2,p3, cols=1))}
)


dev.off()
}

