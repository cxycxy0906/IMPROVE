### Xiaoya Cheng, 9/2/2014 ###
### Plot the standard deviation of field blanks over 2005-2014 based on arthist.dbf ###


setwd("M:\\1. UCD_Work\\Data Redelivery\\Redl of 2010thru2013_conterr")

df <- read.csv('arthist_std_plot.csv', sep = '|')

df[df==-999] <- NA

df$MONTH <- as.factor(df$MONTH)
df$YEAR <- as.factor(df$YEAR)

attach(df)

keep.column <- c(grep('^YEAR$', names(df), ignore.case = FALSE),
                 grep('^MONTH$', names(df), ignore.case = FALSE),
                 grep('^D', names(df), ignore.case = FALSE))

df.std <- df[, keep.column] 

install.packages('reshape')
library(reshape)
df.long <- melt(df.std, id.vars = c('YEAR', 'MONTH'), 
                variable.name = "Species", value.name = "Std", na.rm = TRUE)

library(ggplot2)
pdf('FB_std_arthist.pdf', width=11, height=8.5)

by(df.long, df.long$variable, function(t)
  ggplot(data=t, aes(x=MONTH, y=value, group=YEAR, color=YEAR)) + 
    geom_point(size=4) + 
    xlab("Month") + 
    ylab(t$variable) +
    ggtitle('Standard Deviation of Field Blanks')
  )

dev.off()



