### Xiaoya Cheng, 11/20/2014 ###
### This is to plot the Field Blank median used for each month. ###
### You can specify the months and the elements you want to plot ###

## To-do:  Need to convert to function! ##

install.packages("RODBC")
install.packages("lubridate")
install.packages("ggplot2")


library(RODBC)
myconn <- odbcDriverConnect('driver={SQL Server};server=CL-SQL;database=Improve_2.0;trusted_connection=true')

FB_median <- sqlQuery(myconn, 
                      "SELECT distinct aset.Id, aset.ConfigurationAlias, cali.Application, astat.Parameter, astat.Median, cor.SampleStartDate
                        FROM xrf.FieldBlankSets aset
                        JOIN xrf.FieldBlankStats astat
                          ON aset.Id = astat.FieldBlankSetId
                        JOIN xrf.MassLoadings m
                        	ON aset.Id = m.FieldBlankSetId
                        JOIN xrf.CorrectedSets cor
                        	ON cor.Id = m.CorrectedSetId
                        JOIN xrf.CalibrationSets cali
                        	ON cali.Id = m.CalibrationSetId
                        WHERE cor.Validity = 1
                        	and (cor.Id between 56 and 83
                            or cor.Id between 95 and 100)")   # Use cor.Id to specify the months to be plotted


index = which(FB_median[, 'Parameter'] %in% c('Cl', 'Se', 'Br', 'Pb', 'Rb'))  # Specify the elements to be plotted
FB_md_sub = FB_median[index, ]
# or plot all
FB_md_sub = FB_median



library(ggplot2)

pdf("M:\\1. UCD_Work\\Software & Applications Development\\XRF_Processing_StoredProc_Update\\FB_Median_A14Validation.pdf", width=18, height=7)

by(FB_md_sub, FB_md_sub$Parameter, function(x) {
  ggplot(x, aes(x=as.Date(SampleStartDate), y=Median, color=ConfigurationAlias)) + geom_point(size=5) +
    xlab('Month of Processed Set') + ylab(paste(x$Parameter, ", Median of Field Blanks, cps/mA", sep = ''))
})

dev.off()



# FB median is based on Configurations. But different Application might have slight influence. To show it , use the following
# by(FB_md_sub, FB_md_sub$Element, function(x) {
#   ggplot(x, aes(x=SampleStartDate, y=Median, color=ConfigurationAlias, shape=Application)) + geom_point(size=5) +
#     xlab('Month of Processed Set') + ylab(paste(x$Parameter, ", Median of Field Blanks, cps/mA", sep = ''))
# })


