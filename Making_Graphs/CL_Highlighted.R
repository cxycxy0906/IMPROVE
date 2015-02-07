
install.packages("RODBC")
install.packages("ggplot2")

# Prepare the data

library(RODBC)
myconn <- odbcDriverConnect('driver={SQL Server};server=CL-SQL-SANDBOX;database=Improve; trusted_connection=true')

CL.test <- sqlQuery(myconn, 
                    "SELECT fl.flowflag1, fl.flowflag2, fl.flowflag3, fl.flowflag4  ,qa.site, qa.samdat, 
                        	mf, 1.293*no3 +1.375*so4 +1.6*cld +1.8*(o1+o2+o3+o4+op) +(e1+e2+e3-op) +17*fe as 'chem_mass', 
                          rcmc + 1.29*no3 as 'rcmn', flow_a
                      FROM AnalysisQA qa
                      JOIN LegacySampleFlow fl
                      	on qa.site = fl.sitename
                      		and qa.samdat = fl.sampledate
                      WHERE qa.samdat between '2011-01-01' and '2013-12-31'")

CL.test[CL.test == -999] = NA

# Eliminate the outlier
CL.test = CL.test[-which(CL.test$flow_a == 2.3), ]


# Plot

# !!! Create a look-up table for colors, mapping flowflag1 to categories (called palette in R)
# (only want to highlight CL and CG, but has flags more than that)
menu = c("CL "= "Clogged", "CG " = 'Clogging', "EP " = 'All 2011-2013', "LF " = 'All 2011-2013', "NM " ='All 2011-2013',"PO "='All 2011-2013',"RF "='All 2011-2013')
CL.test$Sample = menu[as.character(CL.test$flowflag1)]

# Plot and put Clogged as the top layer (by using subset...)
library(plyr)

plot = ggplot(data = CL.test) + 
  geom_point(aes(x = chem_mass/1000, y = mf/1000, color = Sample, size = Sample)) + 
  geom_point(aes(x = chem_mass/1000, y = mf/1000, color = Sample, size = Sample), subset = .(Sample == "Clogging")) +
  geom_point(aes(x = chem_mass/1000, y = mf/1000, color = Sample, size = Sample), subset = .(Sample == "Clogged")) +
  geom_abline(color = 'green', size = 1) +
  xlab("chemical mass, ug/m3") + ylab("gravimetric mass, ug/m3") +
  xlim(c(0,max(CL.test$chem_mass/1000))) +
  ggtitle("MF vs. Chemical Mass, after CL Criteria Changed") +
  scale_color_manual(values = c('blue', 'red', 'orange')) +
  scale_size_manual(values = c(2,4, 4)) +
  theme(text = element_text(size = 16))



# Backup-----------------------------------------------------------------------------------
# #----------
# # way1 plot and then add CL once more
# library(ggplot2)
# 
# plot = ggplot(data = CL.test,
#               aes(x = rcmn, y = mf)) + geom_point() + geom_abline()
# 
# plot + geom_point(data = CL.test[which(CL.test$flowflag1 == "CL "),], aes(x = rcmn, y = mf), color = 'red')
# 
# #------
# # way2 color by factor
# mycolor = vector(length = nrow(CL.test))
# mycolor[which(CL.test$flowflag1 == 'CL ')] = "Clogged"
# mycolor[which(CL.test$flowflag1 != 'CL ')] = "All 2011-2013"
# mycolor = as.factor(mycolor)
# 
# 
# # Highlight CL
# plot = ggplot(data = CL.test,
#               aes(x = rcmn, y = mf, color = mycolor)) + geom_point() + geom_abline()
# 
# #----
# # Duncan
# cols = c("CL "= "red", "CG " = 'black', "EP " = 'black', "LF " = 'black', "NM " ='black',"PO "='black',"RF "='black')
# colors = cols[as.character(CL.test$flowflag1)]
# 
# plot = ggplot(data = CL.test,
#               aes(x = rcmn, y = mf)) + geom_point(color = colors) + geom_abline()
# 
# #---
# # derived form Ducan's
# # Duncan
# menu = c("CL "= "Clogged", "CG " = 'All 2011-2013', "EP " = 'All 2011-2013', "LF " = 'All 2011-2013', "NM " ='All 2011-2013',"PO "='All 2011-2013',"RF "='All 2011-2013')
# Sample = menu[as.character(CL.test$flowflag1)]
# 
# plot = ggplot(data = CL.test,
#               aes(x = chem_mass/1000, y = mf/1000, color = Sample, size = Sample)) + 
#   geom_point() + geom_abline() +
#   xlab("chemical mass, ug/m3") + ylab("gravimetric mass, ug/m3") +
#   ggtitle("MF vs. Chemical Mass, after CL Criteria Changed") +
#   scale_color_manual(values = c('blue', 'red')) +
#   scale_size_manual(values = c(2,4)) +
#   theme(text = element_text(size = 16))


