##### Xiaoya Cheng, 8/18/2014 #####
#### This is a template of comparing a series of files 
#### using one command.

# 1. Columns we may want to exclude before comparing
no_column <- c("cld_err","cld_mdl","no2_err",  "no2_mdl","no3_err", "no3_mdl",	
               "so4_err",	"so4_mdl","nh4_err","nh4_mdl","o1_err","o1_mdl","o2_err",
               "o2_mdl","o3_err","o3_mdl","o4_err","o4_mdl","op_err",	
               "op_mdl",	"e1_err",	"e1_mdl",	"e2_err",	"e2_mdl",	"e3_err",	"e3_mdl",	"so2_err",	"so2_mdl")

# no_column_2 <- c("cld_err","cld_mdl","no2_err",  "no2_mdl","no3_err", "no3_mdl",  
#                "so4_err",	"so4_mdl","nh4_err","nh4_mdl","o1_err","o1_mdl","o2_err",
#                "o2_mdl","o3_err","o3_mdl","o4_err","o4_mdl","op_err",	
#                "op_mdl",	"e1_err",	"e1_mdl",	"e2_err",	"e2_mdl",	"e3_err",	"e3_mdl",	"so2_err",	"so2_mdl", 
#                "fabs_err", "fabs_mdl")


# 2. Remove suppose-to-change columns from both old and new files
setwd('../data_txt/')
files = list.files('.')

lapply(files, function(i)
                    {
                      t = read.csv(i)
                      write.csv(t[, !(names(t) %in% no_column)], paste("../data_clean/", i,"_clean.csv", sep=""))
                      }
                    )


# 3. Make comparisons

setwd('../data_clean/')
files_clean = list.files('.')

a <- lapply(files_clean, function(i){
  if (length(grep("(2)", i))>0) {           ## "(2)" in one of the two files to compare with each other
      t1 = read.csv(i)
      t2 = read.csv(paste(substr(i, 1,18), ".TXT_clean.csv", sep=""))
  
      if (identical(t1, t2) == 'FALSE') {
        print(i) 
      }
  }
})







