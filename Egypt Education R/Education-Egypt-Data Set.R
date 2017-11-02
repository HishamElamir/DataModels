###
# PREPARE AND SAVE SECTION
###
library(readxl)
Education_Egypt_Data_Set <- read_excel("D:/Master/Projects/DataScience/R/Egypt Education DS/Education-Egypt-Data Set.xls")
View(Education_Egypt_Data_Set)

# Getting Header of DATASET
header <- Education_Egypt_Data_Set[2,]

data.all <- Education_Egypt_Data_Set[0:-2,]
colnames(data.all) <- header
df <- data.frame(data.all)

write.csv( data.all, 'DataFrame.All.csv', row.names = T, sep = "," )

read.table('DataFrame.All.csv', header = T)

###
# CLEAN AND MANIPULATE SECTION
###

# Getting Egypt Rows
data.egypt <- df[c(7631:7739),]
colnames(data.egypt) <- header

# Save Rows
write.csv( data.egypt, 'DataFrame.Egypt.csv', row.names = T )


