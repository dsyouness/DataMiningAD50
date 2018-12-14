library("readxl")
variable <- read_excel("Ecall.xlsx",range = "B3:B37",col_names = FALSE)
my_data <- read_excel("Ecall.xlsx",range = "C3:AR37",col_names = FALSE)


suppressWarnings( my_data )
library(FactoMineR)
my_data
res<-CA(my_data,variable)
summary(my_data)
