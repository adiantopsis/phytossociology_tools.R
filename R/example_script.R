## Loading functions
source("func/FitoR.R")
source("func/FitoR_camp.R")
source("func/parc_resume.R")

#### Forest
data<-read.csv(file="data/example_tab.csv", sep=";", dec=",")
data_corrected<-dap.eq(data) # Using only equivalent diameter 
fitoR(x=data, area=100, VI="cottam", filename="my_result")
fitoR(x=data_corrected, area=100, VI="cottam", filename="my_result")
parc.resume(data)

### Grassland
data_camp <- read.csv(file="data/example_tab_camp.csv", sep=";", dec=",")
fitoR_camp(x=data_camp, area=1, filename="my_result_camp")
