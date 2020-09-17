library(dplyr)
library(questionr)

load("C:/Workspace/RStudioProjects/landlordism/Data/36151-0002-Data.rda")

#Select the urban households
#(0) rural 0 (1) urban 1 
#27579       14573 

hh_urban=da36151.0002%>% filter(URBAN2011=="(1) urban 1")

#From now on use hh_urban for any analysis
wtd.table()

hh_urban$quintiles = quant.cut(hh_urban$COPC,nbclass = 5,include.lowest = TRUE)

hh_urban=hh_urban %>% mutate(landlord=case_when(IN1>0~ "Landlord",
                                                IN1==0~"Not landlord"))

wtd.table(x=hh_urban$quintiles,
          y=hh_urban$landlord,
          weights = hh_urban$INDFWT)

wtd.table(x=hh_urban$CG1,
          y=hh_urban$landlord,
          weights = hh_urban$INDFWT)

prop.table(wtd.table(x=hh_urban$URBAN4_2011,
          y=hh_urban$landlord,
          weights = hh_urban$INDFWT),margin = 1)

prop.table(wtd.table(x=hh_urban$URBAN4_2011,
                     y=hh_urban$CG1,
                     weights = hh_urban$INDFWT),margin = 1)

hh_urban%>%
  group_by(quintiles)%>%
  summarise(income_share=wtd.mean(IN1*100/INCOME))

hh_urban

