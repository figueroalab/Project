library(tidyverse)
library(readxl)

#dataframe deletions_Pgt21 was downloaded, and must be assigned to an 
#object to begin the work

Del_pgt21 <- read_excel("data/read_coverage_AvrSr27_deleted.xlsx")

#select columns of interest

gcoverage_isolate <- Del_pgt21 %>% 
  select("ID","21-0,percent zero bases", 
         "Sr27_1,percent zero bases","Sr27_2,percent zero bases",
         "Sr27_3,percent zero bases", "34M1,percent zero bases",
         "34M2,percent zero bases", "98,percent zero bases", 
         "194,percent zero bases", "SA01,percent zero bases",
         "SA02,percent zero bases","SA03,percent zero bases",
         "SA04,percent zero bases", "SA05,percent zero bases",
         "SA06,percent zero bases", "SA07,percent zero bases")

#make a tidy dataframe

tidy_gcoverage_isolate <- gcoverage_isolate %>% 
  gather(key = "Percent_coverage", value = "coverage", -ID)

#separate the cells from Percent_coverage by the ","

df_plot <- separate(tidy_gcoverage_isolate,
         Percent_coverage, into = c("isolate","percent"), sep = "," ) %>% 
        select(-"percent")

#make a plot

ggplot(df_plot,mapping= aes(x= "isolate", y= "ID", ))+

  geom_tile (aes(fill = coverage), color="white")+
  
  scale_colour_continuous()


