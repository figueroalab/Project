library(tidyverse)
library(readxl)
library(cowplot)


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
         mutate(Coverage = 100-coverage) %>% 
         select(-"percent", - "coverage") 

#just for clarity I am renaming the mutants
  
df_plot2 <- df_plot %>% 
  mutate(isolate = str_replace(isolate,"Sr27_", "Mutant ")) %>%
  mutate(isolate= str_replace(isolate,"SA", "SA-")) %>% 
  mutate(isolate = replace(isolate, isolate == "194", "194-1,2,3,5,6")) %>% 
  mutate(isolate = replace(isolate, isolate == "98", "98-1,2,3,5,6")) %>%  
  mutate(isolate = replace(isolate, isolate == "34M1", "34-2-12")) %>%  
  mutate(isolate = replace(isolate, isolate == "34M2", "34-2-12-13")) 

#convert to factor 

df_plot3 <- df_plot2 %>% 
  mutate(isolate = factor(isolate,levels = rev(unique(isolate)))) %>%  #to prevent the plot using alphabetical order change column to factor 
mutate(ID = factor(ID,levels = unique(ID))) 


#make a plot that shows coverage of each gene

del_heatmap <- ggplot(data = df_plot3,
       mapping= aes(y=isolate, x= ID, color = Coverage)) +
  geom_tile(aes(fill = Coverage), color="white") +
  theme_minimal() +
  theme(text = element_text(size=8),
        axis.text.x = element_text(angle=90, hjust=0.1,vjust=0.1, color= "black"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, color = "black")) + 
  ylab("Isolate") +
  xlab("Gene model") +
  scale_fill_gradient(low = "lightgray", high ="red") +
  scale_x_discrete(expand = c(0, 0),
                   position = "top") +
  coord_equal() 

#save plot
  
ggsave("results/deletion_heatmap.jpg", plot = del_heatmap, 
       width = 20, height = 10, units = "cm")

## second part of the figure

#dataframe was downloaded, and must be assigned to an 
#object to begin the work

Phenotypes <- read_excel("data/Phenotypes.xlsx")


#inspect dataframe (notice that there are a lot of NAs)
#also inspect using glimpse(Phenotypes) in console (notice that all columns are chr)
#notice I need to fix column name to be Sr_gene, see next line


colnames(Phenotypes)[1] <- "Sr_gene"

#use this in the console to see how many variables I have in column reaction
#unique(clean_phenotypes[3])
#this tells me that I have 6


#create a tidy dataframe

clean_phenotypes <- Phenotypes %>% 
  gather(- Sr_gene, key = "isolate", value = "reaction") %>%  # must have three columns
  mutate(reaction = replace_na(reaction,"N")) %>% # I now replace the empty cells (NA) with N
  mutate(reaction = replace(reaction, reaction == "?", "N")) %>%  # I now replace cells in the reaction column that are "?" to "N"
  mutate(reaction = replace(reaction, reaction == "A/V", "N")) %>% 
  mutate(reaction = replace(reaction, reaction == "A", "Avirulence")) %>% 
  mutate(reaction = replace(reaction, reaction == "V", "Virulence")) %>% 
  mutate(reaction = replace(reaction, reaction == "X", "Mesothetic")) %>%  
  mutate(reaction = replace(reaction, reaction == "N", "Unknown")) %>% 
  mutate(isolate = factor(isolate,levels = unique(isolate)))


# I am overwritting the previous dataframe to have the columns 
#in the order I would like

clean_phenotypes <- clean_phenotypes[c(2,1,3)]

#select data that is only for gene  Sr27
 

pheno_interest <- clean_phenotypes %>% 
  filter(Sr_gene =="27") 

df_plot4 <- right_join(pheno_interest, df_plot3, by = "isolate")

#generate plot 



heatmap_2 <- df_plot4 %>% 
  mutate(isolate = factor(isolate,levels = rev(unique(isolate)))) %>% 
  ggplot(
    mapping= aes(y=isolate, x= ID, color = Coverage)) +
  geom_tile(aes(fill = Coverage), color="white") +
  theme_minimal() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=0.1,vjust=0.1, color= "black"),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) + 
  ylab("Isolate") +
  xlab("Gene model") +
  scale_fill_gradient(low = "lightgray", high ="red") +
  scale_colour_manual("Virulence", values= c("grey70","black")) +
  scale_x_discrete(expand = expand_scale(mult = c(0, 0), add= c(7,0)),
                   position = "top") +
  coord_equal() +
  geom_text(aes(x = 0, y = isolate, label = isolate, colour = reaction), hjust = 1, size = 3)


ggsave("results/phenotype_heatmap.jpg", plot = heatmap_2, 
       width = 20, height = 16, units = "cm")



