library(tidyverse)
library(readxl)

#dataframe was downloaded, and must be assigned to an 
#object to begin the work

Phenotypes <- read_excel("data/Phenotypes.xlsx")

#note that column 1, from row 1- 43 needs "Sr" must be in front characters


 

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
  mutate(reaction = replace(reaction, reaction == "N", "Unknown")) 


  
# I am overwritting the previous dataframe to have the columns 
#in the order I would like

clean_phenotypes <- clean_phenotypes[c(2,1,3)]

#create a heatmap to visualize data

heatmap <- ggplot(data = clean_phenotypes,
                  mapping = aes(x = isolate, y = Sr_gene)) +
           geom_tile (aes(fill = reaction), color="white") +
           ylab("Wheat differentials (Sr genes)") +
           xlab("Isolate ID") +
           scale_fill_manual(values = c("Avirulence"="yellow", "Unknown"= "grey","Virulence"= "blue", "Mesothetic" = "navy")) +
           theme(text = element_text(size=10),
           axis.text.x = element_text(angle=90, hjust=0.2,vjust=0.2, color= "black"),
           axis.text.y = element_text(angle=0, hjust=1,vjust=1, color = "black")) +
           scale_y_discrete(expand = c(0, 0))+
           scale_x_discrete(expand = c(0, 0), 
                            position = "top", 
                            limits = c("Ug99", "Pgt55", "Pgt59", "Pgt60","Pgt61","04KEN156","126-5,6,7,11", 
                            "21-0", "34-2-12", "34-2-12-13", "98-1,2,3,5,6", "194-1,2,3,5,6", 
                           "326-1,2,3,5,6", "SA-01", "SA-02","SA-03", "SA-04", "SA-05", "SA-06", "SA-07",
                           "Pgt62", "MCCFC", "RKQQC")) +
          coord_equal()

ggsave("results/first_heatmap.jpg", plot = heatmap, 
       width = 20, height = 35, units = "cm")



