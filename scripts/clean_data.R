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
 mutate(reaction = replace(reaction, reaction == "A/V", "N")) 


  
# I am overwritting the previous dataframe to have the columns 
#in the order I would like

clean_phenotypes <- clean_phenotypes[c(2,1,3)]

#create a heatmap to visualize data

ggplot(data = clean_phenotypes,
       mapping = aes(x = isolate, y = Sr_gene)) +
      geom_tile (aes(fill = reaction)) +
      ylab("Wheat differentials") +
      xlab("Isolate ID") +
      #scale_fill_brewer(palette= "Set5")
    scale_fill_manual(values = c("A"="yellow", "N"= "white","V"= "red", "X" = "orange"))

