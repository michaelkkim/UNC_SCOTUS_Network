#Analyze the scotus network (Supreme Court of the United States)
#created: 02/24/16

#Instructions:
    #Move this file to your computer!
    #modify the setup section to fit your computer
        #Set the working directory
        #install packages you are missing

#TODO:

#Problems:


#######
#Setup#
#######

#Set this to your own working directory
#workingdirectory <-"/Users/iaincarmichael/Dropbox/UNCDatsci/Law/Analyze_network/scotus/with_node_attributes"
#setwd(workingdirectory)
setwd("C:/Users/michael/Desktop/Scotus_Network_2/scotus/with_node_attributes")

library(igraph)

#Load the network 
citation_net <-read.graph( file='scotus_net_EL_date.txt', format="gml")



#########################
#Basic network summaries#
#########################
dates <- as.Date(V(citation_net)$date)

years <-dates %>% 
    as.character %>% 
    strsplit('-') %>%  
    sapply(function(x) x[1]) %>% 
    as.integer

tb <- table(years)
prob_years <- as.integer(names(tb[tb> 1000]))


degs<- degree(citation_net)
sum(degs[years %in% prob_years] == 0)

years_covered <- unique(years)
nzero <- rep(0, length(years_covered))
for(j in 1:length(years_covered)){
    y <- years_covered[j]
    nzero[j] <- sum(degs[years == y] == 0)
}
sum(degs[years == 1990] == 0)
