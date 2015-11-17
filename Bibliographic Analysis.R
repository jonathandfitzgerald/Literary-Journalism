library(dplyr)
library(ggplot2)
library(tidyr)

#Read in txt file and separate each line of Sims Bibliography
SimsBib = scan("data/SimsBib.txt", character(0), sep = "\n")
SimsBibDF = SimsBib %>% data.frame()
SimsBibDF = SimsBibDF %>% mutate(year=gsub(".*(\\d{4}).*","\\1",.)) 
SimsBibDF = SimsBibDF %>% mutate("year" = as.numeric(year))
SimsBibDF = SimsBibDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
SimsBibDF = SimsBibDF %>% unite(id, author, year, sep = "_")


#Read in txt file and separate each line of Bibliography
LJBib = scan("data/LJSBib.txt", character(0), sep = "\n")
LJBibDF = LJBib %>% data.frame()
LJBibDF = LJBibDF %>% mutate(year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBibDF = LJBibDF %>% mutate("year" = as.numeric(year))
LJBibDF = LJBibDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJBibDF = LJBibDF %>% unite(id, author, year, sep = "_")

#Read in txt file and separate each line of LJS Bibliography (Fall 2011)
LJBib2 = scan("data/LJSBib2.txt", character(0), sep = "\n")
LJBib2DF = LJBib2 %>% data.frame()
LJBib2DF = LJBib2DF %>% mutate(year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBib2DF = LJBib2DF %>% mutate("year" = as.numeric(year))
LJBib2DF = LJBib2DF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJBib2DF = LJBib2DF %>% unite(id, author, year, sep = "_")

#Combine Sims and LJS Bibs
allBib <- rbind(LJBibDF, SimsBibDF, LJBib2DF)

allBibClean = distinct(allBib, id)

#Visualzing Critical texts by year
allBibViz = allBibClean %>% 
  filter(year>1850) %>%
  group_by(year) %>%
  mutate(count=n()) %>% 
  mutate(src = "Bib")


ggplot(allBibViz) + 
  geom_line(colour="blue") + 
  aes(x=year,y=count) + 
  geom_point(size=2,color="blue") + 
  geom_smooth(method="loess") + 
  ggtitle("Number of critical works published by year")


#Read in txt file and separate each line of Primary
LJPrim = scan("data/LJPrimary.txt", character(0), sep = "\n")
LJPrimDF = LJPrim %>% data.frame()
LJPrimDF = LJPrimDF %>% mutate(year=gsub(".*(\\d{4}).*","\\1",.)) 
LJPrimDF = LJPrimDF %>% mutate("year" = as.numeric(year))
LJPrimDF = LJPrimDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJPrimDF = LJPrimDF %>% unite(id, author, year, sep = "_")


#Visualizing Primary texts by year
primByYear = LJPrimDF  %>% 
  filter(year>1850) %>%
  group_by(year) %>%
  mutate(count=n()) %>% 
  mutate(src = "Prim")

ggplot(primByYear) + 
  geom_line(colour="blue") + 
  aes(x=year,y=count) + 
  geom_point(size=count,alpha=.6,color="blue") + 
  geom_smooth(method="loess")


ggplot(primByYear) + 
  geom_density(colour="blue") + 
  aes(x=year,y=count) + 
  geom_point(size=count,alpha=.6,color="blue") 


#combine Bib and Primary
bibAndPrim <- rbind(allBibViz, primByYear)

ggplot(bibAndPrim) +   
  geom_line() +
  aes(x=year,y=count, color=src) + 
  geom_point(size=count,alpha=.6) + 
  ggtitle("Number of primary and critical works published by year")


