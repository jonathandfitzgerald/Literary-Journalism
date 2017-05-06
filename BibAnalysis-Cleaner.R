### SECONDARY SOURCES ###

#Read in txt file and separate each line of Sims Bibliography (http://normansims.com/wp-content/uploads/2014/05/Bibliography-of-Literary-Journalism-Scholarship-and-Articles.pdf)
SimsBib = scan("data/SimsBib.txt", character(0), sep = "\n")
SimsBibDF = SimsBib %>% data.frame()
SimsBibDF = SimsBibDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
SimsBibDF = SimsBibDF %>% mutate("Year" = as.numeric(Year))
SimsBibDF = SimsBibDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
SimsBibDF = SimsBibDF %>% unite(id, author, Year, sep = "_")
SimsBibDF = SimsBibDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
SimsBibDF = SimsBibDF %>% mutate("Year" = as.numeric(Year))
names(SimsBibDF)[names(SimsBibDF)=="."] <- "fullCit"

#Read in txt file and separate each line of LJS Bibliography (https://www.dropbox.com/s/3a1wilg8lxngag5/LJS_Biblio_v120301.pdf?dl=0)
LJBib = scan("data/LJSBib.txt", character(0), sep = "\n")
LJBibDF = LJBib %>% data.frame()
LJBibDF = LJBibDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBibDF = LJBibDF %>% mutate("Year" = as.numeric(Year))
LJBibDF = LJBibDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJBibDF = LJBibDF %>% unite(id, author, Year, sep = "_")
LJBibDF = LJBibDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBibDF = LJBibDF %>% mutate("Year" = as.numeric(Year))
names(LJBibDF)[names(LJBibDF)=="."] <- "fullCit"

#Read in txt file and separate each line of LJS Bibliography (Fall 2011) (http://ialjs.journalism.ryerson.ca/wp-content/uploads/2012/01/125-127_SelectedBibliographyMaguire-2.pdf)
LJBib2 = scan("data/LJSBib2.txt", character(0), sep = "\n")
LJBib2DF = LJBib2 %>% data.frame()
LJBib2DF = LJBib2DF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBib2DF = LJBib2DF %>% mutate("Year" = as.numeric(Year))
LJBib2DF = LJBib2DF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJBib2DF = LJBib2DF %>% unite(id, author, Year, sep = "_")
LJBib2DF = LJBib2DF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJBib2DF = LJBib2DF %>% mutate("Year" = as.numeric(Year))
names(LJBib2DF)[names(LJBib2DF)=="."] <- "fullCit"

#Combine Sims and LJS Bibs [LETS DO THIS AT THE END]
secondary <- rbind(LJBibDF, SimsBibDF, LJBib2DF)
secondary = distinct(secondary, id, Year, fullCit)
secondary = secondary %>% 
  filter(Year>1850) %>%
  group_by(Year) %>%
  mutate(count=n()) %>% 
  mutate(Source = "Critical Works") %>% 
  mutate(fullCit = as.character(fullCit))

#visualize
secondary %>%
  filter(Year>1880) %>%
  ggplot() +
  geom_line() +
  aes(x=Year,y=count, color=Source) + 
  geom_point(alpha=.8) + 
  ggtitle("Number of Critical Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 

### PRIMARY SOURCES ###

#Read in txt file and separate each line of Primary (for Sims bibliography)
LJPrim = scan("data/LJPrimary.txt", character(0), sep = "\n")
LJPrimDF = LJPrim %>% data.frame()
LJPrimDF = LJPrimDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJPrimDF = LJPrimDF %>% mutate("Year" = as.numeric(Year))
LJPrimDF = LJPrimDF %>% mutate(author=gsub("(^[A-Za-z]{1,20}).*","\\1",.))
LJPrimDF = LJPrimDF %>% unite(id, author, Year, sep = "_")
LJPrimDF = LJPrimDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJPrimDF = LJPrimDF %>% mutate("Year" = as.numeric(Year))
names(LJPrimDF)[names(LJPrimDF)=="."] <- "fullCit"

#Read in txt file and separate each line of Primary (from Art of Fact TOC)
LJtoc = scan("data/ArtofFactTOC.txt", character(0), sep = "\n")
LJtocDF = LJtoc %>% data.frame()
LJtocDF = LJtocDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJtocDF = LJtocDF %>% mutate("Year" = as.numeric(Year))
LJtocDF = LJtocDF %>% mutate(author=gsub(".*( / )","\\1",.))
LJtocDF = LJtocDF %>% mutate(author=gsub("( --).*","\\1",LJtocDF$author))
LJtocDF = LJtocDF %>% mutate(author=gsub("[[:punct:]]","\\1",LJtocDF$author))
LJtocDF = LJtocDF %>% mutate(author=gsub(".*( [A-Z])","\\1",LJtocDF$author))
LJtocDF = LJtocDF %>% mutate(author=gsub(" ","\\1",LJtocDF$author))
LJtocDF = LJtocDF %>% unite(id, author, Year, sep = "_")
LJtocDF = LJtocDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) 
LJtocDF = LJtocDF %>% mutate("Year" = as.numeric(Year))
names(LJtocDF)[names(LJtocDF)=="."] <- "fullCit"

#Combine primary sources
primary <- rbind(LJtocDF, LJPrimDF)
primary = distinct(primary, id, Year, fullCit)
primary = primary %>% 
  filter(Year>1850) %>%
  group_by(Year) %>%
  mutate(count=n()) %>% 
  mutate(Source = "Primary Works") %>% 
  mutate(fullCit = as.character(fullCit))

#visualize (line and point)
primary %>%
  filter(Year>1880) %>%
  ggplot() +
  geom_line() +
  aes(x=Year,y=count, color=Source) + 
  geom_point(alpha=.8) + 
  ggtitle("Number of Primary Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 

#visualize (bar)
primary %>%
  filter(Year>1880) %>%
  ggplot() +
  geom_bar() +
  aes(x=Year) + 
  ggtitle("Number of Primary Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 

### COMBINED ###
bibAndPrim <- rbind(primary, secondary) 

#visualize
bibAndPrim %>%
  filter(Year>1880) %>%
  ggplot() +
  geom_line() +
  aes(x=Year,y=count, color=Source) + 
  geom_point(alpha=.8) + 
  ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 


bibAndPrim %>%
  filter(Year>1880, Year<2000) %>%
  ggplot() +
  geom_histogram() +
  aes(x=Year,fill=Source) + 
  ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 



#clear all
rm(list=ls())
