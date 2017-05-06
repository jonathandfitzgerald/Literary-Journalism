source("../functions.R")
### BEGIN SECONDARY LITERATURE ###

#Read in txt file and separate each line of Sims Bibliography (http://normansims.com/wp-content/uploads/2014/05/Bibliography-of-Literary-Journalism-Scholarship-and-Articles.pdf)
SimsBib = scan("../data/SimsBib.txt", character(0), sep = "\n") %>% data.frame() %>% mutate(Source = "SimsBib")

#Read in txt file and separate each line of LJS Bibliography of scholarship (https://www.dropbox.com/s/3a1wilg8lxngag5/LJS_Biblio_v120301.pdf?dl=0)
LJBib = scan("../data/LJSBib.txt", character(0), sep = "\n") %>% data.frame() %>% mutate(Source = "LJSBib")


#Read in txt file and separate each line of LJS Bibliography of Scholarship update (Fall 2011) (http://ialjs.journalism.ryerson.ca/wp-content/uploads/2012/01/125-127_SelectedBibliographyMaguire-2.pdf)
LJBib2 = scan("../data/LJSBib2.txt", character(0), sep = "\n") %>% data.frame() %>% mutate(Source = "LJSBib2")


#Combine Sims and LJS Bibs, remove duplicates
allSecondary <- rbind(LJBib, SimsBib, LJBib2) %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.))  %>% 
  mutate("Year" = as.numeric(Year)) %>% 
  mutate(author=gsub("((^[a-zA-Z]+( [a-zA-Z])?[a-zA-Z]*)|(^[a-zA-Z\u00C0-\u017F]*)|(^[a-zA-Z]+(’[a-zA-Z])?[a-zA-Z]*)), ([A-Za-z]{1,20}|[A-Za-z]{1,20}).*","\\1",.)) %>% 
  mutate(author_firstname=gsub("((^[a-zA-Z]+( [a-zA-Z])?[a-zA-Z]*)|(^[a-zA-Z\u00C0-\u017F]*)|(^[a-zA-Z]+(’[a-zA-Z])?[a-zA-Z]*)), ([A-Za-z]{1,20}|[A-Za-z]{1,20}).*","\\7",.)) %>% 
  mutate(title=gsub("(^.*(“(.*)\\W”.*))|(^.*?\\. ([A-Z].*?)\\. [A-Z]*.*)","\\3\\5",.)) %>%
  mutate(id = paste(author, Year, sep = "_")) %>% 
  rename(., fullCit = .) %>%
  group_by(fullCit) %>% 
  slice(1) %>% 
  mutate(Type = "Secondary Works")



### END SECONDARY LITERATURE ###

### BEGIN PRIMARY LITERATURE ###

#Read in txt file and separate each line of Primary (from Sims bibliography)
LJPrimDF = scan("../data/LJPrimary.txt", character(0), sep = "\n") %>% data.frame() %>% mutate(Source = "LJPrimary")
LJPrimDF = LJPrimDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) %>% 
  mutate("Year" = as.numeric(Year)) %>% 
  mutate(author=gsub("((^[a-zA-Z]+( [a-zA-Z])?[a-zA-Z]*)|(^[a-zA-Z\u00C0-\u017F]*)|(^[a-zA-Z]+(’[a-zA-Z])?[a-zA-Z]*)), ([A-Za-z]{1,20}|[A-Za-z]{1,20}).*","\\1",.)) %>% 
  mutate(author_firstname=gsub("((^[a-zA-Z]+( [a-zA-Z])?[a-zA-Z]*)|(^[a-zA-Z\u00C0-\u017F]*)|(^[a-zA-Z]+(’[a-zA-Z])?[a-zA-Z]*)), ([A-Za-z]{1,20}|[A-Za-z]{1,20}).*","\\7",.)) %>%
  mutate(title=gsub("(^.*(“(.*)\\W”.*))|(^.*?\\. ([A-Z].*?)\\. [A-Z]*.*)","\\3\\5",.)) %>%
  mutate(id = paste(author, Year, sep = "_")) %>%
  rename(., fullCit = .)


#Read in txt file and separate each line of Primary (from Art of Fact TOC)
AoFtocDF = scan("../data/ArtofFactTOC.txt", character(0), sep = "\n") %>% data.frame() %>% mutate(Source = "AoFTOC")
AoFtocDF = AoFtocDF %>% mutate(Year=gsub(".*(\\d{4}).*","\\1",.)) %>% 
  mutate("Year" = as.numeric(Year)) %>% 
  mutate(title=gsub("(^from )(.*)( /).*|(^.*)( /).*","\\2\\4",.)) %>%
  mutate(author=gsub(".*( / )","\\1",.)) %>% 
  mutate(author=gsub("( --).*","\\1",.$author)) %>% 
  mutate(author=gsub("[[:punct:]]","\\1",.$author)) %>% 
  mutate(author_firstname=gsub("([A-Z][a-z]*).*","\\1",.$author)) %>% 
  mutate(author_firstname=gsub(" ","\\1",.$author_firstname)) %>% 
  mutate(author=gsub(".*( [A-Z])","\\1",.$author)) %>% 
  mutate(author=gsub(" ","\\1",.$author)) %>% 
  mutate(id = paste(author, Year, sep = "_")) %>% 
  rename(., fullCit = .)


#Combine Sims and AoF Bibs
allPrimary <- rbind(LJPrimDF, AoFtocDF) %>% 
  group_by(fullCit) %>% 
  slice(1) %>% 
  mutate(Type = "Primary Works")


### END SECONDARY LITERATURE ###


#combine Bib and Primary
allBib <- rbind(allPrimary, allSecondary) 

# Gender?
genders = gender(allBib$author_firstname, years = c(1880, 1950))
genders <- genders[c("name", "gender")] %>% group_by(name) %>% slice(1)
names(genders)[names(genders)=="name"] <- "author_firstname"
allBib <- allBib %>% group_by(author_firstname) %>% left_join(genders, by = "author_firstname")

#Visualzing Secondary texts by Year
allSecondary %>% 
  filter(Year>1850) %>%
  group_by(Year) %>%
  mutate(count=n()) %>% 
  ggplot() + 
  geom_line(colour="blue") + 
  aes(x=Year,y=count) + 
  geom_point(size=2,color="blue") + 
  geom_smooth(method="loess") + 
  ggtitle("Number of secondary works published by Year")


# Visualize Primary
allPrimary %>% 
  filter(Year>1850) %>%
  group_by(Year) %>%
  mutate(count=n()) %>% 
  ggplot() + 
  geom_line(colour="red") + 
  aes(x=Year,y=count) + 
  geom_point(size=2,color="red") + 
  geom_smooth(method="loess") + 
  ggtitle("Number of primary works published by Year")




# Save to CSV
write.csv(allSecondary, file=paste('output/allSecondary.csv',sep=""))
write.csv(allPrimary, file=paste('output/allPrimary',sep=""))
write.csv(allBib, file=paste('output/allBib-3-9-17.csv',sep=""))

# Read allBib back in with updated genders
allBibedited <- read_csv(file = "./data/allBib-3-9-17-edited.csv")
allBibedited$X1 <- NULL
allBibedited[is.na(allBibedited)] <- "Anonymous"

# Visualize allBib
plotly = allBib %>%
  filter(Year>1700) %>%
  group_by(Year) %>% 
  mutate(count = n()) %>%
  ggplot() +
  geom_line() +
  aes(x=Year,y=count, color=Type) + 
  geom_point(alpha=.8) + 
  ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20) 

ggplotly(plotly)

# Bar graph by gender
eBar = allBibedited %>%
  group_by(Year, gender, Type) %>% 
  mutate(gendercount = n()) %>% 
  ggplot() +
  geom_bar() +
  aes(x=gender, fill=Type)

ggplotly(eBar)
  
# Year graph faceted by gender  
  eLine = allBibedited %>%
    filter(gender != "Anonymous", Year >= 1800) %>%
    group_by(Year, gender) %>%
    mutate(count = n()) %>%
    ungroup %>% 
    ggplot() +
    geom_point(alpha=.8) + 
    geom_line() +
    aes(x=Year,y=count, color=Type) + 
    ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year by Gender") + 
    facet_wrap(facets = "gender")  
  
ggplotly(eLine)

# Bar graph by year
yearTest = allBibedited %>%
  filter(gender != "Anonymous", Year >= 1800) %>%
  group_by(Year, gender, Type) %>% 
  mutate(gendercount = n()) %>% 
  ggplot() +
  geom_bar() +
  aes(x=Year, fill=gender,text = paste(paste(gender, "authored:"), gendercount)) +
  facet_wrap(facets = "Type")

ggplotly(yearTest)

