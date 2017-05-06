allBibGender <- allBib %>% gather(Year, gender, -Year)
allBibGender <- allBib %>% group_by(Year, gender, Type) %>% 
  mutate(gendercount = n())


allBibGender <- allBibGender %>% spread(gender, gendercount)
allBibGender <- allBibGender %>% group_by(Year) %>% slice(1)
allBibGender <- allBibGender[c("female", "male")]
allBibGender <- allBibGender %>% as.data.frame()
rownames(allBibGender) <- allBibGender$Year


# Limit columns in allBib

skinnyBib <- allBib[c(-7,-9)]
names(skinnyBib)[names(skinnyBib) == 'fullCit'] <- 'Full Citation'
unique(skinnyBib$Source)

skinnyBib$Source[skinnyBib$Source == "SimsBib"] <- "Sims - LJ in the 20th Century"

class(skinnyBib$Source)


skinnyBib <- skinnyBib %>% mutate(Year = as.numeric(Year))


timelineBib <- timelineBib %>% ungroup()
timelineBib$Count <- NA

timelineBib %>%
  filter(Year>1850) %>%
  group_by(Year) %>%
  mutate(Count=n())


timelineBib %>%
  filter(Year>1700) %>%
  group_by(Year) %>% 
  mutate(Count = n()) %>%
  ggplot() +
  geom_line() +
  aes(x=Year,y=Count, color=Type) + 
  geom_point(alpha=.8) + 
  ggtitle("Number of Primary and Critical Works of Literary Journalism Published by Year") + 
  theme_bw(base_size = 20)


write.csv(allBib, file=paste('data/allBib.csv',sep=""))
write.csv(timelineBib, file=paste('data/timelineBib.csv',sep=""))
write.csv(skinnyBib, file=paste('data/skinnyBib.csv',sep=""))
