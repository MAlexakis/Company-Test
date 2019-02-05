### Ubisoft Barcelona Mobile Test ###

pacman::p_load(readr,dplyr,tidyr,highcharter,treemap,viridisLite,magrittr,gridExtra,ggplot2)

setwd("~/Dropbox/Ubisoft Test")
BabyNames<-read.csv("Baby_Names__Beginning_2007.csv",sep = ",")
NameOrigins<-read.csv("Names_Origins.csv",sep = ";")[ ,1:2]
# Merging the 2 Datasets
BabyNamesOrigin <- merge(BabyNames,NameOrigins,by="First.Name")

unique(BabyNames$County)
unique(BabyNames$First.Name)
summary(BabyNames)
str(BabyNames)
which.max(BabyNames$Count)
BabyNames[13690,]

BabyNames$Year<-as.factor(BabyNames$Year)


#Which county has the highest number of Kids over the years
CountyMap <- BabyNames %>% 
  group_by(County) %>% 
  summarise(n = sum(Count), unique = length(unique(First.Name))) %>% 
  arrange(-n, -unique) %>% 
  glimpse()
hchart(CountyMap, "treemap", hcaes(x = County, value = n, color = unique))

#Number of Distincts names per county
GrCountyName <- BabyNames %>%
  group_by(County,First.Name) %>%
  summarise(Sum = sum(Count)) %>%
  arrange(-Sum) %>%
  glimpse()
GrCountyNameMap <- GrCountyName %>%
  group_by(County) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  glimpse()
# #Check is the numbers are right
# KINGS <- BabyNames[which(BabyNames$County=="KINGS"),]
# KINGS<- KINGS %>% 
#   group_by(First.Name) %>%
#   summarise(n = n()) %>% 
#   arrange(-n) %>% 
#   glimpse()
#hchart(GrCountyNameMap, "scatter", hcaes(x = County, y = n, group = County))
hctreemap(treemap(GrCountyNameMap, index = "County",
              vSize = "n", vColor = "n",
              type = "value", palette = viridis(6)))

# Genter Exploration per County
GenterMap <- BabyNames %>% 
  count(County, Sex) %>% 
  glimpse()
colnames(GenterMap)[3] <- "Number_Of_Kids"
hchart(GenterMap, "column", hcaes(x = County, y = Number_Of_Kids, group = Sex))

# Number of kids over the years
YearBorns <- BabyNames %>% 
  group_by(Year) %>%
  summarise(Number_Of_Kids=sum(Count)) %>%
  arrange(-Number_Of_Kids) %>%
  glimpse()
hchart(YearBorns, "column", hcaes(x = Year, y = Number_Of_Kids, group = Year))

# Number of Kids per name (2nd question)
KidsPerName <- BabyNames %>%
  group_by(First.Name) %>%
  summarise(Number_Of_Kids=sum(Count)) %>%
  arrange(-Number_Of_Kids) %>%
  glimpse()
which.max(KidsPerName$Number_Of_Kids)
NameOrigins[which(NameOrigins$First.Name=="MICHAEL"),]
### √       First.Name Origin
### 1106    MICHAEL    Dutch

# Names per year
NamesPerYear <- BabyNames %>%
  count(Year,First.Name) %>%
  glimpse()
NamesPerYearSum <- NamesPerYear %>%
  count(Year) %>%
  glimpse()

### Clustering
WideBabyNO <- GrCountyName %>% spread(County,Sum,fill = 0)
rownames(WideBabyNO)<- WideBabyNO %>% .$First.Name  #set rownames
WideBabyNO %<>% select(-First.Name) # remove name var.

set.seed(101)
ClKmean <- kmeans(WideBabyNO,6)
table(ClKmean$cluster)
names(ClKmean$cluster[ClKmean$cluster==3])

# # Group of names with similar distribution
# Group3<-names(ClKmean$cluster[ClKmean$cluster==3])
# 
# Group3Set <- BabyNamesOrigin %>%
#   filter(First.Name %in% Group3)
# 
# Set3Grouping <- Group3Set %>%
#   group_by(County,First.Name) %>%
#   summarise(Sum = sum(Count)) %>%
#   arrange(-Sum) %>%
#   glimpse()
# WideSet3 <- Set3Grouping %>% spread(County,Sum,fill = 0)
# rownames(WideSet3) <- WideSet3 %>% .$First.Name  #set rownames
# WideSet3 %<>% select(-First.Name) # remove name var.

# WideSet3[,which(!colSums(WideSet3)%in%0)]



