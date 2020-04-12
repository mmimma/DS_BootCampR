library("readr")
library("dplyr")
library("forcats")
library("DT")
library("stringr")
library("ggplot2")
library("tidyr")
library("tidyverse") 
library("formatR")
library(RColorBrewer)
library(rmarkdown)

# import1
olympic_base <- read_csv("athlete_events.csv")

# scan
dim(olympic_base)
head(olympic_base,3)
str(olympic_base)
summary(olympic_base)
# dplyr function
glimpse(olympic_base)

# missing
sapply(olympic_base, function(x) sum(is.na (x)))

# recode
olympic_base$Medal[is.na(olympic_base$Medal)]<-"No Medal"

# unique
unlist(lapply(olympic_base, function(x) length(unique(x))))

# No Teams per NOC
olympic_base %>%
  
  group_by(NOC) %>%
  
  summarise(No_Teams=n_distinct(Team)) %>%
  
  arrange(desc(No_Teams)) %>%
  
  DT::datatable()

# import2
noc_base <- read_csv("noc_regions.csv")
glimpse(noc_base)

# merge
olympic_new <- left_join(olympic_base, noc_base,
                         by=c("NOC" = "NOC"))

# new variable1
olympic_new <- mutate(olympic_new, Country=NOC)


# Number of NOC per region (more than one)
olympic_new %>%
  
  group_by(region) %>%
  
  summarise(No_NOCs=n_distinct(NOC)) %>%
  
  filter(No_NOCs>1)%>%
  
  arrange(desc(No_NOCs))%>%
  
  DT::datatable()

# How many missing values we have and what NOC is for that 
olympic_new %>% 
  
  filter(is.na(region)) %>% 
  
  group_by(NOC) %>% 
  
  summarise(Cases=n(),No_Teams=n_distinct(Team)) %>% 
  
  arrange(Cases)%>%
  
  DT::datatable()

# additional info 
lapply(olympic_new[olympic_new$NOC=="UNK",c("Team","notes")],function(x)unique(x))
lapply(olympic_new[olympic_new$NOC=="TUV", c("Team","notes")],function(x)unique(x))
lapply(olympic_new[olympic_new$NOC=="ROT", c("Team","notes")],function(x)unique(x))
lapply(olympic_new[olympic_new$NOC=="SGP", c("Team","notes")],function(x)unique(x))

#change missing names for NOCs in Country
#forcats package, function fct_recode()
olympic_new<-olympic_new%>%
  mutate(Country=fct_recode(Country,
                            "Unknown"="UNK",
                            "Tuvalu"="TUV",
                            "Refugee Olympic Athletes"="ROT",
                            "Singapore"=SGP))

#find NOCs for other with more than 1
olympic_new %>%
  
  filter(region %in% c("Germany","Czech Republic", "Malaysia", "Russia", "Serbia", "Yemen", "Australia", "Canada", "China", "Greece", "Syria", "Trinidad", "Vietnam", "Zimbabwe"))%>%
  
  group_by(region,NOC)%>%
  
  summarise()

# change that Nocs
olympic_new<-olympic_new%>%
  
  mutate(Country=fct_recode(Country,
                            "Australasia"="ANZ",
                            "Newfoundland"="NFL",
                            "Hong Kong"="HKG",
                            "Bohemia"="BOH",
                            "Czechoslovakia"="TCH",
                            "West Germany"="FRG",
                            "East Germany"="GDR",
                            "Saar"="SAA",
                            "Crete"="CRT",
                            "Malaya"="MAL",
                            "North Borneo"="NBO",
                            "Unified Team"="EUN",
                            "Soviet Union"="URS",
                            "Serbia and Montenegro"="SCG",
                            "Yugoslavia"="YUG",
                            "United Arab Republic"="UAR",
                            "Trinidad and Tobago"="TTO",
                            "West Indian Federation"="WIF",
                            "South Vietnam"="VNM",
                            "North Yemen"="YAR",
                            "South Yemen"="YMD",
                            "Rhodesia"="RHO"))

#length of strings
olympic_new<-olympic_new %>%
  
  mutate(No_char=nchar(as.character(Country)))

# change all other
olympic_new$Country<-ifelse(olympic_new$No_char==3,paste(olympic_new$region),paste(olympic_new$Country))

# remove variable
olympic<-olympic_new[,-c(17,19)]

#change name of variable Country to TeamNOc
colnames(olympic)[17]<-"TeamNOC"



#a) Find variables that end with letter t and start with letter S using `select()` function.
  
colnames(select(olympic, starts_with("S") & ends_with("t")))

#b) Create new variable - Body Mass Index using `mutate()` function.
  
  ```{r new variable}
olympic<-olympic%>%
  
  mutate(BMI=Weight/(Height/100)**2)

#and round to 1 decimal place
olympic[,'BMI']=round(olympic[,'BMI'],1)
head(olympic$BMI,5)

#a) Find from data using 'filter()' function:*
  
#a1) only Serbian teams and save it as `olympicSR` df
olympicSR<-filter(olympic, TeamNOC=="Serbia")
head(olympicSR, 5)


#a2) only Serbian teams from 2000 onward and save it as olympicSR21c
olympicSR21c<- filter(olympicSR, Year>=2000)
head(olympicSR21c, 5)

#a3) athletes whose weight is bigger than 100kg and height is over 2m
olympicSR21c<- filter(olympicSR, Year>=2000)
head(olympicSR21c, 5)

#b) Arrange Serbian athletes in `olympicSR21c` df by Height in ascending and descending order.*
  
#Ascending order:
olympicSR21c_a<-arrange(olympicSR21c, Height)
head(olympicSR21c_a,5)


#Descending order:
olympicSR21c_d<-arrange(olympicSR21c, desc(Height))
head(olympicSR21c_d,5)

#c) Using `olympicSR` df find:
  

#c1: the youngest athlete
athletesSR_y<-arrange(olympicSR, Age)
head(athletesSR_y,5)

unique(olympicSR[olympicSR$Age==min(olympicSR$Age), c("Name","Age")])

#c2: the heaviest athlete*
athletesSR_h<-arrange(olympicSR, desc(Weight))
head(athletesSR_h,5)

unique(olympicSR[olympicSR$Weight==max(olympicSR$Weight), c("Name","Weight")])


# d) Use 'summarise()' to print out a summary  
  
#d1: max*
olympicSR%>%
  
  summarise(max_Age=max(Age), max_BMI=max(BMI))

#d2: mean*
  
summarise(olympicSR,mean_Age=mean(Age), mean_BMI=mean(BMI))


# No_Medal1 for Serbia 

olympicSR21c%>%
  
  mutate(Medal=factor(Medal,levels=c("Gold","Silver","Bronze","No Medal")))%>%
  
  filter(Medal!="No Medal")%>%
  
  group_by(Medal)%>%
  
  summarise(Number=n())%>%
  
  DT::datatable()


# Number of medals graph1
olympicSR21c%>% 
  
  mutate(Medal=factor(Medal,levels=c("Gold","Silver","Bronze","No Medal")))%>%
  
  filter(Medal!="No Medal")%>%
  
  group_by(Medal)%>%
  
  summarise(Number=n())%>%
  
  ggplot(aes(x = Medal, y = Number, fill = Medal)) +
  
  geom_bar(stat="identity", color = "black") +
  
  geom_text(aes(label=Number), vjust=1.5, size=4)+
  
  labs (title = "Medals for Serbian team after 2000", 
        caption = "Data from: kaggle - 120 years of Olympic history", 
        x = "Medal", y = "Number_Medals") +
  
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())+
  
  theme(legend.position="none")+
  
  scale_fill_brewer(palette="Dark2")


# Sports with medals 
olympicSR21c %>%
  
  filter(Medal!="No Medal")%>%
  
  group_by(Sport)%>%
  
  summarise(Number_Medals=n())%>%
  
  ggplot(aes(x = reorder(Sport,Number_Medals), y = Number_Medals, fill = Sport )) +
  
  geom_bar(stat="identity", color = "black") +
  
  geom_text(aes(label=Number_Medals), vjust=0.5, hjust=-0.5, size=3)+
  
  coord_flip() +
  
  labs (title = "Sports with medals", 
        x = "Sport", y = "Medals") +
  
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())+
  
  theme(legend.position="none")+
  
  scale_fill_brewer(palette="Paired")


# Number of medals on Summer and Winter 
olympicSR21c%>% 
  
  filter(Medal!="No Medal")%>%
  
  group_by(Season,Medal)%>%
  
  summarise(Number=n())%>%
  
  DT::datatable()

#Sex ratio
olympicSR21c%>%
  
  group_by(Sex)%>%
  
  summarise(Number=n(), ptc=round(Number/NROW(olympicSR21c$Sex)*100))%>%
  
  ggplot(aes(x="", y=ptc, fill=Sex)) +
  
  geom_bar(stat="identity", width=1, color="black") +
  
  coord_polar("y", start=0) +
  
  theme_void()+
  
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5))+
  
  labs(title="Gender ratio")+
  
  geom_text(aes(label = paste(ptc,"%")), position = position_stack(vjust = 0.5),color="black",size=4) +
  
  scale_fill_brewer(palette="Dark2")  

#Medals per Gender
olympicSR21c%>% 
  
  mutate(Medal=factor(Medal,levels=c("Gold","Silver","Bronze","No Medal")))%>%
  
  filter(Medal!="No Medal")%>%
  
  group_by(Medal,Sex)%>%
  
  summarise(Number=n())%>%
  
  ggplot(aes(x = Medal, y = Number, fill = Sex)) +
  
  geom_bar(stat="identity", position = "dodge", color = "black") + 
  
  geom_text(aes(label=Number), position=position_dodge(width=0.9),vjust=-0.25,size=4)+
  
  labs (title = "Medals for Serbian team per Gender", 
        x = "Medal", y = "frequency") +
  
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())+
  
  theme(legend.position="right")+
  
  scale_fill_brewer(palette="Dark2")

# Distribution of Age  
olympicSR21c%>% 
  
  ggplot(aes(x=Sex,y=Age,fill=Sex))+
  
  geom_boxplot(outlier.colour ="hotpink")+
  
  geom_jitter(position = position_jitter(width = 0.1, height = 0),alpha=.2) +
  
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_blank())+
  
  labs(title="Age Distributions",x="Gender", y="Age")+
  
  scale_fill_brewer(palette="Dark2")+
  
  theme(legend.position="none")


# World


#a) Find and visualise data about the number of medals per each team?
  
#a1: Table* No_Medal2
olympic%>% 
  
  filter(Medal!="No Medal") %>% 
  
  group_by(TeamNOC) %>% 
  
  summarise(Number_Medals=n()) %>% 
  
  arrange(desc(Number_Medals))%>%
  
  DT::datatable()

#a2: Graph No_Medal graph2
olympic %>% 
  
  filter(Medal!="No Medal")%>%
  
  group_by(TeamNOC) %>% 
  
  summarise(Number_Medals=n()) %>%
  
  top_n(5,Number_Medals)%>%
  
  ggplot(aes(x = reorder(TeamNOC,Number_Medals), y = Number_Medals, fill=TeamNOC)) +
  
  geom_bar(stat="identity", color = "black") + 
  
  coord_flip()+
  
  theme_classic()+
  
  labs (title = "Medals per Team - Top 5", 
        caption = "Data from: kaggle - 120 years of Olympic history", 
        x = "Top 5 teams", y = "Number_Medals")+
  
  theme(legend.position="none")+
  
  scale_fill_brewer(palette="Dark2")


# Distribution of medals-Top 5
olympic %>% 
  
  mutate(Medal=factor(Medal,levels=c("Gold","Silver","Bronze","No Medal")))%>%
  
  filter(Medal!="No Medal" & TeamNOC %in% c("USA","Soviet Union","Germany","UK","France")) %>%
  
  group_by(TeamNOC,Medal) %>% 
  
  summarise(Number_Medals=n())%>%
  
  ggplot(aes(x = reorder(TeamNOC,-Number_Medals), y = Number_Medals, fill=Medal)) +
  
  geom_bar(stat="identity", color = "black") + 
  
  theme_classic()+
  
  labs (title = "Medals per Team - Top 5", 
        caption = "Data from: kaggle - 120 years of Olympic history", 
        x = "Top 5 teams", y = "Number_Medals")+
  
  theme(legend.position="right")+
  
  scale_fill_brewer(palette="Dark2")
