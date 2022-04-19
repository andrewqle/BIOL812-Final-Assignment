#Bio812 Dataset 
library(ggplot2)
library(readr)
library(dplyr)
library(ggpubr)

Shedding_output <- read_csv("Desktop/Shedding_output.csv")
View(Shedding_output)

colnames(Shedding_output)

# Remove repeat Unique ID numbers using a new df
Shedding_output %>%
  group_by(UniqID)
  #slice_head(n = 1) %>%
  count(n())
Shedding_output %>% 
  group_by(UniqID) %>% 
  filter(COVID == "Positive")

#Fixing errors in dataset for COVID results
unique(Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="positive",replacement = "Positive",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="HIDE",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="Pending",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="for re-extraction",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern=";Informed by Sarah Buie at Ross Memorial Hospital that the sample does not belong to this patient, therefore the previously reported results are not valid.",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="PSQ",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="COVIDE",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="NRP",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="Indeterminate",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern=";Referred to accession H77568 collected May 14,2020 which was reported as COVID19 virus DETECTED by real time PCR.",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="NOTNPS",replacement = "NA",Shedding_output$`COVID`)
Shedding_output$`COVID` <- gsub(pattern="pending",replacement = "NA",Shedding_output$`COVID`)

#Fixing errors in dataset for Sex
Shedding_output$`Sex` <- gsub(pattern="U",replacement = "N/A",Shedding_output$`Sex`)

na.omit(Shedding_output$Sex)
na.omit(Shedding_output$Age.bin)

df1 <- na.omit(Shedding_output)

# Mutate for Year column
df3 <- df1 %>%
  mutate(Year = as.Date(day, format="%Y-%m-%d")) %>%
  group_by(Year) %>% # group by the day column
  summarise(sum(COVID=="Positive"))
colnames(df3)<- c("date", "cases")

df3<- df1 %>%
  mutate(year = format(CollectDate, format="%Y"))

View(df3)

#Graphs for Age and CT values - Standard histogram to see the data 
ggplot(df1, aes(x=`COVID`, fill = `Age.bin`, na.rm = TRUE)) +
  geom_histogram(stat = 'count') +
  theme_classic() +
  facet_wrap(~Sex)

#Scatterplot option of Sex and Age 
#Opening the graphic functions
options(scipen=999)
theme_set(theme_bw())
data("midwest", package = "ggplot2")

#Creating the scatterplot figure for Sex and Age?
#Option - error for discrete value on continuous scale
gg <- ggplot(df1, aes(x=Age.bin, y=COVID)) +
  geom_point(aes(col=COVID, size=Age)) +
  geom_smooth(method = "loess", se=F) +
  xlim(c(1, 3)) +
  ylim(c(0, 1000)) +
  labs(subtitle ="COVID and Sex/Age" ,
       y= "Count",
       x="Age",
       title = "SARS",
       caption = "COVID")
plot(gg)
       
#Boxplot option for Sex and Age
#Graphic function
theme_set(theme_classic())

#All positive samples in df1 with repeat unique ID's removed.
#Using ggplot boxplot stratify based on Age bin and CT values

theme_set(theme_bw())

g <- ggplot(df3, aes(Age.bin, CTValue))
g + geom_boxplot() + 
  facet_wrap(~year) +
  geom_dotplot(binaxis='y', 
               stackdir='centerwhole',
               stackratio = 0.25,
               dotsize = .25, 
               fill="Blue") +
  theme(axis.text.x = element_text(angle=0, vjust=1)) + 
  labs(title="SARS-CoV-2 Viral Load and Age", 
       subtitle="E-gene cycle threshold values",
       caption="Source: KHSC",
       x="Age Bins",
       y="SARS-CoV-2 Ct E-gene Viral Load") +

AOVModel <- aov(CTValue ~ Age.bin, data = df3)
  
#Graphs for Sex and CT values
ggplot(df1, aes(x= `COVID`, fill = `CTValue`, na.rm = TRUE)) +
  geom_histogram(stat = "count") +
  theme_classic()

#df1 con't (unique IDs removed) 
#Stratify based on Sex and CT Values
g <- ggplot(df3, aes(Sex, CTValue))
g + geom_boxplot() + 
  facet_wrap(~year) +
  geom_dotplot(binaxis='y', 
               stackdir='centerwhole',
               stackratio = 0.25,
               dotsize = .25, 
               fill="Blue") +
  theme(axis.text.x = element_text(angle=0, vjust=1)) + 
  labs(title="SARS-CoV-2 Viral Load and Sex", 
       subtitle="E-gene cycle threshold values",
       caption="Source: KHSC",
       x="Sex",
       y="SARS-CoV-2 Ct E-gene Viral Load")
wilcox.test(CTValue ~ Sex, data = df3)

summary(df1)
View(df1)
summary(df3)
View(df3)
