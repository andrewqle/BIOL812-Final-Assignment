library(ggplot2)
library(tidyverse)
library(missMDA)
library (plot3D)
library(dplyr)
library(lubridate)
source("http://bit.ly/theme_pub")
theme_set(theme_pub())

getwd()
setwd("C:/Users/ColauttiLab/Documents/Data")
setwd("..")
CovidData<-read.csv("Data/CovidData.csv", header=T)

#Clean up data- use only covid positive rows, with no repeats in UniqID

df1<-data.frame(CovidData[(CovidData$COVID=="Positive") & (!duplicated(CovidData$UniqID)),])

head(df1)

#Plot Age v viral Load (Age bin = 10 years)
var.labels<-c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100+")

df2<-data.frame(df1$Age, df1$CTValue, df1$Sex) %>%
  na.omit(df2) %>%
  mutate(CustomAgeBin = cut(df1.Age, breaks= c(0,10,20,30,40,50,60,70,80,90,100,110), right= TRUE, labels=var.labels))

head(df2)
nrow(df2)

##Discrete Bar Plot
g1<-ggplot(df2, aes(x=CustomAgeBin, y=df1.CTValue, colour="pink", fill="white")) + geom_bar(stat="identity", position=position_dodge())+
  ggtitle("Plot of Viral Load by Age")+xlab("Age")+ylab("CTValue")

g1+theme(plot.title = element_text(size=16, vjust=2))

##Discrete Scatter Plot
g1<-ggplot(df2, aes(x=CustomAgeBin, y=df1.CTValue, colour="pink", fill="white")) + geom_point(size=0.75)+ geom_smooth(method=lm) + 
  ggtitle("Plot of Viral Load by Age")+xlab("Age")+ylab("CTValue")

g1+theme(plot.title = element_text(size=16, vjust=2))

##Continuous Scatter Plot
g1<-ggplot(df2, aes(x=df1.Age, y=df1.CTValue)) + geom_point(size=0.5)+ 
  geom_smooth(method=lm) + 
  ggtitle("Plot of Viral Load by Age")+xlab("Age")+ylab("CTValue")+
  theme(plot.title = element_text(size=16, vjust=2))
g1

##check correlation
corData<-data.frame(df1$Age,df1$CTValue)
cor(corData, use="complete.obs")

corLModel<-lm(df1$CTValue ~ df1$Age)
summary(corLModel)
anova(corLModel)

#season v viral load
##group by season
df3<-data.frame(df1$CollectDate, df1$Sex, df1$CTValue, df1$Age, df1$Age.bin) %>%
  na.omit(df3)

w21 <- as.Date(seq(dmy("21/12/2020"),(dmy("19/03/2021")),"days"))
w20 <- as.Date(seq(dmy("21/12/2019"),(dmy("19/03/2020")),"days"))

sp21 <- as.Date(seq((dmy("20/03/2021")),(dmy("20/06/2021")),"days"))
sp20 <- as.Date(seq((dmy("20/03/2020")),(dmy("20/06/2020")),"days"))

su21 <- as.Date(seq((dmy("21/06/2021")),(dmy("20/09/2021")),"days"))
su20 <- as.Date(seq((dmy("21/06/2020")),(dmy("20/09/2020")),"days"))

f21 <- as.Date(seq((dmy("21/09/2021")),(dmy("20/12/2021")),"days"))
f20 <- as.Date(seq((dmy("21/09/2020")),(dmy("20/12/2020")),"days"))

chooseSeason<-function(df){

df<- df %>% 
  mutate(Season=ifelse ((dmy(df$df1.CollectDate) %in% c(w21,w20)), "Winter",
                    ifelse ((dmy(df$df1.CollectDate) %in% c(sp21,sp20)), "Spring",
                      ifelse ((dmy(df$df1.CollectDate) %in% c(su21,su20)),"Summer",
                        ifelse ((dmy(df$df1.CollectDate) %in% c(f21,f20)), "Fall", "NA")))))

head(unique(df$Season))
}

chooseSeason(df3)

##group by year
df3 <- df3 %>%
  mutate(Year = year(as.Date(dmy(df3$df1.CollectDate)))) %>%
  group_by(Year)

##plots
###plot by season
g2 <-ggplot(df3, aes(Season,df1.CTValue))+ geom_boxplot()

g3 <- g2+labs(title="Plot of Viral Load by Season", x= "Season", y="CTValue") +
  theme(plot.title = element_text(size=16, vjust=2))

###plot by season and year
levels(df3$Year) <- c(2020,2021)
levels(df3$Year)

g4 <- g3 + facet_wrap(~df3$Year)
g4

##plot by season and age
levels(df3$df1.Age.bin) <- c("<19","19-64","65+")
g5 <- g3 + facet_wrap(~df3$df1.Age.bin) # + facet_wrap(~df3$Year)
g5

##plot by season and sex for 2020 and 2021
###remove "U" rows

df3_2020<-filter(df3, (Year == 2020) & (df1.Sex != "U"))
df3_2021<-filter(df3, Year == 2021& (df1.Sex != "U"))

unique(df3_2020$df1.Sex)

###set levels and add season
levels(df3_2020$df1.Sex) <- c("F","M")
chooseSeason(df3_2020)

levels(df3_2021$df1.Sex) <- c("F","M")
chooseSeason(df3_2021)
###plot
g1_2020 <- ggplot(df3_2020, aes(df1.Sex,df1.CTValue)) + geom_boxplot() + 
  facet_wrap(~df3_2020$Season)+
  labs(title="Plot of Viral Load by Sex - 2020", x= "Sex", y="CTValue") +
  theme(plot.title = element_text(size=16, vjust=2))
g1_2020

g1_2021 <- ggplot(df3_2021, aes(df1.Sex,df1.CTValue)) + geom_boxplot() + 
  facet_wrap(~df3_2021$Season)+
  labs(title="Plot of Viral Load by Sex - 2021", x= "Sex", y="CTValue") +
  theme(plot.title = element_text(size=16, vjust=2))
g1_2021


#Significance Test for season v viral load
df3<-data.frame(df1$Season, df1$CTValue) %>%
  na.omit(df3)

##Create appropriate data frame- uneven size of rows
SPRING <- filter(df3, Season == 'Spring') 
WINTER <- subset(df3, Season == 'Winter')
FALL <- subset(df3, Season == 'Fall')

df3_sub = list(x=SPRING, y=WINTER, z=FALL)
attributes(df3_sub) = list(names = names(df3_sub),
                           row.names = 1:max(length(SPRING), length(WINTER), length(FALL)), class = 'data.frame')

head(df3_sub)
ggplot(df3_sub, aes(x=SPRING, y=df3_sub$x.df1.CTValue))+geom_point(size=0.5)

##ANOVA test
SvCTV<-lm(df3$df1.CTValue ~ df3$Season)
summary(SvCTV)

modifySvCTV<-data.frame(Response = df3$df1.CTValue,
                        PredGroup = df3$Season) %>%
  mutate(df1.SeasonSpring=recode(df3$Season, "Fall"=0, "Spring"=1, "Winter"=0, "Summer"=0),
         df1.SeasonSummer=recode(df3$Season, "Fall"=0, "Spring"=0, "Winter"=0, "Summer"=1 ),
         df1.SeasonWinter=recode(df3$Season, "Fall"=0, "Spring"=0, "Winter"=1, "Summer"=0 ))
head(modifySvCTV)

recodeSvCTV <- lm(Response ~ df1.SeasonSpring + df1.SeasonWinter, data=modifySvCTV )
summary(SvCTV)

Fall<-modifySvCTV %>%
  filter(PredGroup == "Fall") %>%
  summarize(mean(Response))
Winter<-modifySvCTV %>%
  filter(PredGroup == "Winter") %>%
  summarize(mean(Response))
Spring<-modifySvCTV %>%
  filter(PredGroup == "Spring") %>%
  summarize(mean(Response))
Summer<-modifySvCTV %>%
  filter(PredGroup == "Summer") %>%
  summarize(mean(Response))

anova(SvCTV)

#season v number of cases
df4<-data.frame(df1$Age, df1$CTValue, df1$Sex, df1$COVID, df1$CollectDate) %>%
  na.omit(df7)

df4 <- df4 %>%
  mutate(Season = ifelse ((dmy(df4$df1.CollectDate) %in% c(w21,w20)), "Winter",
                    ifelse ((dmy(df4$df1.CollectDate) %in% c(sp21,sp20)), "Spring",
                      ifelse ((dmy(df4$df1.CollectDate) %in% c(su21,su20)),"Summer",
                        ifelse ((dmy(df4$df1.CollectDate) %in% c(f21,f20)), "Fall", "NA")))))

d5<- data.frame(table(df4$df1.COVID, df4$Season))
ggplot(d5, aes(x=Var2, y=Freq, color="red")) + geom_boxplot() +
  labs(title="Number of Positive Cases by Season", x= "Season", y="Number of Positive Cases") +
  theme(plot.title = element_text(size=16, vjust=2))

#3D Plot-this does not work, I was just trying something lol//I have to redo the matrix

#df3<- data.frame (df2$df1.Sex, df2$CustomAgeBin, df2$df1.CTValue)


#zaxis<- data.matrix(df3)
#zaxis

#threed1 <-hist3D (x= nrow(zaxis), y=1:ncol(zaxis), z=zaxis,
        #bty="g", phi=20, thetha= -60,
        #main="Factors affecting Viral Load",
        #col="blue", border="white", shade=0.8,
        #ticktype="detailed", space = 0.20, d=2)


#head(zaxis)



  