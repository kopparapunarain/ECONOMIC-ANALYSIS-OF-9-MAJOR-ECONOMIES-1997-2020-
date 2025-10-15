library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(knitr)

#Dataset -
"https://www.kaggle.com/code/algoholiccreations/preliminary-eda-on-economic-dataset-1980-2020/notebook"

#Data:
Data =read_excel("/Users/narainkopparapu/Downloads/Edata.xlsx")

#Filtering the data to post 1997 since some countries have data only post 1997.
Edata <-filter(Data, Year > '1996')
view(Edata)

#Filtering the data by country:
U.S <- filter(Edata, Country == "U.S")
U.K <- filter(Edata, Country == "U.K")
India <- filter(Edata, Country == "India")
Japan <- filter(Edata, Country == "Japan")
Germany <- filter(Edata, Country == "Germany")
France <- filter(Edata, Country == "France")
Spain <- filter(Edata, Country == "Spain")
#All the countries have data post 1997, but some of China & Hong Kong's economic data is post 2004, adding another line of code to omit blank cells
H.K.1 <- filter(Edata, Country == "Hong Kong")
H.K.1[complete.cases(H.K.1),]
H.K <- H.K.1[complete.cases(H.K.1),]
China.1 <- filter(Edata, Country == "China")
China.1[complete.cases(China.1),]
China <- China.1[complete.cases(China.1),]

#Summary of all the country data:
summary(U.S)
summary(U.K)
summary(India)
summary(Japan)
summary(China)
summary(H.K)
summary(Spain)
summary(France)
summary(Germany)

#Filtering Columns for better data manipulation :
Stock_Index <- Edata[, -c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
Country <- Edata[, -c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
Year <- Edata[, -c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
Index_Price <- Edata[, -c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)]
Inflation_Rate <- Edata[, -c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14)]
Oil_prices <- Edata[, -c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14)]
Exchange_Rate <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14)]
Gdp_Growth <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14)]
PerCapita <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)]
UnEmployment <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14)]
Manufacturing <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14)]
Trade_Balance <- Edata[, -c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14)]


#A column with only country names for data manipulation
Inflist  <- filter(Country, Year == 2008)

#Question 1- How is the growth/decline in manufacturing correlated to the growth/decline of the stock market idex by country?

#Dropping blank cells and attached values
Edata%>% 
  drop_na()

#Scatter Plot of Every Country's stock market data
ggplot(data = Edata) +
geom_point(mapping =aes(x = Year, y = (Index_Price))) +
facet_wrap(~ Country, nrow = 2)

#Scatter Plot of Every Country's manufacturing data
ggplot(data = Edata) +
geom_point(mapping =aes(x = Year, y = (Manufacturing), color = Country))

#Correlation between Index Price and Manufacturing
cor(U.S$Index_Price, U.S$Manufacturing)
cor(U.K$Index_Price, U.K$Manufacturing)
cor(India$Index_Price, India$Manufacturing)
cor(China$Index_Price, China$Manufacturing) 
cor(H.K$Index_Price, H.K$Manufacturing) 
cor(Japan$Index_Price, Japan$Manufacturing)
cor(France$Index_Price, France$Manufacturing)
cor(Spain$Index_Price, Spain$Manufacturing)
cor(Germany$Index_Price, Germany$Manufacturing)

#Table for the above data
Data1 <- rbind(cor(U.S$Index_Price,U.S$Manufacturing),cor(U.K$Index_Price,U.K$Manufacturing),cor(India$Index_Price,India$Manufacturing),cor(Japan$Index_Price,Japan$Manufacturing),cor(H.K$Index_Price,H.K$Manufacturing),cor(China$Index_Price,China$Manufacturing),cor(Germany$Index_Price,Germany$Manufacturing),cor(France$Index_Price,France$Manufacturing),cor(Spain$Index_Price,Spain$Manufacturing))
Table.1.1 <- cbind(Inflist,Data1)
colnames(Table.1.1) = c('Country', 'Correlation')

#Question 2- Is a countries GDP growth affected by Inflation Rate and Trade Balance ?
#GDP Growth by country

#Box Plot of Every Country's GDP data
ggplot() +
geom_boxplot(data = China, mapping =aes(x = Country, y = Gdp_Growth, color = "China")) +
geom_boxplot(data = India, mapping =aes(x = Country, y = Gdp_Growth, color = "India")) +
geom_boxplot(data = U.S, mapping =aes(x = Country, y = Gdp_Growth, color = "U.S")) +
geom_boxplot(data = Spain, mapping =aes(x = Country, y = Gdp_Growth, color = "Spain")) +
geom_boxplot(data = U.K, mapping =aes(x = Country, y = Gdp_Growth, color = "U.K")) +
geom_boxplot(data = Germany, mapping =aes(x = Country, y = Gdp_Growth, color = "Germany")) +
geom_boxplot(data = France, mapping =aes(x = Country, y = Gdp_Growth, color = "France")) +
geom_boxplot(data = H.K, mapping =aes(x = Country, y = Gdp_Growth, color = "H.K")) +
geom_boxplot(data = Japan, mapping =aes(x = Country, y = Gdp_Growth, color = "Japan"))

#Scatter Plot of 3 countries Inflation vs GDP growth
ggplot() +
geom_point(data = China, mapping =aes(x = Inflation_Rate, y = Gdp_Growth, color = "China")) +
geom_point(data = U.S, mapping =aes(x = Inflation_Rate, y = Gdp_Growth, color = "U.S")) +
geom_point(data = India, mapping =aes(x = Inflation_Rate, y = Gdp_Growth, color = "India")) 

#Scatter Plot of 3 countries Trade Balance vs GDP growth
ggplot() +
geom_point(data = China, mapping =aes(x = Trade_Balance, y = Gdp_Growth, color = "China")) +
geom_point(data = U.S, mapping =aes(x = Trade_Balance, y = Gdp_Growth, color = "U.S")) +
geom_point(data = India, mapping =aes(x = Trade_Balance, y = Gdp_Growth, color = "India")) 

#Linear Regression for the 3 countries compared
LRGI_US <- lm(Gdp_Growth ~ Inflation_Rate, data = U.S) 
LRGT_US <- lm(Gdp_Growth ~ Trade_Balance, data = U.S) 
LRGI_India <- lm(Gdp_Growth ~ Inflation_Rate, data = India) 
LRGT_India <- lm(Gdp_Growth ~ Trade_Balance, data = India) 
LRGI_China <- lm(Gdp_Growth ~ Inflation_Rate, data = China) 
LRGT_China <- lm(Gdp_Growth ~ Trade_Balance, data = China) 

#Filtering out only coefficient data
LRGI_US$coefficients
LRGT_US$coefficients
LRGI_India$coefficients
LRGT_India$coefficients
LRGI_China$coefficients
LRGT_China$coefficients

#Question 3- Does high PerCapita mean high unemployment?

#Scatter Plot of Every Country's Per Capita vs Unemployment
ggplot(data = Edata) +
geom_point(mapping =aes(x = PerCapita, y = Unemployment, color = Country))

#Line Plot of 3 countries Per Capita vs Unemployment
ggplot() +
geom_line(data = Spain, aes(x = PerCapita, y = Unemployment, color ="Spain")) +
geom_line(data = France, aes(x = PerCapita, y = Unemployment, color ="France")) +
geom_line(data = Germany, aes(x = PerCapita, y = Unemployment, color ="Germany")) 

#Question  4- The 2008 Global recession and the 2020 Covid scare had an adverse effect on economies, which economies were the worst affected :
#A - filtering data by country and inflation rate to compute the greatest differences :
#2008-09 inflation data
Inf_Y2008  <- filter(Inflation_Rate, Year == 2008)
Inf_Y2009 <- filter(Inflation_Rate, Year == 2009)

#Difference between the 2
Inf_Diff1 <- Inf_Y2008 - Inf_Y2009

#Output of Most affected and Least affected
MinInf1 <- which(Inf_Diff1== min(Inf_Diff1))
Inflist[MinInf1, 1]
MaxInf1 <- which(Inf_Diff1== max(Inf_Diff1))
Inflist[MaxInf1, 1]

#2019-20 inflation data
Inf_Y2019  <- filter(Inflation_Rate, Year == 2019)
Inf_Y2020 <- filter(Inflation_Rate, Year == 2020)

#Difference between the 2
Inf_Diff2 <- Inf_Y2019 - Inf_Y2020

#Output of Most affected and Least affected
MinInf2 <- which(Inf_Diff2== min(Inf_Diff2))
Inflist[MinInf2, 1]
MaxInf2 <- which(Inf_Diff2== max(Inf_Diff2))
Inflist[MaxInf2, 1]

#Table data
Data_Frame.1 <- cbind(Inflist,Inf_Diff1,Inf_Diff2)
colnames(Data_Frame.1) = c('Country','2008-2009 Inflation Difference','2019-2020 Inflation Difference')
 
#B- filtering data by country and index price to find which countries stock market was effected the most :
##2008-09 stock market data
Ind_Y2008  <- filter(Index_Price, Year == 2008)
Ind_Y2009 <- filter(Index_Price, Year == 2009)

#Difference between the 2 in %
Ind_Diff1 <- Ind_Y2009 - Ind_Y2008
Ind_PerDiff1 <- Ind_Diff1/Ind_Y2008 * 100

#Output of Most affected and Least affected
MinInd1 <- which(Ind_PerDiff1== min(Ind_PerDiff1))
Inflist[MinInd1, 1]
MaxInd1 <- which(Ind_PerDiff1== max(Ind_PerDiff1))
Inflist[MaxInd1, 1]

#2019-20 stock market data
Ind_Y2019  <- filter(Index_Price, Year == 2019)
Ind_Y2020 <- filter(Index_Price, Year == 2020)

#Difference between the 2 in %
Ind_Diff2 <- Ind_Y2020 - Ind_Y2019
Ind_PerDiff2 <- Ind_Diff2/Ind_Y2019 * 100

#Output of Most affected and Least affected
MinInd2 <- which(Ind_PerDiff2== min(Ind_PerDiff2))
Inflist[MinInd2, 1]
MaxInd2 <- which(Ind_PerDiff2== max(Ind_PerDiff2))
Inflist[MaxInd2, 1]

#Table data
Data_Frame.2 <- cbind(Inflist,Ind_Diff1,Ind_PerDiff1,Ind_Diff2,Ind_PerDiff2) 
colnames(Data_Frame.2) = c('Country','08-09(Index Change)','08-09(% Change)','19-20(Change)','19-20(%Change)') 
 


