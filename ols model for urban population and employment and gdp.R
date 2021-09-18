library(ggplot2)
library(xlsx)
library(dplyr)


data_1 <- read.csv("urban and gdp data for OLS.csv")

olsData1 <- lm(formula = data_1$Urban.Population.in.Thousand. ~ data_1$Total.Population..in.Thousand. + data_1$People.Employed.in.Agriculture + data_1$People.Employed.in.Industry 
   + data_1$People.Employed.in.Services + data_1$GDP..Current.US....In.10.million., data = data_1)

olsData2 <- lm(formula = data_1$Urban.Population.in.Thousand. ~ data_1$People.Employed.in.Agriculture + data_1$People.Employed.in.Industry 
               + data_1$People.Employed.in.Services, data = data_1)

olsData3 <-  lm(formula = data_1$Urban.Population.in.Thousand. ~ data_1$People.Employed.in.Agriculture + data_1$People.Employed.in.Industry 
                + data_1$People.Employed.in.Services + data_1$GDP.Per.Capita.Current.US.., data = data_1)



olsData4 <-  lm(formula = data_1$GDP..Current.US....In.10.million. ~ data_1$Urban.Population.in.Thousand. + data_1$People.Employed.in.Agriculture + data_1$People.Employed.in.Industry 
                + data_1$People.Employed.in.Services, data = data_1)

summary(olsData1)
summary(olsData2)
summary(olsData3)
summary(olsData4)


modified_data1 <- data.frame(Time = rep(data_1$Year, 3), Urban.Population = rep(data_1$Urban.Population.in.Thousand., 3), 
                            Employed.people = c(data_1$People.Employed.in.Agriculture, data_1$People.Employed.in.Industry, data_1$People.Employed.in.Services),
Employment_status = rep(c("Agriculture", "Industry", "Services"), each = 30), gdp_total = rep(data_1$GDP..Current.US....In.10.million., 3), 
gdp_per_capita = rep(data_1$GDP.Per.Capita.Current.US.., 3))


ggplot(modified_data1, aes(x = Urban.Population, y=Employed.people, shape = Employment_status, colour = Employment_status, fill = Employment_status))+
  geom_smooth(method = "lm")+ facet_grid(Employment_status~., scales = "fixed")+
  geom_point(size = 3)+ 
  theme_bw()+
  xlab("Urban Population of Bangladesh (In thousand)")+
  ylab("Number of Employed People in different sectors")+ ggtitle("COrrelation between urban population and Employment sectors") 