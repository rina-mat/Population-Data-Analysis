library(dplyr)
library(xlsx)

options(digits = 15)

population_data <- read.csv("WPP2019_TotalPopulationBySex.csv")
urban_population_data <- select(read.csv("Urban Data.csv"), 1:9)



population_data_BD <- population_data %>% filter((Location == "Bangladesh" & Time <= "2021") & Variant == "Medium")
population_data_INDIA <- population_data %>% filter((Location == "India" & Time <= "2021") & Variant == "Medium")
population_data_BD <- population_data_BD %>% mutate(growth_rate_combined = round(((PopTotal - lag(PopTotal))/PopTotal)*100, digits = 3),
                                                    Female_growth_rate = round(((PopFemale - lag(PopFemale))/PopFemale)*100, digits = 3), 
                                                    Male_growth_rate = round(((PopMale - lag(PopMale))/PopMale)*100, digits = 3))

population_data_INDIA <- population_data_INDIA %>% mutate(growth_rate_combined = round(((PopTotal - lag(PopTotal))/PopTotal)*100, digits = 3),
                                                    Female_growth_rate = round(((PopFemale - lag(PopFemale))/PopFemale)*100, digits = 3), 
                                                    Male_growth_rate = round(((PopMale - lag(PopMale))/PopMale)*100, digits = 3))

new_bd_file <- data.frame(Year = population_data_BD$Time, BD.Male.Population = population_data_BD$PopMale,BD.Female.Population = population_data_BD$PopFemale,
                          BD.Total.Population = population_data_BD$PopTotal, BD.Population.Density = population_data_BD$PopDensity, 
                          BD.Population.growth.rate = population_data_BD$growth_rate_combined, BD.male.growth.rate = population_data_BD$Male_growth_rate,
                          BD.female.growth.rate = population_data_BD$Female_growth_rate)
new_india_file <- data.frame(Year = population_data_INDIA$Time, IN.Male.Population = population_data_INDIA$PopMale, IN.Female.Population = population_data_INDIA$PopFemale,
                          IN.Total.Population = population_data_INDIA$PopTotal,IN.Population.Density = population_data_INDIA$PopDensity,
                          IN.Population.growth.rate = population_data_INDIA$growth_rate_combined, IN.male.growth.rate = population_data_INDIA$Male_growth_rate,
                          IN.female.growth.rate = population_data_INDIA$Female_growth_rate)

bd_india_file <- merge(new_bd_file, new_india_file, by="Year")

write.xlsx(new_bd_file, file = "Population Data.xlsx", sheetName = "Bangladesh", append = TRUE)
write.xlsx(new_india_file, file = "Population Data.xlsx", sheetName = "INDIA", append = TRUE)
write.xlsx(bd_india_file, file = "Population Data.xlsx", sheetName = "Combined BD and INDIA", append = TRUE)


## Urban Population Data Manipulation


urban_population_data <- urban_population_data %>% mutate(Urban.Population.Growth.Prcntg.BD = round(((Urban.Population.BD - lag(Urban.Population.BD))/Urban.Population.BD)*100, digits = 3), 
                                                       Urban.Population.Growth.Prcntg.India = round(((Urban.Population.India - lag(Urban.Population.India))/Urban.Population.India)*100, digits = 3))
urban_population_data$avg_annual_prcntg_BD = " "
urban_population_data$avg_annual_prcntg_India = " "

for (i in seq(1, nrow(urban_population_data), 5)){
  if(i+5 >=  nrow(urban_population_data)){
    break()
  }
  urban_population_data$avg_annual_prcntg_BD[i+2]=round (( log(urban_population_data$Urban.Population.Percentage..in.BD[i+5]/urban_population_data$Urban.Population.Percentage..in.BD[i])/5) * 100, 4) 

  urban_population_data$avg_annual_prcntg_India[i+2]=round (( log(urban_population_data$Urban.Population.Percentage..in.India[i+5]/urban_population_data$Urban.Population.Percentage..in.India[i])/5) * 100, 4) 
  
  }
write.xlsx(urban_population_data , file = "Population Data.xlsx", sheetName = "Urban Data of BD and India", append = TRUE)


New_urban_data <- data.frame(Year = c(1990:2070), Actual_Population_BD = c(urban_population_data$Urban.Population.BD[41:72], rep(NA, 49)), 
                             Actual_population_India = c(urban_population_data$Urban.Population.India[41:72], rep(NA, 49)))

New_urban_data$Calculated_population_BD_Logistic_model <- round(142570.7/(1+3.593*exp(-0.055*(New_urban_data$Year-2000))), 3)

New_urban_data$Calculated_population_India_Logistic_model <- round(1207163.893/(1+3.143*exp(-0.037*(New_urban_data$Year-2000))), 3)


write.xlsx(New_urban_data , file = "Population Data.xlsx", sheetName = "Actual data and Calculated Data", append = TRUE)









