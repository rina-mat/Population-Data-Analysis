library(dplyr)
library(xlsx)


options(digits = 15)



Population_data <- read.csv("WPP2019_TotalPopulationBySex.csv")
urban_population_data_diff_countries <- read.csv("Urban population data of selected countries.csv")
total_gdp_data_diff_countries <- read.csv("Total GDP of selected countries.csv")

Population_data_selected_columns <- select(Population_data, c(2, 4, 5, 9:10))

Population_data_filtered <- Population_data_selected_columns %>% filter(Variant == "Medium" & (Time >= 1961 & Time <= 2020))

Population_data_selected_country <- Population_data_filtered %>% filter(Location == "Bangladesh" | Location == "India" | Location == "Pakistan" | Location == "China" |
               Location == "Sri Lanka"| Location == "Singapore"| Location == "Viet Nam")



merged_data_total_population_and_urban <- merge(Population_data_selected_country, urban_population_data_diff_countries, by.x = c("Location", "Time"), by.y = c("Location", "Time"))

merged_data_total_population_and_urban_gdp <- merge(merged_data_total_population_and_urban, total_gdp_data_diff_countries, by.x = c("Location", "Time"), by.y = c("Location", "Time"))



Total_population_and_urban_gdp <- data.frame(Total.Population.BD = merged_data_total_population_and_urban_gdp$PopTotal[1:60], Total.Population.China = merged_data_total_population_and_urban_gdp$PopTotal[61:120],
         Total.Population.India = merged_data_total_population_and_urban_gdp$PopTotal[121:180], Total.Population.Pakistan = merged_data_total_population_and_urban_gdp$PopTotal[181:240],
         Total.Population.Singapore = merged_data_total_population_and_urban_gdp$PopTotal[241:300], Total.Population.Srilanka = merged_data_total_population_and_urban_gdp$PopTotal[301:360],
         Total.Population.Vietnum = merged_data_total_population_and_urban_gdp$PopTotal[361:420],
         
         Urban.Population.BD = merged_data_total_population_and_urban_gdp$Urban.Population[1:60], Urban.Population.China = merged_data_total_population_and_urban_gdp$Urban.Population[61:120],
         Urban.Population.India = merged_data_total_population_and_urban_gdp$Urban.Population[121:180], Urban.Population.Pakistan = merged_data_total_population_and_urban_gdp$Urban.Population[181:240],
         Urban.Population.Singapore = merged_data_total_population_and_urban_gdp$Urban.Population[241:300], Urban.Population.Srilanka = merged_data_total_population_and_urban_gdp$Urban.Population[301:360],
         Urban.Population.Vietnum = merged_data_total_population_and_urban_gdp$Urban.Population[361:420],
         
         Total.GDP.BD = merged_data_total_population_and_urban_gdp$Total.GDP[1:60], Total.GDP.China = merged_data_total_population_and_urban_gdp$Total.GDP[61:120],
         Total.GDP.India = merged_data_total_population_and_urban_gdp$Total.GDP[121:180], Total.GDP.Pakistan = merged_data_total_population_and_urban_gdp$Total.GDP[181:240],
         Total.GDP.Singapore = merged_data_total_population_and_urban_gdp$Total.GDP[241:300], Total.GDP.Srilanka = merged_data_total_population_and_urban_gdp$Total.GDP[301:360],
         Total.GDP.Vietnum = merged_data_total_population_and_urban_gdp$Total.GDP[361:420])

write.xlsx(merged_data_total_population_and_urban_gdp, file = "Total population, Urban Population and GDP.xlsx")

write.xlsx(Total_population_and_urban_gdp, file = "Total population, Urban Population and GDP.xlsx", sheetName = "Total Population, urban population and GDP_diff_countries", append = TRUE)
