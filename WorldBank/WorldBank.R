data = read.csv("world-development-indicators/wdi-csv-zip-57-mb-/WDIData.csv")
countries = read.csv("world-development-indicators/wdi-csv-zip-57-mb-/WDICountry.csv")
country_series = read.csv("world-development-indicators/wdi-csv-zip-57-mb-/WDICountry-Series.csv")


colnames(data)[5:62] = as.character(1960:2017)

