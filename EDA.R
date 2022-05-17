# install.packages(c("lubridate", "tidyverse", "dplyr"))

# loading packages
# library(odbc)
# library(DBI)
library(tidyverse)
library(lubridate)
library(dplyr)

# data was loaded on Virtual Lab and saved to NYCRealEstate.RData so that I could work on my local machine

# establishing connection to SQL server
# con <- dbConnect(odbc(),
#                 Driver = 'SQL Server',
#                 Server = 'met-sql19.bu.edu',
#                 Database = 'NYC Real Estate',
#                 Port = 1433)


# reading dataframes from SQL server
# borough <- dbReadTable(con,"BOROUGH")
# building_class <- dbReadTable(con,"BUILDING_CLASS")
# neighborhood <- dbReadTable(con,"NEIGHBORHOOD")
# nyc_future <- dbReadTable(con,"NYC_FUTURE")
# nyc_historic <- dbReadTable(con,"NYC_HISTORICAL")
# nyc_transaction <- dbReadTable(con,"NYC_TRANSACTION_DATA")


# loading data from save data file
load("NYCRealEstate.RData")


# question 1
# 1. nyc_historic is joined with building_class and neighborhood 
# 2. rows are filtered out to include only residential data from Bushwick
# 3. Entries are grouped by Sale Year and summarised by summing up sale price and gross sq ft
# 4. Price per sq ft = Total Sales/ Total sq ft
df <- nyc_historic %>%
  left_join(building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "X.BUILDING_CODE_ID")) %>%
  left_join(neighborhood) %>%
  select(SALE_ID, SALE_DATE, GROSS_SQUARE_FEET, SALE_PRICE, NEIGHBORHOOD_NAME, TYPE)%>%
  filter(NEIGHBORHOOD_NAME == 'BUSHWICK', TYPE == 'RESIDENTIAL') %>%
  group_by(SaleYear = year(SALE_DATE)) %>%
  summarise(TotalSales = sum(SALE_PRICE), TotalSqFt = sum(GROSS_SQUARE_FEET))%>%
  mutate(PricePerSqFt = TotalSales / TotalSqFt)

         

# question 3: filtering rows where sale price>0, gross sq ft>0

NeighborhoodYearlyResidentialSales <-function(neighbor){
  #' returns a dataframe with the average price per sq ft for the specified neighborhood
  
df <- nyc_historic %>%
  left_join(building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "X.BUILDING_CODE_ID")) %>%
  left_join(neighborhood, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>%
  select(SALE_ID, SALE_DATE, GROSS_SQUARE_FEET, SALE_PRICE, NEIGHBORHOOD_NAME, TYPE)%>%
  filter(NEIGHBORHOOD_NAME == neighbor, TYPE == 'RESIDENTIAL', SALE_PRICE>0, GROSS_SQUARE_FEET>0) %>%
  group_by(SaleYear = year(SALE_DATE)) %>%
  summarise(TotalSales = sum(SALE_PRICE), TotalSqFt = sum(GROSS_SQUARE_FEET))%>%
  mutate(PricePerSqFt = TotalSales / TotalSqFt, Neighborhood = neighbor)

return(df)
}

# question 4
bushwick <- NeighborhoodYearlyResidentialSales('BUSHWICK')

# question 5: nearby neighborhoods
ridgewood <- NeighborhoodYearlyResidentialSales('RIDGEWOOD')
bedfordStuyvesant <- NeighborhoodYearlyResidentialSales('BEDFORD STUYVESANT')

# question 6: comparing yearly prices between neighborhoods
# combining 3 dataframes together for plotting purposes
plot_df <- rbind(bushwick, ridgewood, bedfordStuyvesant)
ggplot()+geom_line(data = plot_df, aes(x=SaleYear, y=PricePerSqFt, color=Neighborhood), size=1) +
  labs(title = "Yearly change in avg. price per sq. ft. in 3 neighborhoods", 
       y = 'Avg. Price per sq. ft. (in $)',
       x = 'Sale Year')

  
