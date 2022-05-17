# install.packages(c("lubridate", "tidyverse", "dplyr"))
library(tidyverse)
library(lubridate)
library(dplyr)

# data was loaded on Virtual Lab and saved to NYCRealEstate.RData so that I could work on my local machine

# loading data from save data file
load("NYCRealEstate.RData")

# nyc_historic is joined with building_class and neighborhood 
# column called SaleYear is created
# filtering entries with sale_price>0, gross_square_feet>0, year>=2009
all_neighborhoods_2009 <- nyc_historic %>%
  left_join(building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "X.BUILDING_CODE_ID")) %>%
  left_join(neighborhood) %>%
  select(SALE_ID, SALE_DATE, GROSS_SQUARE_FEET, SALE_PRICE, NEIGHBORHOOD_NAME, TYPE)%>%
  mutate(SaleYear = year(SALE_DATE))%>% 
  filter(SALE_PRICE>0, GROSS_SQUARE_FEET>0, SaleYear>=2009)

# filtering out the Bushwick data and Bushwick residential data
bushwick <- filter(all_neighborhoods_2009, NEIGHBORHOOD_NAME == 'BUSHWICK') 
bushwick_residential <- filter(all_neighborhoods_2009, NEIGHBORHOOD_NAME == 'BUSHWICK', TYPE == 'RESIDENTIAL')

# question 1
units_sold <- summarise(bushwick, n())
sprintf("Total number of units sold in Bushwick since 2009: %s", units_sold)

# question 2
avg_sale_price <- round(mean(bushwick_residential$SALE_PRICE), 2)
sprintf("Mean sale price for residential properties in Bushwick since 2009: %s", avg_sale_price)

avg_gross_sq_ft <- round(mean(bushwick_residential$GROSS_SQUARE_FEET), 2)
sprintf("Mean gross square feet for residential properties in Bushwick since 2009: %s", avg_gross_sq_ft)

# question 3
print('5 number summary for Gross Sq Ft and Sale Price')
summary(bushwick_residential[c("GROSS_SQUARE_FEET", "SALE_PRICE")])

# question 4
print ('Proportion of units sold of residential, commercial, mixed, to all properties')
round(prop.table(table(bushwick$TYPE)), 3)

# question 5
sd_sale_price <- round(sd(bushwick_residential$SALE_PRICE), 2)
sprintf("Standard deviation of sale prices for residential properties in Bushwick since 2009: %s", sd_sale_price)

# question 6
corel <- round(cor(bushwick_residential$SALE_PRICE, bushwick_residential$GROSS_SQUARE_FEET), 3)
sprintf("Correlation between sale price and gross square feet for residential properties in Bushwick since 2009: %s", corel)

# question 7
# summarising all the neighborhoods using KPIs (median sale price, no. of residential sales, price per sq ft, Std. dev of sale price)
all_neighborhood_kpi <- filter(all_neighborhoods_2009, TYPE == 'RESIDENTIAL')%>% 
  group_by(NEIGHBORHOOD_NAME)%>% 
  summarise(median_sale_price = median(SALE_PRICE), n_sales = n(), std = sd(SALE_PRICE), price_per_sq_ft = sum(SALE_PRICE)/sum(GROSS_SQUARE_FEET))

# removing rows with na, there are 2 neighborhoods that have 1 sale, so Std. dev is NA
all_neighborhood_kpi <- na.omit(all_neighborhood_kpi)

# standardizing the kpis for all neighborhoods
neighborhood_kpi_scaled = scale(all_neighborhood_kpi[c(-1)])%>% 
  as.data.frame()

# plotting the kpis for all neighborhoods
ggplot(neighborhood_kpi_scaled) + geom_point(mapping = aes(x=median_sale_price, y=n_sales, size=price_per_sq_ft))

# for result reproducibility
set.seed(2021)

# creating 4 clusters using k means
k_mean_result <- kmeans(neighborhood_kpi_scaled, centers = 4)

# appending the k means result to the unscaled data with neighborhood names
all_neighborhood_kpi <- cbind(all_neighborhood_kpi, k_mean_result$cluster)
ggplot(all_neighborhood_kpi) + geom_point(mapping = aes(x=median_sale_price, y=n_sales, size=price_per_sq_ft, color=k_mean_result$cluster)) 

#question 8
# for t-test, chosen neighborhood is baychester 
baychester_residential <- filter(all_neighborhoods_2009, NEIGHBORHOOD_NAME == 'BAYCHESTER', TYPE == 'RESIDENTIAL')
t.test(x=bushwick_residential$SALE_PRICE, y=baychester_residential$SALE_PRICE, alternative = 't', conf.level = 0.95)
t.test(x=bushwick_residential$SALE_PRICE, y=baychester_residential$SALE_PRICE, alternative = 'g', conf.level = 0.95)



