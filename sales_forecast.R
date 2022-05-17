# install.packages(c("lubridate", "tidyverse", "dplyr", "forecast"))
library(tidyverse)
library(lubridate)
library(dplyr)
library(forecast)

# data was loaded on Virtual Lab and saved to NYCRealEstate.RData so that I could work on my local machine
# loading data from save data file

load("NYCRealEstate.RData")

# nyc_transaction is joined with building_class and neighborhood 
# column called SaleYear, SaleQ is created
# filtering entries with sale_price>0, gross_square_feet>0, year>=2009, neighborhood=BUSHWICK, type=RESIDENTIAL
df <- nyc_transaction %>%
  left_join(building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "X.BUILDING_CODE_ID")) %>%
  mutate(SaleYear = year(SALE_DATE), SaleQ = quarter(SALE_DATE))%>% 
  filter(SALE_PRICE>0, GROSS_SQUARE_FEET>0, NEIGHBORHOOD == 'BUSHWICK', TYPE == 'RESIDENTIAL', SaleYear>=2009)%>% 
  mutate(t = SaleYear * 4 + SaleQ - 2009*4)%>% 
  select('SALE_DATE', 'SaleYear', 'SaleQ', 't', 'ADDRESS', 'BUILDING_CLASS_FINAL_ROLL', 'YEAR_BUILT', 'GROSS_SQUARE_FEET', 'RESIDENTIAL_UNITS', 'SALE_PRICE')


# question 1: time series
# grouping the data by year and quarter and calculating total sale price in each time period
df_grouped <- df %>%
  group_by(SaleYear, SaleQ)%>%
  summarise(TotalSales = sum(SALE_PRICE))

# converting total sales by period into ts object  
ts_sales <- ts(df_grouped['TotalSales'][[1]], start = 2009, frequency = 4)

# using stl decomposition to examine the seasonilty, trend and error in the time series
stl_decompose <- stl(ts_sales, "periodic")
plot(stl_decompose)

# creating a time series model
ts_model <- ets(y = ts_sales, model='MNA')
summary(ts_model)

# using the model to forecast sales for the next 8 time periods
ts_forecast <- as.data.frame(forecast(ts_model, 8))

# plotting the forecasted data
plot(forecast(ts_model, 8))



# question 2: Regression Forecast
# converting the saleq to factor for regression analysis
df_grouped$SaleQ <- factor(df_grouped$SaleQ)

# creating time period column, t and convertinf it to numeric
df_grouped$t = rownames(df_grouped)
df_grouped$t <- as.numeric(df_grouped$t)

# regression model with time period as predictor
lm_time <- lm(data = df_grouped, formula = TotalSales~t)
summary(lm_time)

# regression model with time period and season as predictor
lm_time_season <- lm(data = df_grouped, formula = TotalSales~t+SaleQ)
summary(lm_time_season)



# question 3: Regression Prediction
# multiple linear regression
lm_multi<-lm(formula=SALE_PRICE~t + YEAR_BUILT + GROSS_SQUARE_FEET + RESIDENTIAL_UNITS + BUILDING_CLASS_FINAL_ROLL, data=df)
summary(lm_multi)

# appending residuals to df
df["residuals"] <- lm_multi$residuals


# boxplots for gross square feet and residential units to spot out outliers
boxplot(df$GROSS_SQUARE_FEET)
boxplot(df$RESIDENTIAL_UNITS)

# redundant variable analysis with all numeric data used in regression
cols <- c('t', 'YEAR_BUILT', 'GROSS_SQUARE_FEET', 'RESIDENTIAL_UNITS')
vars <- select(df, cols)
corr <- cor(vars)
pairs(vars)

# multiple linear regression with Gross Sq Ft
lm_multi1<-lm(formula=SALE_PRICE~t + YEAR_BUILT + RESIDENTIAL_UNITS + BUILDING_CLASS_FINAL_ROLL, data=df)
summary(lm_multi1)

# multiple linear regression without Residential Units
lm_multi2<-lm(formula=SALE_PRICE~t + YEAR_BUILT + GROSS_SQUARE_FEET + BUILDING_CLASS_FINAL_ROLL, data=df)
summary(lm_multi2)

