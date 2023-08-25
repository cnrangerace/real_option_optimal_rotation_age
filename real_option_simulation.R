# 03/27/2022
# Rotation Age Simulation

# load necessary pacakges
library(dplyr)
library(data.table)
library(tidyverse)
library(openxlsx)


# set work directory
setwd('/Users/fanzhang/Library/CloudStorage/OneDrive-Personal/00.Real Options') # mac path
getwd()

# read the excel spreadsheet
reser_price_data_80 <- read.xlsx('Reser_price_w_41_yr 072421.xlsx', 1)
reser_price_data_41 <- read.xlsx('Reser_price_w_41_yr 072421.xlsx', 2)
reser_price_data_70 <- read.xlsx('Reser_price_w_70_yr 062022.xlsx', 1)
comparison_data <- read.xlsx('Threeway comparison again072122.xlsx',1)
rolling25_data <- read.xlsx('Rolling 25.xlsx',1)
rolling15_and_40_data <- read.xlsx('Rolling 15 and 40.xlsx', 1)
rolling_data <- read.xlsx('Rolling 17 19 21 and 23 reservationprices.xlsx', 1)
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)


reser_price_data_70
reser_price_data_41
reser_price_data_80
comparison_data
rolling25_data
rolling15_and_40_data
rolling_data
head(partial_put_dt)

# set up a single determination case --------------------------------------

# Random price generator
# Pr_1 <- rnorm(65, 169.19, 65.73) # normal distribution here, may use others
# x <- ts(Pr_1, start = 1)
# reser_price_data_80['Pr'] <- x
# 
# # Save the decision process into a dt
# test.dt <- reser_price_data_80 %>%
#   mutate(Decision = case_when(Pr >= Reser.price ~ 1, 
#                               Pr < Reser.price ~ 0)
#   )
# test.dt
# test.dt[test.dt$Decision == 1, ][1, ] # harvest age = 64

# ------------------------------------------------------------------------

# Build loop to generate the decision process --- American Put based Pm 
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(55, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  reser_price_data_70['Pr'] <- x
  simu.dt <- reser_price_data_70 %>%
    mutate(Decision = case_when(Pr >= Reser.price ~ 1, 
                                Pr < Reser.price ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)[8] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[55, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[2]


# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t        Q.t.          Pm   Put.value Reser.price         LEV          Pr    Decision 
# 49.43680 60065.51979   237.76882    59.16113   264.34353  1588.19335   294.02506     0.99934 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_put_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# ------------------------------------------------------------------------

# Build loop to generate the decision process --- European Put based Pm 
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(55, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  reser_price_data_70['Pr'] <- x
  simu.dt <- reser_price_data_70 %>%
    mutate(Decision = case_when(Pr >= Euro_put_reser_pr ~ 1, 
                                Pr < Euro_put_reser_pr ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[55, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[2]


# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t        Q.t.          Pm   Put.value Reser.price         LEV          Pr    Decision 
# 49.43680 60065.51979   237.76882    59.16113   264.34353  1588.19335   294.02506     0.99934 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_euro_put_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------

# Build loop to generate the decision process --- 10-yr rolling pm
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  comparison_data['Pr'] <- x
  simu.dt <- comparison_data %>%
    mutate(Decision = case_when(Pr >= PM_10yr ~ 1, 
                                Pr < PM_10yr ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[2]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t        Q.t.          Pm   Put.value Reser.price         LEV          Pr    Decision 
# 49.43680 60065.51979   237.76882    59.16113   264.34353  1588.19335   294.02506     0.99934 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_10-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---20-yr rolling pm
dt.full <- list() 
dt.harvest <- list()

rolling_data <- read.xlsx('reservation prices of rolling 20-year put.xlsx', 1)
head(rolling_data)
tail(rolling_data)

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.20 ~ 1, 
                                Pr < Rolling.20 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[2]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_20-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---25-yr rolling pm
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling25_data['Pr'] <- x
  simu.dt <- rolling25_data %>%
    mutate(Decision = case_when(Pr >= Pm_rolling_25 ~ 1, 
                                Pr < Pm_rolling_25 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_25-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---13 -yr rolling pm
dt.full <- list() 
dt.harvest <- list()



rolling_data <- read.xlsx('reservationprices rolling 13.xlsx', 1)
head(rolling_data)
tail(rolling_data)

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.13 ~ 1, 
                                Pr < Rolling.13 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_13-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---15 -yr rolling pm
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling15_and_40_data['Pr'] <- x
  simu.dt <- rolling15_and_40_data %>%
    mutate(Decision = case_when(Pr >= pm_rolling_15 ~ 1, 
                                Pr < pm_rolling_15 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_15-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---17 -yr rolling pm
dt.full <- list() 
dt.harvest <- list()

head(rolling_data)

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.17 ~ 1, 
                                Pr < Rolling.17 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_17-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---19 -yr rolling pm
dt.full <- list() 
dt.harvest <- list()
rolling_data <- read.xlsx('Rolling 17 19 21 and 23 reservationprices.xlsx', 1)


for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.19 ~ 1, 
                                Pr < Rolling.19 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_19-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process ---21 -yr rolling pm
dt.full <- list() 
dt.harvest <- list()
rolling_data <- read.xlsx('Rolling 17 19 21 and 23 reservationprices.xlsx', 1)
head(rolling_data)

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.21 ~ 1, 
                                Pr < Rolling.21 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_21-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process --- 23-yr rolling pm
dt.full <- list() 
dt.harvest <- list()
rolling_data <- read.xlsx('Rolling 17 19 21 and 23 reservationprices.xlsx', 1)


for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling_data['Pr'] <- x
  simu.dt <- rolling_data %>%
    mutate(Decision = case_when(Pr >= Rolling.23 ~ 1, 
                                Pr < Rolling.23 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_23-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------


# Build loop to generate the decision process --- 40-yr rolling pm
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  rolling15_and_40_data['Pr'] <- x
  simu.dt <- rolling15_and_40_data %>%
    mutate(Decision = case_when(Pr >= pm_rolling_40 ~ 1, 
                                Pr < pm_rolling_40 ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


dt.harvest_output


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_40-yr-rolling_pm_with_lev_70yr.xlsx',
            row.names = FALSE)



# --------------------------------------------------------------------------------------

partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 10 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_10_percent_put ~ 1, 
                                Pr < pm_10_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_10_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)
# Harvest Age                QT pm_10_percent_put                Pr          Decision 
# 19.4033        11909.4860          220.3558          258.2063            1.0000 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_10_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------

partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 20 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_20_percent_put ~ 1, 
                                Pr < pm_20_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_20_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)
# Harvest Age                QT pm_20_percent_put                Pr          Decision 
# 23.41856       18730.76676         232.16477         267.75826           1.00000 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_20_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------

partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 30 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_30_percent_put ~ 1, 
                                Pr < pm_30_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_30_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)
# Harvest Age                QT pm_30_percent_put                Pr          Decision 
# 26.85244       24800.10451         238.32529         272.81842           1.00000 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_30_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------

partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 40 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_40_percent_put ~ 1, 
                                Pr < pm_40_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_40_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)
# Harvest Age                QT pm_40_percent_put                Pr          Decision 
# 29.85344       30089.28169         243.47990         276.66383           1.00000 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_40_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 50 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_50_percent_put ~ 1, 
                                Pr < pm_50_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_50_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 55 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_55_percent_put ~ 1, 
                                Pr < pm_55_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_55_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 60 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_60_percent_put ~ 1, 
                                Pr < pm_60_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)

class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# t     Q.t.    Pm_BM Pm_Amput  Pm_euro  PM_10yr Pm_20_yr       Pr Decision
# 26 23280.81 291.5693  389.091 230.4874 259.3947 285.7083 317.3294        1 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_60_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 65 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_65_percent_put ~ 1, 
                                Pr < pm_65_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_65_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# Harvest Age                QT pm_65_percent_put                Pr          Decision 
# 36.93326       41994.71545         252.41381         284.26443           1.00000 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_65_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 70 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_70_percent_put ~ 1, 
                                Pr < pm_70_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_70_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# Harvest Age                QT pm_70_percent_put                Pr          Decision 
# 38.27956       44134.64148         253.82458         285.29340           0.99998 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,'C:/OneDrive/00.Real Options/harvest_decision_output_70_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 75 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_75_percent_put ~ 1, 
                                Pr < pm_75_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_75_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# Harvest Age                QT pm_75_percent_put                Pr          Decision 
# 39.52710       46062.92933         255.68663         286.93561           0.99994 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,
            'C:/OneDrive/00.Real Options/harvest_decision_output_75_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)


# --------------------------------------------------------------------------------------
partial_put_dt <- read.xlsx('partial_put_input.xlsx', 1)
head(partial_put_dt)

# Build loop to generate the decision process --- 80 percent partial put
dt.full <- list() 
dt.harvest <- list()

for(i in 1:50000) {
  Pr_1 <- rnorm(56, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  partial_put_dt['Pr'] <- x
  simu.dt <- partial_put_dt %>%
    mutate(Decision = case_when(Pr >= pm_80_percent_put ~ 1, 
                                Pr < pm_80_percent_put  ~ 0)
    )
  dt.full[[i]] <- simu.dt
  if (colSums(simu.dt)['Decision'] >= 1)
  {
    dt.harvest[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest[[i]] <- simu.dt[56, ]
  }
}

# A snapshot of data
dt.full[1]
dt.harvest[3]

# transfer data list into a data frame for further analysis

dt.harvest_output <- rbindlist(dt.harvest) # rbind list into a data frame
dt.harvest_output <- rename(dt.harvest_output, 'Harvest Age' = t)
dt.harvest_output <- (dt.harvest_output[, c('Harvest Age', 'QT', 'pm_80_percent_put', 'Pr', 'Decision')])
head(dt.harvest_output)


class(dt.harvest_output)
str(dt.harvest_output)
colMeans(dt.harvest_output, na.rm=TRUE, dims = 1)

# Harvest Age                QT pm_80_percent_put                Pr          Decision 
# 40.94026       48221.25889         256.65643         287.44903           0.99982 


head(dt.harvest_output)


# test if there is any case that waits until 80 years
dt.harvest_output[dt.harvest_output$Decision==0]


write.xlsx (dt.harvest_output,
            'C:/OneDrive/00.Real Options/harvest_decision_output_80_percent_partial_pm_with_lev_70yr.xlsx',
            row.names = FALSE)




# --------------------------------------------------------------------------------------
# Build loop to generate the decision process --- Original Pm
dt.full.pm <- list() 
dt.harvest.pm <- list()

simu.dt
for(i in 1:50000) {
  Pr_1 <- rnorm(55, 169.19, 65.73) # normal distribution here, may use others
  x <- ts(Pr_1, start = 1)
  reser_price_data_70['Pr'] <- x
  simu.dt <- reser_price_data_70 %>%
    mutate(Decision = case_when(Pr >= Pm ~ 1, 
                                Pr < Pm ~ 0)
    )
  dt.full.pm[[i]] <- simu.dt
  if (colSums(simu.dt)[8] >= 1)
  {
    dt.harvest.pm[[i]] <- simu.dt[simu.dt$Decision == 1, ][1, ]
  }
  else 
  {
    dt.harvest.pm[[i]] <- simu.dt[55, ]
  }
}


# A snapshot of data
# dt.full.pm[1]
# dt.harvest.pm[[5]]

# transfer data list into a data frame for further analysis
library(data.table)
library(tidyverse)
dt.harvest_output.pm <- rbindlist(dt.harvest.pm) # rbind list into a data frame
dt.harvest_output.pm <- rename(dt.harvest_output.pm, 'Harvest Age' = t)

colMeans(dt.harvest_output.pm, na.rm=TRUE, dims = 1)

#Harvest Age        Q.t.          Pm   Put.value Reser.price         LEV          Pr    Decision 
#38.33462 44063.52137   257.53688    60.80032   306.47845  1946.68181   288.21345     1.00000 

#test if there is any case that waits until 80-year to harvest
dt.harvest_output.pm[dt.harvest_output.pm$decision == 0]

write.xlsx (dt.harvest_output.pm,'C:/OneDrive/00.Real Options/harvest_decision_output_pm_with_lev_70yr.xlsx',
            row.names = FALSE)

#-------------------------------------------------------------------------
# visualize the Rotation Age Distribution

library("ggplot2")
library("ggplot2")

# Plot the histogram and the fitted line
ggplot(dt.harvest_output, aes(`Harvest Age`)) + 
  geom_histogram(binwidth=1,aes(y = stat(density))) +
  geom_density(col = "red") +
  ylab("% Probability of harvest")


# create a bind data for harvest ages from two different methods
combdat <- rbind(data.frame(dt.harvest_output.pm[,1],Method='Original Pm'), 
                 data.frame(dt.harvest_output[,1], Method='Put Option Pm')
                 )

head(combdat)
tail(combdat)

ggplot(combdat, aes(`Harvest.Age`, fill=Method))
  +geom_histogram(aes(y=..density..),color='gray30', 
    alpha=0.2, binwidth=1, position = "identity")
  +geom_density(alpha=.1)
  +ylab("% Probability of harvest")
#  +geom_vline(aes(xintercept=mean(x),size=1))


library(ggpubr)
library(cowplot)

gghistogram(
  combdat, x = "Harvest.Age", 
  add = "mean", rug = TRUE,
  fill = "Method", palette = c("#00AFBB", "#E7B800"),
  add_density=TRUE
)
