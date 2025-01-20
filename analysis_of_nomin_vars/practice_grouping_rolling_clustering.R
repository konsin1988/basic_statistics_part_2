library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

df <- fread('../german_credit_augmented.csv')
head(df)
str(df)
df[credit_amount < 2000, .(dur_lt_14 = sum(duration < 14)), by=housing]

reshape(df_agg, v.names = 'amount', 
        idvar = 'sex', 
        timevar = 'housing', 
        direction = 'wide', 
        varying = c('own', 'rent', 'free')
        )

df_agg <- df[credit_amount > 3000,.N, by = .(sex, housing)]
reshape(df_agg, 
        v.names = 'N', 
        idvar = 'housing', 
        timevar = 'sex', 
        direction = 'wide',
        varying = c('male', 'female')
        )

str(df)
df[, .(sum_total = sum(credit_amount)), by = .(sex, age_gt_40 = age > 40)]

reshape(df[, .(total_am = sum(credit_amount, na.rm = T)) , 
           by = .(purpose, housing)][order(housing)], 
        direction = 'wide',
        v.names = 'total_am',
        idvar = 'purpose',
        timevar = 'housing',
        varying = unique(df[order(housing),housing])
)
reshape(df[, .N , 
           by = .(purpose, housing)][order(housing)], 
        direction = 'wide',
        v.names = 'N',
        idvar = 'purpose',
        timevar = 'housing',
        varying = unique(df[order(housing),housing])
)

# mean of selected columns
str(df)
reshape(df[saving_accounts == 'little', 
   lapply(.SD, mean), 
   by = .(sex, housing == 'own'), 
   .SDcols = c('duration', 'credit_amount')],
   v.names = c('duration', 'credit_amount'),
   idvar = 'sex',
   timevar = 'housing',
   direction = 'wide',
   varying = c('duration_own', 'total_own', 
               'duration_not_own', 'total_not_own')
)

df[,.(dur_mean = lapply(.SD, mean), 
      dur_sum = lapply(.SD, sum)), 
   by = .(sex) , 
   .SDcols = c('duration')]

# pivot_table without reshape
df[,.(dur_mean_male = lapply(.SD, function(x) mean(x[sex == 'male'])), 
      dur_mean_female = lapply(.SD, function(x) mean(x[sex == 'female']))), 
    by = housing,
   .SDcols = c('duration')]

# first two date rows in each month
str(df)
df[order(contract_dt),
        .(contract_dt, credit_amount, rank_month = frank(floor_date(contract_dt, 'day'), ties.method="dense")), 
        by = floor_date(contract_dt, 'month')][rank_month %in% c(1, 2), .(contract_dt, credit_amount)]

# percent of credit amount of each month
df[order(contract_dt), 
   .(client_id, 
     contract_dt, 
     credit_amount,
     total = sum(credit_amount),
     perc_month = round(credit_amount * 100 / sum(credit_amount), 3)
     ), 
   by = .(month = floor_date(contract_dt, 'month'))]

# grouping by purpose, sex with pivot table
str(df)
reshape(df[order(purpose),.(duration = round(mean(duration), 2)), 
           by = .(sex, purpose)],
        v.names = 'duration',
        timevar = 'purpose',
        idvar = 'sex',
        direction = 'wide',
        varying = unique(df[order(purpose),purpose])
)

# rollmean (current row and 2 preceding) in 2007-06)
library(zoo)
df[
  order(contract_dt), 
   .(contract_dt, credit_amount, 
    rollmean = round(rollapplyr(credit_amount, 3, mean, partial = T), 2)), 
   by = .(month = floor_date(contract_dt, 'month'))
][
  month > '2007-06-01' & month < '2007-07-01',
  .(contract_dt, month, credit_amount, rollmean)
]

# top 10 credit amount by rank
df[order(-credit_amount), 
 .(credit_rank = frankv(credit_amount, order = -1, ties.method = 'dense'),
      credit_amount, contract_dt)
   ][credit_rank < 11]

# Sum and count of df divided by 20 groups by value of credit_amount
df[order(credit_amount), 
   .(credit_amount, contract_dt, housing, groups_amount = 
       cut(df[order(credit_amount), credit_amount], 
                breaks = 20, include.lowest = T, labels = 1:20,
           ordered_result = T))][ 
         , .(sum_credit = sum(credit_amount), count = .N), by = groups_amount
       ]

