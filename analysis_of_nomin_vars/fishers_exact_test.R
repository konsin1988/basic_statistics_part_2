library(data.table)
library(dplyr)
library(rdrr)

v1  <- c(1, NA, 2)
v2  <- c(3, 4, NA)
all(is.na(v1) == is.na(v2))

all(table(mtcars[,c("am", "vs")]) >= 5)

chisq.test(table(mtcars[,c("am", "vs")]))[3]

smart_test <-  function(x){
  x <- table(x)
  if (all(x >= 5)) return(as.numeric(chisq.test(x)[1:3]))
  else return (as.numeric(fisher.test(x)[1]))
}
smart_test(mtcars[,c("am", "vs")])

test_data <- 
  read.csv("https://stepik.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

str(test_data)
colnames(test_data)


result_df <- data.frame(
    lapply(
      test_data, function(x) as.numeric(chisq.test(table(x))[3])))

names(result_df)[result_df == min(result_df)]

ir <- data.table(iris)

temp <- ir[,lapply(.SD, function(x) x > mean(x)), .SDcols = c(1:4)]
temp_result <- apply(temp, 1, function(x) ifelse(sum(x) >= 3, 'Yes', 'No'))
       

get_important_cases <- function(df){
  library(data.table)
  df <- data.table(df)
  temp <- df[,lapply(.SD, function(x) x > mean(x))]
  n <- ncol(temp)/2
  temp_result <- apply(temp, 1, function(x) ifelse(sum(x) > n, 'Yes', 'No'))
  df[,important_cases := factor(temp_result, levels = c('No', 'Yes'))]
  return(data.frame(df))
}
test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))
get_important_cases(test_data)

test_data <- as.data.frame(list(V1 = c(23, 29, 15, 20, 18, 12, 16, 25), 
                                V2 = c(24, 20, 10, 13, 22, 10, 13, 15), 
                                V3 = c(19, 25, 13, 17, 18, 33, 13, 20), 
                                V4 = c(26, 21, 22, 21, 18, 26, 16, 20), 
                                V5 = c(16, 16, 12, 18, 12, 28, 25, 17), 
                                V6 = c(20, 22, 17, 19, 20, 22, 31, 17)))

stat_mode <- function(x){
  t <- table(v)
  as.numeric(names(t[t == max(t)]))
}
v <- c(1, 2, 3, 3, 3, 4, 5)
stat_mode(v)
t <- table(v)
as.numeric(names(t[t == max(t)]))

test_data <- 
  read.csv("https://stepik.org/media/attachments/course/524/test_drugs.csv")
t <- table(test_data)
resid <- chisq.test(t)$stdres
ind <- which(resid == max(resid), arr.ind = T)
c(rownames(resid)[ind[1]], colnames(resid)[ind[2]])

max_resid <- function(x){
  t <- table(x)
  resid <- chisq.test(t)$stdres
  ind <- which(resid == max(resid), arr.ind = T)
  c(rownames(resid)[ind[1]], colnames(resid)[ind[2]])
}

max_resid(test_data)

library(ggplot2)
ggplot(diamonds, aes(x = color, fill = cut))+
  geom_bar(position = 'dodge')
