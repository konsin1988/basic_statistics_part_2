m1 <- replicate(1000, table(sample(x=seq(1,6), size=120, replace=T)))
dim(m1) <- c(1000, 6)
df <- data.frame(m1)
names(df) <- paste('X', seq(1:6), sep='')

to_adding <- function(O) (O - 20)^2/20
df$chi2 <- rowSums(sapply(df, to_adding))
hist(df$chi2, freq=F, xlab='Chi squared', ylab='Density')
lines(density(df$chi2), col='red', lwd=2)
