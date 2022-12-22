# read file
housing <- read.csv(
  file="./housing.csv",
  sep = ";",
  header = FALSE,
  col.names = c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
)

# get random subset
set.seed(0)
index <- sample(1:nrow(housing), 480)
housing_subset <- housing[index, ]
summary(housing_subset)

par(mfrow=c(1, 2))
hist(housing_subset$B,
     col="blue",
     border="black",
     xlab="Proportion of blacks",
     ylab="counts",
     main="Histogram of proportion of blacks")
box(which="plot", lty="solid", col="black")

housing_subset$B_z <- ((housing_subset$B-mean(housing_subset$B))/sd(housing_subset$B))

hist(housing_subset$B_z,
     breaks=10,
     col="blue",
     border="black",
     xlim = c(-6, 6),
     xlab="Z-standardized proportion of blacks",
     ylab="counts",
     main="Histogram of z-standardized proportion of blacks")
box(which="plot", lty="solid", col="black")

housing_subset[housing_subset$B_z < -3, ]

nrow(housing_subset[housing_subset$B_z < -3, ])

summary(housing_subset$B_z)