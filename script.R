
# RQ: Is GDP per capita positively correlated with happiness score? (2019)


# 1) Data preprocessing

df <- read.csv("~/Downloads/2019.csv", check.names = FALSE)

#change column names for understanding
dat <- data.frame(
  country   = df[["Country or region"]],
  happinessScore = df[["Score"]],
  gdp       = df[["GDP per capita"]]
)
#remove null values
dat <- na.omit(dat)
df

# 2) Correlation test (Pearson)
cor_out <- cor.test(dat$gdp, dat$happinessScore, method = "pearson")

r <- unname(cor_out$estimate)
#person r
p <- cor_out$p.value
#p-value
n <- nrow(dat)
#number of rows used in test

cat("\n--- Correlation test ---\n")
#print full test results
print(cor_out)