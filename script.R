
# RQ: Is GDP per capita positively correlated with happiness score? (2019)


# 1) Data preprocessing

df <- read.csv("~/Downloads/2019.csv", check.names = FALSE)

dat <- data.frame(
  country   = df[["Country or region"]],
  happinessScore = df[["Score"]],
  gdp       = df[["GDP per capita"]]
)
#remove null values
dat <- na.omit(dat)
df
