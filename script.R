
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


# ---- Summary table-----
summary_table <- data.frame(
  n = n,
  happiness_mean = mean(dat$happinessScore),
  happiness_sd   = sd(dat$happinessScore),
  happiness_min  = min(dat$happinessScore),
  happiness_max  = max(dat$happinessScore),
  gdp_mean = mean(dat$gdp),
  gdp_sd   = sd(dat$gdp),
  gdp_min  = min(dat$gdp),
  gdp_max  = max(dat$gdp)
)

cat("\n--- Summary table ---\n")
print(summary_table)

# ================================================
# MAIN PLOT: Scatterplot with regression line
# ================================================

# Regression line fitting from a linear model
fit <- lm(happinessScore ~ gdp, data=dat)

# Plot with colors
plot(dat$gdp, dat$happinessScore,
     pch = 16,
     col = rgb(0.1, 0.4, 0.9, 0.55),           # semi-transparent blue points
     xlab = "GDP per capita",
     ylab = "Happiness score",
     main = "GDP per capita vs Happiness (2019)",
     panel.first = grid(col = "grey85", lty = "dotted"))

# use to draw the regression line
abline(fit, lwd = 2, lty = 2, col = "firebrick")
# red dashed line
lines(lowess(dat$gdp, dat$happinessScore), lwd = 2, col = "darkgreen")  # green smooth

# Add r, p, n in bottom-right
txt <- paste0("Pearson r = ", round(r, 3),
              "\np = ", format(p, scientific=TRUE, digits=2),
              "\nn = ", n)
usr <- par("usr")
text(usr[2], usr[3], txt, adj = c(1, 0), cex = 0.9, col = "grey20")

legend("topleft",
       legend = c("Countries", "Regression line (lm)", "LOWESS smooth"),
       pch = c(16, NA, NA),
       lty = c(NA, 2, 1),
       lwd = c(NA, 2, 3),
       col = c(rgb(0.1, 0.4, 0.9, 0.55), "firebrick", "darkgreen"),
       bty = "n")


#Biggest outliers from the regression line 
res <- resid(fit)
out3 <- order(abs(res), decreasing = TRUE)[1:3]

label_idx <- unique(c(out3))

# Add labels slightly above/right of points
text(dat$gdp[label_idx], dat$happinessScore[label_idx],
     labels = dat$country[label_idx],
     pos = 4, cex = 0.75, col = "black")
