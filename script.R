
# ----------------------------------------------------------
# RQ: Is there a positive correlation between GDP per capita 
#     and  the  Happiness Score in the  2019 World Happiness 
#     Report?
# ----------------------------------------------------------

# ----------------------------------------------------------
# 1) Load and prepare data
# ----------------------------------------------------------

df <- read.csv("2019.csv", check.names = FALSE)

# Change column names
dat <- data.frame(
  country   = df[["Country or region"]],
  happiness = df[["Score"]],
  gdp       = df[["GDP per capita"]]
)
#remove all null values
dat <- na.omit(dat)

dat

# ----------------------------------------------------------
# 2) HISTOGRAMS (Check normality for choosing test)
# ----------------------------------------------------------

par(mfrow = c(1, 2))  # two plots side-by-side

h <- hist(dat$gdp, breaks = 20, plot = FALSE)
ticks <- seq(0, max(h$breaks), length.out = 5)

hist(dat$gdp, breaks = h$breaks,
     xlim = range(ticks),
     ylim = c(0, max(h$counts) * 1.15),
     main = "Histogram of GDP per capita",
     xlab = "GDP per capita (index)",
     ylab = "Number of countries",
     col  = "skyblue", border = "white",
     xaxt = "n")

axis(1, at = ticks, labels = round(ticks, 2))

# Add count labels on top of each bar
text(x = h$mids,
     y = h$counts,
     labels = h$counts,
     pos = 3,          # above the bar
     cex = 0.8,
     col = "black")



h2   <- hist(dat$happiness, breaks = 20, plot = FALSE)
xmax <- max(h2$breaks)
ymax <- max(h2$counts)

hist(dat$happiness, breaks = h2$breaks,
     xlim = c(2, xmax),
     ylim = c(0, ymax * 1.25),
     main = "Histogram of Happiness score ",
     xlab = "Happiness score",
     ylab = "Number of countries",
     col  = "skyblue",
     border = "white")

# Add count labels on top of each bar
text(x = h2$mids,
     y = h2$counts,
     labels = h2$counts,
     pos = 3,        # above the bar
     cex = 0.8,
     col = "black")

par(mfrow = c(1, 1))  # reset

# ----------------------------------------------------------
# 3) Scatterplot + Regression line
# ----------------------------------------------------------

# Regression line fitting from a linear model
fit <- lm(happiness ~ gdp, data=dat)

# Plot with colors
plot(dat$gdp, dat$happiness,
     pch = 16,
     col = rgb(0.1, 0.4, 0.9, 0.55),  # semi-transparent blue points
     xlab = "GDP per capita",
     ylab = "Happiness score",
     main = "GDP per capita vs Happiness Score (2019)",
     panel.first = grid(col = "grey85", lty = "dotted"))

# use to draw the regression line
abline(fit, lwd = 2, lty = 2, col = "firebrick")
# red dashed line
lines(lowess(dat$gdp, dat$happiness), lwd = 2, col = "darkgreen")  # green smooth

legend("topleft",
       legend = c("Countries", "Regression line (lm)", "LOWESS smooth"),
       pch = c(16, NA, NA),
       lty = c(NA, 2, 1),
       lwd = c(NA, 2, 3),
       col = c(rgb(0.1, 0.4, 0.9, 0.55), "firebrick", "darkgreen"),
       bty = "n")

# ------------------------------------------------------------
# 4) Correlation Test: pearson (After checking histograms)
# ------------------------------------------------------------

cor_out <- cor.test(dat$gdp, dat$happiness, method = "pearson")

r <- unname(cor_out$estimate)
#person r
p <- cor_out$p.value
#p-value
n <- nrow(dat)
#number of rows used in test

cat("\n--- Correlation test ---\n")
#print full test results
print(cor_out)

cor_out <- cor.test(dat$gdp, dat$happiness, method = "pearson")
print(cor_out)

# ------------------------------------------------------------
# 5) Summary statistics
# ------------------------------------------------------------
summary_table <- data.frame(
  n = n,
  happiness_mean = mean(dat$happiness),
  happiness_sd   = sd(dat$happiness),
  happiness_min  = min(dat$happiness),
  happiness_max  = max(dat$happiness),
  gdp_mean = mean(dat$gdp),
  gdp_sd   = sd(dat$gdp),
  gdp_min  = min(dat$gdp),
  gdp_max  = max(dat$gdp)
)

cat("\n--- Summary table ---\n")
print(summary_table)

# ------------------------------------------------------------
# 6) Contingency table
# ------------------------------------------------------------

# turned both numeric variables into 3 categories
dat$gdp_cat <- cut(dat$gdp,
                   breaks = quantile(dat$gdp, 
                                     probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   include.lowest = TRUE,
                   labels = c("Low GDP", "Mid GDP", "High GDP")
                   )

dat$happy_cat <- cut(dat$happiness,
                     breaks = quantile(dat$happiness, 
                                       probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = c("Low Happy", "Mid Happy", "High Happy")
                     )

tab <- table(dat$gdp_cat, dat$happy_cat)
tab

# Row proportions (proportion of happiness categories within each GDP category)
round(prop.table(tab, margin = 1), 3)

