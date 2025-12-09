
# ----------------------------------------------------------
# RQ: Is there a positive correlation between GDP per capita 
#     and  the  Happiness Score in the  2019 World Happiness 
#     Report?
# ----------------------------------------------------------

# ----------------------------------------------------------
# 1) Load and prepare data
# ----------------------------------------------------------

df <- read.csv("2019.csv", check.names = FALSE)

# Rename the columns names
# Rename the 2nd column "Country or region" of  df to "country"
colnames(df)[2] <- "country"
# Rename the 3rd column "Score" of df to "happiness"
colnames(df)[3] <- "happiness"
# Rename the 4th column "GDP per capita" of df to "gdp"

colnames(df)[4] <- "gdp"

# Removes rows containing null/NA values.
df <- na.omit(df)

df



# ----------------------------------------------------------
# 2) Main Plot = Scatterplot with Regression line
# ----------------------------------------------------------
png("scatterplot.png", width = 1200, height = 900)

# Regression line fitting from a linear model
fit <- lm(happiness ~ gdp, data=df)

# Plot with colors
plot(df$gdp, df$happiness,
     pch = 16,
     col = rgb(0.1, 0.4, 0.9, 0.55),  # semi-transparent blue points
     xlab = "GDP per capita",
     ylab = "Happiness score",
     main = "GDP per capita vs Happiness Score (2019)",
     panel.first = grid(col = "grey85", lty = "dotted"))

# use to draw the regression line
abline(fit, lwd = 2, lty = 2, col = "firebrick")
# red dashed line
lines(lowess(df$gdp, df$happiness), lwd = 2, col = "darkgreen")  # green smooth

legend("topleft",
       legend = c("Countries", "Regression line (lm)", "LOWESS smooth"),
       pch = c(16, NA, NA),
       lty = c(NA, 2, 1),
       lwd = c(NA, 2, 3),
       col = c(rgb(0.1, 0.4, 0.9, 0.55), "firebrick", "darkgreen"),
       bty = "n")
dev.off()



# ----------------------------------------------------------
# 3) HISTOGRAMS (Check normality for choosing test)
# ----------------------------------------------------------

# ----------------------------------------------------------
# Happiness Histogram : Dependent variable (outcome)
# ----------------------------------------------------------
png("hist_happiness.png", width = 1200, height = 900)
h <- hist(df$happiness, breaks = 20, plot = FALSE)
xmax <- max(h$breaks)
ymax <- max(h$counts)

hist(df$happiness,
     breaks = h$breaks,
     xlim = c(2, xmax),
     ylim = c(0, ymax * 1.25), 
     main = "Histogram of Happiness Score (2019)",
     xlab = "Happiness score",
     ylab = "Number of countries",
     col  = "skyblue",
     border = "white")

# Add count labels on top of each bar
text(x = h$mids,
     y = h$counts,
     labels = h$counts,
     pos = 3,        # above the bar
     cex = 0.8,
     col = "black")

dev.off()

# ----------------------------------------------------------
# GDP Histogram : Independent variable (predictor)
# ----------------------------------------------------------


h2 <- hist(dat$gdp, breaks = 20, plot = FALSE)
#use seq to create ticks for the x-axis
ticks <- seq(0, max(h2$breaks), length.out = 5)

png("hist_gdp.png", width = 1200, height = 900)

hist(dat$gdp, breaks = h2$breaks,
     xlim = range(ticks),
     ylim = c(0, max(h2$counts) * 1.15),
     main = "Histogram of GDP per capita",
     xlab = "GDP per capita (index)",
     ylab = "Number of countries",
     col  = "skyblue", border = "white",
     xaxt = "n")

axis(1, at = ticks, labels = round(ticks, 2))

text(x = h2$mids,
     y = h2$counts,
     labels = h2$counts,
     pos = 3,
     cex = 1.2,
     col = "black")

dev.off()


# ------------------------------------------------------------
# 4) Correlation Test: pearson (After checking histograms)
# ------------------------------------------------------------

cor_out <- cor.test(df$gdp, df$happiness, method = "pearson")

r <- unname(cor_out$estimate)
#person r
p <- cor_out$p.value
#p-value
n <- nrow(df)
#number of rows used in test

cat("\n--- Correlation test ---\n")
#print full test results
print(cor_out)

cor_out <- cor.test(df$gdp, df$happiness, method = "pearson")
print(cor_out)

# ------------------------------------------------------------
# 5) Summary statistics
# ------------------------------------------------------------
summary_table <- data.frame(
  n = n,
  happiness_mean = mean(df$happiness),
  happiness_sd   = sd(df$happiness),
  happiness_min  = min(df$happiness),
  happiness_max  = max(df$happiness),
  gdp_mean = mean(df$gdp),
  gdp_sd   = sd(df$gdp),
  gdp_min  = min(df$gdp),
  gdp_max  = max(df$gdp)
)

cat("\n--- Summary table ---\n")
print(summary_table)

# ------------------------------------------------------------
# 6) Contingency table
# ------------------------------------------------------------

# turned both numeric variables into 3 categories
df$gdp_cat <- cut(df$gdp,
                   breaks = quantile(df$gdp, 
                                     probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   include.lowest = TRUE,
                   labels = c("Low GDP", "Mid GDP", "High GDP")
                   )

df$happy_cat <- cut(df$happiness,
                     breaks = quantile(df$happiness, 
                                       probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE,
                     labels = c("Low Happy", "Mid Happy", "High Happy")
                     )

tab <- table(df$gdp_cat, df$happy_cat)
tab

# Row proportions (proportion of happiness categories within each GDP category)
round(prop.table(tab, margin = 1), 3)

