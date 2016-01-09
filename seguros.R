library(tidyr)
library(plyr)
library(dplyr)
library(pastecs)
library(xlsx)
library(moments)

seguros <- read.csv('seguros.csv')

# reshape data
df <- gather(seguros, pais, premio, Pais1:Pais5)

###############################
# EXPLORATORY ANALYSIS 
###############################

# data object
str(seguros)

#summary statistics
df_stats <- round(stat.desc(seguros),1)
df_stats
write.xlsx(df_stats, "statistics.xlsx")

# skewness and kurtosis
with(df, tapply(premio, pais, skewness))
with(df, tapply(premio, pais, kurtosis))

# boxplots
plot(df$premio ~ df$pais, main ="Insurance Premiums per Country", ylab=NA, xlab=NA, cex.main=0.9)

# density plot for the aggregated samples --> not used
hist(df$premio, 
     main="Density plot for Insurance Premia", 
     xlab="Premias", 
     border="blue", 
     col="red",
     xlim=c(0,1000),
     las=1, 
     breaks=20,
     prob = TRUE)
lines(density(df$premio)) #Get a density curve to go along with your histogram

# histograms
op <- par(mar = c(1, 2, 3, 1), mfrow = c(5, 1))
hist(seguros$Pais1, main="Insurance Premiums in Pais 1", cex.main=0.9, xlab=NA)
hist(seguros$Pais2, main="Insurance Premiums in Pais 2", cex.main=0.9, xlab=NA)
hist(seguros$Pais3, main="Insurance Premiums in Pais 3", cex.main=0.9, xlab=NA)
hist(seguros$Pais4, main="Insurance Premiums in Pais 4", cex.main=0.9, xlab=NA)
hist(seguros$Pais5, main="Insurance Premiums in Pais 5", cex.main=0.9, xlab=NA)

# qqplots
op <- par(mar = c(1, 2, 3, 1), mfrow = c(3, 2))
qqnorm(seguros$Pais1, main="Pais1"); qqline(seguros$Pais1)
qqnorm(seguros$Pais2, main="Pais2"); qqline(seguros$Pais2)
qqnorm(seguros$Pais3, main="Pais3"); qqline(seguros$Pais3)
qqnorm(seguros$Pais4, main="Pais4"); qqline(seguros$Pais4)
qqnorm(seguros$Pais5, main="Pais5"); qqline(seguros$Pais5)

# check unique values
length(unique(seguros$Pais1))
length(unique(seguros$Pais2))
length(unique(seguros$Pais3))
length(unique(seguros$Pais4))
length(unique(seguros$Pais5))



###############################
# PARAMETRIC TESTS
###############################

# Shapiro-Wilk test (per sample) --> NON-PARAMETRIC
with(df, tapply(premio, pais, shapiro.test))

# Bartlet test
bartlett.test(premio ~ pais, data=df)

# one-way ANOVA
oneway.test(premio ~ pais, data=df, var.equal=TRUE)

# one-way ANOVA, same test, different approach
anova(lm(premio ~ pais, data=df))

# one-way ANOVA, same test, another approach
an1 <- aov(premio ~ pais, data=df)
summary(an1)

# Tukey-HSD test
TukeyHSD(aov(premio ~ pais, data=df))

# Tukey-HSD test; plot confidence levels
par(mar=c(5,6.1,4.1,2.1))
plot(TukeyHSD(an1), las=1)

# Tukey-HSD test, 99%
t <- TukeyHSD(aov(premio ~ pais, data=df), conf.level = 0.99)
plot(t, las=1)
