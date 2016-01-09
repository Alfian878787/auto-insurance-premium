library(tidyr)
library(plyr)
library(dplyr)
library(pastecs)
library(xlsx)

seguros <- read.csv('seguros.csv')

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
hist(seguros$Pais1, main="Insurance Premiums in Pais 1", cex.main=0.9, xlab=NA)
hist(seguros$Pais2, main="Insurance Premiums in Pais 2", cex.main=0.9, xlab=NA)
hist(seguros$Pais3, main="Insurance Premiums in Pais 3", cex.main=0.9, xlab=NA)
hist(seguros$Pais4, main="Insurance Premiums in Pais 4", cex.main=0.9, xlab=NA)
hist(seguros$Pais5, main="Insurance Premiums in Pais 5", cex.main=0.9, xlab=NA)

# check unique values
length(unique(seguros$Pais1))
length(unique(seguros$Pais2))
length(unique(seguros$Pais3))
length(unique(seguros$Pais4))
length(unique(seguros$Pais5))


###############################
# PARAMETRIC TESTS
###############################

# Shapiro-Wilk test (per sample)
with(df, tapply(premio, pais, shapiro.test))

# Bartlet test
bartlett.test(premio ~ pais, data=df)
qchisq(0.95, df=4)

# one-way ANOVA
oneway.test(premio ~ pais, data=df, var.equal=TRUE)

# one-way ANOVA, same test, different approach
anova(lm(premio ~ pais, data=df))

# one-way ANOVA, same test, another approach
an1 <- aov(premio ~ pais, data=df)
summary(an1)

# Tukey-HSD test
TukeyHSD(aov(premio ~ pais, data=df))