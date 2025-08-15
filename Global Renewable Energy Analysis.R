#Install Packages
install.packages("dplyr")
library(dplyr)

#Load Data set
df<-read.csv("C:\\Users\\??????\\Desktop\\python\\complete_renewable_energy_dataset.csv")
View(df)
head(df)
str(df)
summary(df)


# 1.Is there a significant difference in average CO2 emissions between France and India?

france <- na.omit(df[df$Country == "France", "CO2.Emissions"])
india  <- na.omit(df[df$Country == "India", "CO2.Emissions"])
t_result <- t.test(france, india, var.equal = FALSE)
print(t_result)
if (t_result$p.value < 0.05) {
  print("Reject H0: Mean CO2 emissions differ significantly between France and India")
} else {
  print("Fail to Reject H0: No significant difference in CO2 emissions")
}
#Result:A significant difference was observed in average CO2 emissions between France and India 

# 2.Do solar and wind energy sources have different variability in CO2 emissions?

solar <- na.omit(subset(df, Energy.Type == "Solar")$CO2.Emissions)
wind  <- na.omit(subset(df, Energy.Type == "Wind")$CO2.Emissions)
f_result <- var.test(solar, wind)
print(f_result)

if (f_result$p.value < 0.05) {
  print("Reject H0: Variances are significantly different")
} else {
  print("Fail to Reject H0: No significant difference in variances")
}
#Result:Variance in emissions is statistically similar between Solar and Wind technologies.

# 3.Is there a relationship between a country and the type of energy it uses?

table_data <- table(df$Country, df$Energy.Type)
chi_result<-chisq.test(table_data)

if (chi_result$p.value < alpha) {
  print("Reject H0: Country and Energy Type are associated")
} else {
  print("Fail to Reject H0: No significant association between Country and Energy Type")
}
#Result:The choice of energy type does not statistically depend on the country in this data set.

# 4.Is there a statistically significant difference between the mean renewable energy production and the installed capacity across all countries?

install.packages("BSDA")
library(BSDA)

production <- na.omit(df$Production..GWh.)
capacity <- na.omit(df$Installed.Capacity..MW.)
z_result <- z.test(x = production, y = capacity,
                   sigma.x = sd(production),
                   sigma.y = sd(capacity))
print(z_result)

if (z_result$p.value < 0.05) {
  print("Reject H0: There is a significant difference between Production and Installed Capacity")
} else {
  print("Fail to Reject H0: No significant difference between Production and Installed Capacity")
}
#Result:Showed a statistically significant difference between mean renewable energy production and installed capacity across countries.


# 5.Does R&D expenditure vary significantly across different energy types?

anova_result <- aov(R.D.Expenditure ~ Energy.Type, data = df)
summary(anova_result)

pval <- summary(anova_result)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
  print("Significant: R&D expenditure differs across energy types")
} else {
  print("Not significant")
}
#Result:R&D expenditure was found to differ significantly across different renewable energy types.






