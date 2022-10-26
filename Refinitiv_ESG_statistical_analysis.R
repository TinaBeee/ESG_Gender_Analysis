install.packages("tidyverse")
install.packages("Hmisc")

library(tidyverse)
library(Hmisc)

#load csv with the final ESG data
esg_data = read.csv("final_ESG_score_finnhub_data.csv")

#create a new column to sort the Share of Women column into 
#equal-sized buckets based on occurences using cut2 from the Hmisc library
esg_data$women_cats <- factor(Hmisc::cut2(esg_data$Share.of.Women, g=8))

#create boxplots for the different category ESG scores and % of women
plt <- ggplot(esg_data, aes(x=ESG.Score, y=women_cats))
plt + geom_boxplot(fill='#ffa600', color='#a33b9c') + xlab("ESG Score") + 
  ylab("% Share of female executives") + ggtitle("Plot of Total ESG Score by % of Women")

plt <- ggplot(esg_data, aes(x=Environment, y=women_cats))
plt + geom_boxplot(fill='#49a34f', color='#ffa600') + xlab("Environment Score") + 
  ylab("% Share of female executives") + ggtitle("Plot of Environment ESG Score by % of Women")

plt <- ggplot(esg_data, aes(x=Social, y=women_cats))
plt + geom_boxplot(fill='#5670a3', color='#ffa600') + xlab("Social Score") + 
  ylab("% Share of female executives") + ggtitle("Plot of Social ESG Score by % of Women")

plt <- ggplot(esg_data, aes(x=Governance, y=women_cats))
plt + geom_boxplot(fill='#71d9d2', color='#ffa600') + xlab("Governance Score") + 
  ylab("% Share of female executives") + ggtitle("Plot of Governance ESG Score by % of Women")

#summarizing linear model fit for Total ESG Score & % of Women
summary(lm(Share.of.Women ~ ESG.Score,esg_data))

#creating a correlation matrix for the different metrics
cor_matrix <- as.matrix(esg_data[,c("ESG.Score","Environment","Social","Governance",
                                    "Share.of.Women","Employees","Emissions","Resource.Use",
                                    "Innovation","Human.Rights","Product.Responsibility",
                                    "Workforce","Community","Management","Shareholders","CSR.Strategy")])
cor(cor_matrix)

#summarizing multi-linear model to determine impact on Total ESG Score by different metrics
summary(lm(formula = ESG.Score ~ Environment + Share.of.Women + Employees + Social + Governance, data = esg_data))
