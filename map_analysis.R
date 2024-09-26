install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("xlsx")
library("xlsx")

map_data <- read_xlsx("map_data.xlsx")
print(map_data)

#dummy variables (for data manipulation)
map_data$Spring_Math_Growth_dummy <- ifelse(map_data$Spring_Math_Met_Growth == "Yes", 1, 0)
map_data$Winter_Math_Growth_dummy <- ifelse(map_data$Winter_Math_Met_Growth == "Yes", 1, 0)
map_data$Fall_Math_Growth_dummy <- ifelse(map_data$Fall_Math_Met_Growth == "Yes", 1, 0)

map_data$Spring_Math_Percentile_dummy <- ifelse(map_data$Spring_Math_Percentile >= 50, "Yes", "No")
map_data$Winter_Math_Percentile_dummy <- ifelse(map_data$Winter_Math_Percentile >= 50, "Yes", "No")
map_data$Fall_Math_Percentile_dummy <- ifelse(map_data$Fall_Math_Percentile >= 50, "Yes", "No")

print(str(map_data))
head(map_data, 20)

# Count and calculate percentages (Yes/No)
growth_summary_spring <- summarise(
  group_by(
    filter(map_data, !is.na(Spring_Math_Met_Growth)), 
    Spring_Math_Met_Growth
  ),
  Count = n()
)

growth_summary_winter <- summarise(
  group_by(
    filter(map_data, !is.na(Winter_Math_Met_Growth)), 
    Winter_Math_Met_Growth
  ),
  Count = n()
)

growth_summary_fall <- summarise(
  group_by(
    filter(map_data, !is.na(Fall_Math_Met_Growth)), 
    Fall_Math_Met_Growth
  ),
  Count = n()
)
#calculate percentages
growth_summary_spring$Percentage <- (growth_summary_spring$Count / sum(growth_summary_spring$Count)) * 100
growth_summary_winter$Percentage <- (growth_summary_winter$Count / sum(growth_summary_winter$Count)) * 100
growth_summary_fall$Percentage <- (growth_summary_fall$Count / sum(growth_summary_fall$Count)) * 100

print(growth_summary_spring)
print(growth_summary_winter)
print(growth_summary_fall)


# bar charts
ggplot(growth_summary_spring, aes(x = Spring_Math_Met_Growth, y = Percentage, fill = Spring_Math_Met_Growth)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth Goals in Spring",
       x = "Growth Goal Met", y = "Percentage") +
  theme_minimal()

ggplot(growth_summary_winter, aes(x = Winter_Math_Met_Growth, y = Percentage, fill = Winter_Math_Met_Growth)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth Goals in Winter",
       x = "Growth Goal Met", y = "Percentage") +
  theme_minimal()

ggplot(growth_summary_fall, aes(x = Fall_Math_Met_Growth, y = Percentage, fill = Fall_Math_Met_Growth)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth Goals in Fall",
       x = "Growth Goal Met", y = "Percentage") +
  theme_minimal()

#check for 50th percentile
percentile_summary_spring <- summarise(
  group_by(
    map_data, 
    Spring_Math_Percentile_dummy
  ),
  Count = n()
)

percentile_summary_winter <- summarise(
  group_by(
    map_data, 
    Winter_Math_Percentile_dummy
  ),
  Count = n()
)

percentile_summary_fall <- summarise(
  group_by(
    map_data, 
    Fall_Math_Percentile_dummy
  ),
  Count = n()
)
# Calculate percentages
percentile_summary_spring$Percentage <- (percentile_summary_spring$Count / sum(percentile_summary_spring$Count)) * 100
percentile_summary_winter$Percentage <- (percentile_summary_winter$Count / sum(percentile_summary_winter$Count)) * 100
percentile_summary_fall$Percentage <- (percentile_summary_fall$Count / sum(percentile_summary_fall$Count)) * 100

print(percentile_summary_spring)
print(percentile_summary_winter)
print(percentile_summary_fall)

#bar charts
ggplot(percentile_summary_spring, aes(x = Spring_Math_Percentile_dummy, y = Percentage, fill = Spring_Math_Percentile_dummy)) +
  geom_bar(stat = "identity") +
  labs(title = "Students Meeting by Math Percentile: Spring",
       x = "Math Percentile", y = "Percentage") +
  theme_minimal()

ggplot(percentile_summary_winter, aes(x = Winter_Math_Percentile_dummy, y = Percentage, fill = Winter_Math_Percentile_dummy)) +
  geom_bar(stat = "identity") +
  labs(title = "Students Meeting by Math Percentile: Winter",
       x = "Math Percentile", y = "Percentage") +
  theme_minimal()

ggplot(percentile_summary_fall, aes(x = Fall_Math_Percentile_dummy, y = Percentage, fill = Fall_Math_Percentile_dummy)) +
  geom_bar(stat = "identity") +
  labs(title = "Students Meeting by Math Percentile: Fall",
       x = "Math Percentile", y = "Percentage") +
  theme_minimal()


#Group by Race/Ethnicity (changed Excel column to make this part easier!)
race_grouped <- group_by(map_data, Race_Ethnicity)

#count kids who met growth
race_growth_summary_spring <- summarise(race_grouped,
                            Count = n(),  
                            Growth_Met = sum(Spring_Math_Met_Growth == "Yes"))  

race_growth_summary_winter <- summarise(race_grouped,
                                   Count = n(),  
                                   Growth_Met = sum(Winter_Math_Met_Growth == "Yes"))  

race_growth_summary_fall <- summarise(race_grouped,
                                   Count = n(), 
                                   Growth_Met = sum(Fall_Math_Met_Growth == "Yes")) 

#percentage of students who met growth
race_growth_summary_spring$Percentage <- (race_growth_summary_spring$Growth_Met / race_growth_summary_spring$Count) * 100
race_growth_summary_winter$Percentage <- (race_growth_summary_winter$Growth_Met / race_growth_summary_winter$Count) * 100
race_growth_summary_fall$Percentage <- (race_growth_summary_fall$Growth_Met / race_growth_summary_fall$Count) * 100

print(race_growth_summary_spring)
print(race_growth_summary_winter)
print(race_growth_summary_fall)

#bar charts for met growth
ggplot(race_growth_summary_spring, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth by Race/Ethnicity: Spring",
       x = "Race/Ethnicity", y = "Percentage of Students Meeting Growth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

ggplot(race_growth_summary_winter, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth by Race/Ethnicity: Winter",
       x = "Race/Ethnicity", y = "Percentage of Students Meeting Growth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggplot(race_growth_summary_fall, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Meeting Growth by Race/Ethnicity:Fall",
       x = "Race/Ethnicity", y = "Percentage of Students Meeting Growth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#above 50th percentile by race
race_percentile_summary_spring <- summarise(race_grouped,
                                Count = n(),  # Total number of students in each race/ethnicity group
                                Over_50 = sum(Spring_Math_Percentile > 50))  # Number of students scoring over 50

race_percentile_summary_spring$Percentage <- (race_percentile_summary_spring$Over_50 / race_percentile_summary_spring$Count) * 100

print(race_percentile_summary_spring)

ggplot(race_percentile_summary_spring, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Scoring Over 50 in Spring Math Percentile by Race/Ethnicity",
       x = "Race/Ethnicity", y = "Percentage of Students Scoring Over 50") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

race_percentile_summary_winter <- summarise(race_grouped,
                                            Count = n(),  # Total number of students in each race/ethnicity group
                                            Over_50 = sum(Winter_Math_Percentile > 50))  # Number of students scoring over 50

race_percentile_summary_winter$Percentage <- (race_percentile_summary_winter$Over_50 / race_percentile_summary_winter$Count) * 100

print(race_percentile_summary_winter)

ggplot(race_percentile_summary_winter, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Scoring Over 50 in Winter Math Percentile by Race/Ethnicity",
       x = "Race/Ethnicity", y = "Percentage of Students Scoring Over 50") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

race_percentile_summary_fall <- summarise(race_grouped,
                                            Count = n(),  # Total number of students in each race/ethnicity group
                                            Over_50 = sum(Fall_Math_Percentile > 50))  # Number of students scoring over 50

race_percentile_summary_fall$Percentage <- (race_percentile_summary_fall$Over_50 / race_percentile_summary_fall$Count) * 100

print(race_percentile_summary_fall)

ggplot(race_percentile_summary_fall, aes(x = Race_Ethnicity, y = Percentage, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Students Scoring Over 50 in Fall Math Percentile by Race/Ethnicity",
       x = "Race/Ethnicity", y = "Percentage of Students Scoring Over 50") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


