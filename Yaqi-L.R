library(readxl)

file_path <-"C:/Users/Hp/Downloads/data (1).xlsx"

# Read all sheets in the Excel file
sheets <- excel_sheets(file_path)  # get sheet names
data <- lapply(sheets, function(sheet) read_excel(file_path, sheet))

# Alternatively, you can read specific sheets by name
return_data <- read_excel(file_path, sheet = "Return data")
demography_data <- read_excel(file_path, sheet = "Demography data")
sheet_1 <- read_excel(file_path, sheet = "Sheet1")

mean_defensive <- mean(return_data$Defensive)
sd_defensive <- sd(return_data$Defensive)

mean_growth <- mean(return_data$Growth)
sd_growth <- sd(return_data$Growth)
print(paste("The Mean of the investment option Defensive is:", format(mean_defensive, digits = 4)))
print(paste("The Standard Deviation of the investment option Defensive is: ", format(sd_defensive, digits = 4)))
print(paste("The Mean of the investment option Growth  is: ", format(mean_growth, digits = 4)))
print(paste("The Standard Deviation of the investment option Growth is: ", format(sd_growth, digits = 4)))


library(ggplot2)

df <- data.frame(
  option = rep(c("Defensive", "Growth"), each = length(return_data$Defensive)),
  value = c(return_data$Defensive, return_data$Growth)
)

ggplot(df, aes(x = option, y = value, fill = option)) + 
  geom_boxplot() +
  labs(title = "Box plot of Defensive and Growth investment options",
       x = "Option", y = "Return")

mean_values <- data.frame(Option = c("Defensive", "Growth"),
                          Mean = c(mean_defensive, mean_growth))

ggplot(mean_values, aes(x = Option, y = Mean, fill = Option)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean returns of Defensive and Growth investment options",
       x = "Option", y = "Mean return") +
  scale_fill_discrete(name = "Option")
df_error <- data.frame(
  option = c("Defensive", "Growth"),
  mean = c(mean_defensive, mean_growth),
  sd = c(sd_defensive, sd_growth)
)

ggplot(df_error, aes(x = option, y = mean, fill = option)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(title = "Mean and standard deviation of Defensive and Growth investment options",
       x = "Option", y = "Return") +
  scale_fill_discrete(name = "Option")


library(survival)
library(tidyr)

#Kaplan -Meier Curve
data<-demography_data
data_long <- pivot_longer(data, cols = -Age, names_to = "Year", values_to = "Status")
surv_object <- Surv(data_long$Age, as.factor(data_long$Status))

km_fit <- survfit(surv_object ~ 1)
plot(km_fit, xlab = "Age", ylab = " Mortality Probability", main = "Kaplan-Meier Curve",
     col = c("blue", "red"), lwd = 2, lty = c(1, 2))
legend("topright", legend = c("Healthy", "Ill-Healthy/Dead"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

investment_data<-return_data
# Extract the returns for years 1-150
defensive_returns <- investment_data$Defensive[1:150]
growth_returns <- investment_data$Growth[1:150]

# Calculate the mean return for each investment option
defensive_mean_return <- mean(defensive_returns)
growth_mean_return <- mean(growth_returns)

# Set the time horizon for the projection
time_horizon <- 5

# Calculate the future values for each year in the time horizon
defensive_future_values <- numeric(length = time_horizon)
growth_future_values <- numeric(length = time_horizon)

for (i in 1:time_horizon) {
  defensive_future_values[i] <- sum(defensive_returns) + (sum(defensive_returns) * defensive_mean_return * i)
  growth_future_values[i] <- sum(growth_returns) + (sum(growth_returns) * growth_mean_return * i)
}

# Create a data frame for the future values
future_values_df <- data.frame(
  Year = seq(151, 155),
  Defensive = defensive_future_values,
  Growth = growth_future_values
)

# Plot the future values for each investment option
library(ggplot2)

ggplot(future_values_df, aes(x = Year, y = Defensive, color = "Defensive")) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = Growth, color = "Growth")) +
  geom_point(aes(y = Growth, color = "Growth")) +
  xlab("Year") +
  ylab("Percentage of Projected Return (%)") +
  ggtitle("Projected Returns for Defensive and Growth Options (Years 151-155)") +
  scale_color_manual("", values = c("Defensive" = "blue", "Growth" = "red")) +
  theme_bw()
library(knitr)
library(kableExtra)
# Create a data frame for the future values
future_values_df <- data.frame(
  Year = seq(151, 155),
  Defensive = defensive_future_values,
  Growth = growth_future_values
)

# Format the table using kable
kable(future_values_df, format = "html", align = "c", caption = "Projected Returns for Defensive and Growth Options (%)") %>%
  kable_styling(bootstrap_options = "striped")





#MONTE-CARLO SIMULATION
install.packages("tidyverse")
library(tidyverse)
data<-demography_data
# Create an empty vector to store the results of the Monte Carlo simulation
results <- c()

# Set the number of iterations for the simulation
n <- 1000

# Run the simulation
for (i in 1:n) {
  
  # Generate a random index for the year
  year_index <- sample(2:ncol(data), 1)
  
  # Calculate the total number of people in the selected year
  total <- sum(data[, year_index])
  
  # Calculate the probability of each category (health, ill-health, death)
  prob_health <- sum(data[data$Age >= 18 & data$Age <= 65, year_index]) / total
  prob_illhealth <- sum(data[data$Age > 65, year_index]) / total
  prob_death <- sum(data[data$Age < 18, year_index]) / total
  
  # Generate a random number and determine the health status category
  rand <- runif(1)
  
  if (rand < prob_health) {
    result <- "Health"
  } else if (rand < prob_health + prob_illhealth) {
    result <- "Ill-Health"
  } else {
    result <- "Death"
  }
  
  # Add the result to the results vector
  results <- c(results, result)
  
}

numeric_results <- as.numeric(factor(results, levels = c("Health", "Ill-Health", "Death"), labels = c(1, 2, 3)))


# Convert the data to a long format and convert Status to a factor variable
data_long <- pivot_longer(data, -Age, names_to = "Year", values_to = "Status") %>%
  mutate(Status = factor(Status, levels = c(1, 2, 3),
                         labels = c("Health", "Ill-Health", "Death")))

# Define the colors for the bars
colors <- c("blue", "green", "red")

# Create a histogram with three bars and a legend
ggplot(data_long, aes(x = Status, fill = Status)) +
  geom_bar() +
  scale_fill_manual(values = colors, name = "Health Status") +
  labs(title = "Monte Carlo Simulation Results",
       x = "Health Status Category",
       y = "Count") +
  theme_minimal()

