# Week 2: Titanic Data Visualization and Insight Communication
# Using R

# Load required libraries
library(ggplot2)
library(dplyr)

# Load Titanic dataset
titanic <- read.csv("titanic-1.csv")

# View basic information
head(titanic)
str(titanic)
summary(titanic)

# Convert categorical variables to factors
titanic$Survived <- factor(titanic$Survived,
                            levels = c(0, 1),
                            labels = c("Not Survived", "Survived"))

titanic$Sex <- factor(titanic$Sex)
titanic$Pclass <- factor(titanic$Pclass)

# ------------------------------------------------
# Visualization 1: Survival Count
# ------------------------------------------------

ggplot(titanic, aes(x = Survived)) +
  geom_bar() +
  labs(
    title = "Titanic Survival Count",
    x = "Survival Status",
    y = "Number of Passengers"
  ) +
  theme_minimal()

# ------------------------------------------------
# Visualization 2: Survival by Gender
# ------------------------------------------------

ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  theme_minimal()

# ------------------------------------------------
# Visualization 3: Survival by Passenger Class
# ------------------------------------------------

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  theme_minimal()

# ------------------------------------------------
# Visualization 4: Age Distribution
# ------------------------------------------------

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  labs(
    title = "Age Distribution of Titanic Passengers",
    x = "Age",
    y = "Number of Passengers"
  ) +
  theme_minimal()

# ------------------------------------------------
# Visualization 5: Age and Fare Relationship
# ------------------------------------------------

ggplot(titanic, aes(x = Age, y = Fare, color = Survived)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  labs(
    title = "Relationship Between Age and Fare",
    x = "Age",
    y = "Fare",
    color = "Survival Status"
  ) +
  theme_minimal()

# ------------------------------------------------
# Visualization 6: Fare Distribution by Passenger Class
# ------------------------------------------------

ggplot(titanic, aes(x = Pclass, y = Fare)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Fare Distribution by Passenger Class",
    x = "Passenger Class",
    y = "Fare"
  ) +
  theme_minimal()

# ------------------------------------------------
# Key Summary Statistics
# ------------------------------------------------

survival_summary <- titanic %>%
  group_by(Survived) %>%
  summarise(
    Passenger_Count = n()
  )

print(survival_summary)

gender_summary <- titanic %>%
  group_by(Sex, Survived) %>%
  summarise(
    Passenger_Count = n(),
    .groups = "drop"
  )

print(gender_summary)

class_summary <- titanic %>%
  group_by(Pclass, Survived) %>%
  summarise(
    Passenger_Count = n(),
    .groups = "drop"
  )

print(class_summary)
