# Grouped Breakdown 


library(sf)
library(ggplot2)
library(maps)
library(tigris)
options(tigris_use_cache = TRUE)
library(USAboundaries)


occ_grouped_Healthcare

ggplot(occ_grouped_Healthcare, aes(x = mean, y = cv)) +
  geom_point(color = "blue") +  # Plot the points
  geom_text(aes(label = State), hjust = 0.5, vjust = -0.5, size = 3) +  # Add labels
  labs(title = "Scatter Plot of Mean vs Coefficient of Variation (CV)",
       x = "Mean", y = "Coefficient of Variation (CV)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

ggplot(occ_grouped_Healthcare, aes(x = State, y = cv)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Coefficient of Variation (CV) by State",
       x = "State", y = "Coefficient of Variation (CV)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 3)) +
  theme_minimal()

occ_grouped_Healthcare
ggplot(occ_grouped_Healthcare, aes(x = State, y = median)) +
  geom_bar(stat = "identity")
