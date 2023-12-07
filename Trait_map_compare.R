# Frequency table for the global dataset ----
global_frequency <- table(trait_data$TraitName)
print("Global Frequency Table:")
print(global_frequency)

# Frequency table for the filtered dataset with only African countries
africa_frequency <- table(trait_africa$TraitName)
print("African Countries Frequency Table:")
print(africa_frequency)



## Convert frequency tables to data frames ----
global_frequency_df <- as.data.frame(global_frequency)
africa_frequency_df <- as.data.frame(africa_frequency)

# Rename columns for clarity
colnames(global_frequency_df) <- c("Trait", "Global_Frequency")
colnames(africa_frequency_df) <- c("Trait", "Africa_Frequency")

# Merge data frames based on the Trait column
merged_frequency <- merge(global_frequency_df, africa_frequency_df, by = "Trait", all = TRUE)

# Creating a vector of new trait names
new_trait_names <- c("LA(undefined)", "LA(leaf, petiole ex)",
                     "LA(leaf, petiole in)", "LA(leaf, petiole undefined)",
                     "LA(leaflet, petiole ex)", "LA(leaflet, petiole in)",
                     "LA(leaflet, petiole undefined)", "SLA(petiole, rhachis, midrib ex)",
                     "SLA(petiole ex)", "SLA(petiole in)",
                     "SLA(petiole undefined)", "LLS",
                     "Leaf(N)", "Vcmax/LA", 
                     "Vcmax/LMA", "Root rooting depth")

# Mutate the merged_frequency data frame with the new trait names
merged_frequency <- merged_frequency %>%
  mutate(Trait_New = new_trait_names[Trait])

# Assuming merged_frequency$Trait is a factor, if not, convert it to a factor first
merged_frequency$Trait_New <- factor(merged_frequency$Trait_New, levels = unique(merged_frequency$Trait_New)) 


# Plot the bar chart ----
ggplot(merged_frequency, aes(x = Trait_New, y = Global_Frequency, fill = "Global")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = Africa_Frequency, fill = "Africa"), stat = "identity", position = "dodge") +
  labs(title = "Trait Frequency Comparison",
       x = "Trait",
       y = "Frequency") +
  scale_fill_manual(values = c("Global" = "blue", "Africa" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))






# Summary for differences without missing traits ----

# Export merged_frequency to a CSV file
write.csv(merged_frequency, "merged_frequency.csv", row.names = FALSE)




##  Omit the traits not found in Africa ----
# Remove missing values from the merged frequency data frame
merged_frequency <- na.omit(merged_frequency)

# Plot the bar chart
ggplot(merged_frequency, aes(x = Trait_New, y = Global_Frequency, fill = "Global")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = Africa_Frequency, fill = "Africa"), stat = "identity", position = "dodge") +
  labs(title = "Trait Frequency Comparison",
       x = "Trait",
       y = "Frequency") +
  scale_fill_manual(values = c("Global" = "blue", "Africa" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))






## The difference in percentage ----
# Calculate percentage differences
merged_frequency <- merged_frequency %>%
  mutate(Africa_Percentage = (Africa_Frequency / Global_Frequency) * 100)

# Plot the bar chart with percentage differences
ggplot(merged_frequency, aes(x = Trait_New, y = Africa_Percentage, fill = "Africa")) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Global Traits in Africa",
       x = "Trait",
       y = "Percentage") +
  scale_fill_manual(values = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))



# visualizing this as a heat map plot for each one of the eight traits ----
# Create a heatmap for percentage differences
ggplot(merged_frequency, aes(x = Trait_New, y = 1, fill = Africa_Percentage)) +
  geom_tile() +
  labs(title = "Percentage of Global Traits in Africa",
       x = "Trait",
       y = NULL) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank())


# Create a heatmap for absolute frequencies
ggplot(merged_frequency, aes(x = Trait_New, y = 1, fill = Global_Frequency)) +
  geom_tile() +
  labs(title = "Absolute Frequencies of Traits in Global and Africa",
       x = "Trait",
       y = NULL) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank()) +
  geom_tile(aes(x = Trait_New, y = 2, fill = Africa_Frequency))



