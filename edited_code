# Load required libraries
library(tidyverse)
library(ggpubr)

# Create the data frame
data <- data.frame(
  event = c("Talking with family", "Donating good", "Posting on social media", "Give interview", "Working/Volunteering for a charity", "Speaking with journalist", "Participating in protest"),
  lk1 = c(0.23, 0.20, 0.21, 0.40, 0.36, 0.40, 0.23),
  lk2 = c(0.10, 0.36, 0.30, 0.12, 0.12, 0.12, 0.25),
  lk3 = c(0.36, 0.20, 0.37, 0.36, 0.40, 0.36,0.40),
  lk4 = c(0.31, 0.24, 0.12, 0.12, 0.12, 0.12, 0.12),
  lk1_lk2 = c(0.33, 0.56, 0.51, 0.52, 0.48, 0.52, 0.48)
)

# Reshape the data for ggplot
df1_1 <- data %>%
  pivot_longer(cols = c(lk1:lk4), names_to = "eval", values_to = "value") %>%
  mutate(
    factor = factor(event, levels = rev(unique(event))),
    eval = factor(eval, levels = c("lk4", "lk3", "lk2", "lk1")) # Specify the desired order of bars
  ) %>%
  arrange(eval)

# Calculate position and text color
dflab <- df1_1 %>%
  group_by(factor) %>%
  mutate(
    position = cumsum(value),
    txcolor = if_else(eval == "lk4", "black", "white")
  )

# First plot
p1 <- df1_1 %>%
  ggplot(aes(x = value, y = factor, fill = eval)) +
  geom_col(width = 0.8) + # Adjust the width of the bars
  geom_text(
    aes(
      label = paste0(round(dflab$value * 100, 0), "%"),
      x = 1 - dflab$position + (dflab$value / 2),
      y = dflab$factor
    ),
    color = dflab$txcolor
  ) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10), # Adjust the size of y-axis labels
    legend.text = element_text(size = 10) # Adjust the size of legend text
  ) +
  labs(
    x = "Percent",
    y = "",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#f6f6f6","#077fcd","#07619b","#03314e"),
    labels = c("lk1" = "Often", "lk2" = "Occasionally", "lk3" = "Never", "lk4" = "Prefer not to say")
  ) +
  theme(
    legend.box.background = element_rect(color = "black", size = 1) # Add box frame around the legend
  )

# Reshape the data for ggplot
df2_1 <- data %>%
  select(event, lk1_lk2) %>%
  mutate(
    factor = event,
    label = paste0(round(data$lk1_lk2 * 100, 0), "%")
  ) %>%
  select(factor, label)

df2_1$factor <- factor(df2_1$factor, levels = rev(unique(df2_1$factor)))

# Second plot
p2 <- df2_1 %>%
  ggplot(aes(y = factor, x = 1)) +
  theme_void() +
  scale_x_continuous(limits = c(0.9, 1.2)) +
  geom_text(aes(label = label), size = 5, color = "#07619b", show.legend = F) +
  labs(
    title = "Do at All",
    subtitle = "(Often and Occasionally)"
  ) +
  theme(
    plot.title = element_text(color = "#07619b", size = 20), # Set color and size for plot title
    plot.subtitle = element_text(color = "#07619b", size = 14) # Set color and size for plot subtitle
  )

# Arrange both plots side by side
ggarrange(p1, p2, ncol = 2, widths = c(6, 1.5), common.legend = F, legend = "top")
