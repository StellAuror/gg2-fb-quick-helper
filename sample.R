library(tidyverse)
library(ggpubr)

stk <- function(x) {
  x / sum(x)
}

evals <- c("Often", "Frequently", "Hardly ever", "Never", "Ugabuga")

# Generate sample data
data.frame(
  factor = rep(unlist(lapply(rep(2:3, 4), function(x) lorem::ipsum_words(x))), 5),
  eval = rep(evals, 4),
  value = sample(5:100, 20)
) %>%
  group_by(factor) %>%
  mutate(
    eval = factor(eval, levels = evals, ordered = TRUE),
    value = stk(value)
) -> df1

# Calculate position and text color
df1 %>%
  arrange(eval) %>%
  group_by(factor) %>%
  mutate(
    position = cumsum(value),
    txcolor = if_else(eval == "Ugabuga", "black", "white")
  ) -> dflab

# Create labels for the second plot
data.frame(
  factor = unique(df1$factor),
  label = sample(10:100, 8) %>% paste0(., "%")
) -> df2

# First plot
p1 <- df1 %>%
  ggplot(aes(x = value, y = factor, fill = eval)) +
  geom_col() +
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
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "",
    y = "",
    fill = "Interviewee Respond",
    title = "Some Ultra Important Poll",
    subtitle = "Based on Even More Relevant Studies"
  ) +
  scale_fill_manual(
    values = c("#03314e", "#07619b", "#077fcd", "#7f7f7f", "#f6f6f6")
  )

# Second plot
p2 <- df2 %>%
  ggplot(aes(y = factor, x = 1)) +
  theme_void() +
  geom_text(aes(label = label), color = "#03314e") +
  labs(
    title = lorem::ipsum_words(2),
    subtitle = lorem::ipsum_words(3)
  )

# Arrange both plots side by side
ggarrange(p1, p2, ncol = 2, widths = c(8, 2), common.legend = TRUE, legend = "bottom")
