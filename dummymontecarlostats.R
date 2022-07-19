summary_values <- simulations %>%
  group_by(step) %>%
  summarise(mean_return = mean(value), max_return = max(value), min_return = min(value)) %>%
  gather("series", "value", -step)

summary_values %>%
  ggplot(aes(x = step, y = value)) +
  geom_line(aes(color = series)) +
  ggtitle("Mean values from simulations")