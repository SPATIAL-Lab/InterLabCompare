library(dplyr); library(tidyr);library(ggplot2)

summ <- long %>% 
  group_by(iso, lab, sample) %>% 
  summarize(mean = mean(value), 
            sd = sd(value), 
            min = min(value),
            max = max(value))

# huh some samples seem to just have higher SD 

ggplot() + 
  geom_point(data = summ, aes(x = sd, y = mean, color = sample, shape = iso))

ggplot() + 
  geom_col(data = summ, aes(y = sd, x = sample, color = iso, fill = lab), position = "dodge") + 
  theme_classic()
