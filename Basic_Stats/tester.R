#Testing
chicks <- as_tibble(ChickWeight)

grp_stat <- chicks %>%
  filter(Time == 21) %>%
  group_by(Diet, Time) %>%
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

library(ggpubr) # needed for arranging multi-panel plots
plt1 <- chicks %>%
  filter(Time == 21) %>%
  ggplot(aes(x = Diet, y = weight)) +
  geom_point(data = grp_stat, aes(x = Diet, y = mean_wt),
             col = "black", fill = "red", shape = 23, size = 3) +geom_jitter(width = 0.05) + # geom_point() if jitter not required
  labs(y = "Chicken mass (g)") +
  theme_pubr()
plt1
plt2 <- ggplot(data = grp_stat, aes(x = Diet, y = mean_wt)) +
  geom_bar(position = position_dodge(), stat = "identity",
           col = NA, fill = "salmon") +
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)") +
  theme_pubr()
plt2
# position_dodge() places bars side-by-side
# stat = "identity" prevents the default count from being plotted
# a description of the components of a boxplot is provided in the help file
# geom_boxplot()
plt3 <- chicks %>%
  filter(Time == 21) %>%
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)") +
  theme_pubr()
plt4 <- chicks %>%
  filter(Time %in% c(10, 21)) %>%
  ggplot(aes(x = Diet, y = weight, fill = as.factor(Time))) +
  geom_boxplot() +
  geom_jitter(shape = 21, width = 0.1) +
  labs(y = "Chicken mass (g)", fill = "Time") +
  theme_pubr()


cuckootest <- cuckoos %>%
  group_by(species) %>%
  summarise(n = n(),
            mean = mean(length),
            sd = sd(length)) %>%
  mutate(se = sd/sqrt(n))  %>%
  mutate(ic = se * qt((1-0.05)/2 + .5, n-1))

ggplot(cuckootest) +
  geom_bar(aes(x = species, y = mean), stat="identity", fill="forestgreen", alpha=0.5) +
  geom_errorbar(aes(x = species, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using standard error")