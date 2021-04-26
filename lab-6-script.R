
# load packages -----------------------------------------------------------

library(tidyverse)

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# do stuff ----------------------------------------------------------------

fish_location_summary <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

t.test(formula = species ~ location, data = fish_long)

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 10, 
    alpha = 0.5, 
    position = "identity"
  ) +
  geom_histogram(binwidth = 2)
  
  

