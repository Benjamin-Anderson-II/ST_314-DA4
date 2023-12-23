population <- tibble(
  hip = c(rep("Squeaking", 17500), rep("No squeaking", 232500))
)
ggplot(population, aes(x=hip)) +
  geom_bar() + 
  labs(x="", y = "Count",
       title = "Ceramic hip patients that develop squeaking")
population %>% 
  count(hip) %>% 
  mutate(p=n/sum(n))

samp1 <- population %>%
  sample_n(200)

samp1 %>%
  count(hip) %>%
  mutate(p_hat=n/sum(n))

samp2 <- population %>%
  sample_n(200)

samp2 %>%
  count(hip) %>%
  mutate(p_hat=n/sum(n))

sample_props200 <- population %>%
  rep_sample_n(size=200, reps = 10000, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")

sample_props_small <- population %>%
  rep_sample_n(size=100, reps = 10, replace = TRUE) %>%
  count(hip) %>%
  mutate(p_hat = n/sum(n)) %>%
  filter(hip == "Squeaking")

p <- 0.07
sampling_dist <- 0.018
less_than_4_percent <- pnorm(0.04, p, sampling_dist)
less_than_4_percent
less_than_10_percent <- pnorm(0.1, p, sampling_dist)
more_than_10_percent <- 1-less_than_10_percent
more_than_10_percent
between_5_and_10_percent <- less_than_10_percent - pnorm(0.05, p, sampling_dist)
between_5_and_10_percent
