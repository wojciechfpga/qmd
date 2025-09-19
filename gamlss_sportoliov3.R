library(ggplot2)
library(gamlss)
data("quakes")


ggplot(quakes, aes(x=mag)) +
  geom_histogram()

quakes_mod <- quakes %>%
  mutate(
    classifcation = case_when(
      mag < 4.7 ~ "Weak",
      TRUE ~ "Strong"
    )
  )

ggplot(quakes_mod, aes(x = stations, y = lat, color = classifcation))+
  geom_point()

dist_ranking <- gamlss::fitDist(quakes_mod$lat, type = "realAll")

example_of_SICHEL <- rSICHEL(1000)

example_of_SICHEL_de <- data.frame(
  SICHEL = example_of_sep4
)

ggplot(example_of_SICHEL_de, aes(x = SICHEL))+
  geom_histogram()

gamlss_object <- gamlss(
  lat ~pb(stations),
  sigma.formula = ~pb(stations),
  nu.formula = ~pb(stations),
  tau.formula = ~pb(stations),
  family = SEP4(),
  data = quakes_mod
)

precition_from_gamlss <- predict(gamlss_object,type = "response" )

precition_from_gamlss_df <- data.frame(
  sequence = quakes_mod$stations,
  precition_from_gamlss = precition_from_gamlss
)

ggplot(quakes_mod, aes(x = stations, y = lat, color = classifcation))+
  geom_point()+
  geom_line(data = precition_from_gamlss_df, 
            aes(x = sequence, y = precition_from_gamlss), 
            color = "green",
            size = 1.5,
            inherit.aes = FALSE)