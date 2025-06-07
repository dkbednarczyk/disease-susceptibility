library(dplyr)
library(usmap)
library(ggplot2)
library(lsr)
library(plotly)

incidence <- read.csv("./datasets/United States Cancer Statistics 1999-2021 Child Incidence.csv")
incidence <- incidence[order(incidence$state), ]

population <- read.csv("./datasets/cwoutcomes-acf-hhs-gov-export-2025-06-07.csv")
population <- population[order(population$state), ]

incidence$count <- (incidence$count / population$population) * 100000

south <- incidence %>%
  filter(region == "South")

northeast <- incidence %>%
  filter(region == "Northeast")

midwest <- incidence %>%
  filter(region == "Midwest")

west <- incidence %>%
  filter(region == "West")

# Run to show labeled bar chart of average incidences
plot_ly(
  x = c("South", "Northeast", "Midwest", "West"),
  y = c(
    ceiling(sum(south$count)/nrow(south)),
    ceiling(sum(northeast$count)/nrow(northeast)),
    ceiling(sum(midwest$count)/nrow(midwest)),
    ceiling(sum(west$count)/nrow(west))
  ),
  name = "Average childhood cancer incidence per 100,000 by US region",
  text = y,
  type = "bar"
) %>%
  layout(title="Average childhood cancer incidence per 100,000 by US region")

# Run to show US map shaded per state
plot_usmap(values="count", data=incidence) +
  scale_fill_continuous(low = "white", high = "red", name = "Cases per 100,000 children", label = scales::comma) +
  labs(title = "US Childhood Cancer Statistics, 1999-2021", subtitle = "Incidence rate per state") +
  theme(legend.position = "right")
