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
incidence$count <- incidence$count / 22

south <- incidence %>%
  filter(region == "South")

northeast <- incidence %>%
  filter(region == "Northeast")

midwest <- incidence %>%
  filter(region == "Midwest")

west <- incidence %>%
  filter(region == "West")

y <- c(
  trunc(sum(south$count)/nrow(south) * 100) / 100,
  trunc(sum(northeast$count)/nrow(northeast) * 100) / 100,
  trunc(sum(midwest$count)/nrow(midwest) * 100) / 100,
  trunc(sum(west$count)/nrow(west) * 100) / 100
)

# Run to show labeled bar chart of average incidences
plot_ly(
  x = c("South", "Northeast", "Midwest", "West"),
  y = y,
  name = "Average childhood cancer incidence per 100,000 by US region",
  text = y,
  type = "bar"
) %>%
  layout(title="Average annual childhood cancer incidence per 100,000 by US region")

# Run to show US map shaded per state
plot_usmap(values="count", data=incidence) +
  scale_fill_continuous(low = "white", high = "red", name = "Cases", label = scales::comma) +
  labs(title = "Average annual childhood cancer incidence per 100,000", subtitle = "1999-2021") +
  theme(legend.position = "right")
