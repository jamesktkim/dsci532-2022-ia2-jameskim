library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(dashHtmlComponents)
library(ggplot2)
library(tidyverse)
library(here)
library(plotly)

# Read in the gapminder data
gap <- read_csv(here("data", "gapminder.csv"))
# Create dictionary for stat labels
labels <- list(
  "life_expectancy" = "Life Expectancy",
  "pop_density" = "Population Density",
  "child_mortality" = "Child Mortality"
)
# Options
metrics <- list(
  list("label" = "Life Expectancy", "value" = "life_expectancy"),
  list("label" = "Child Mortality", "value" = "child_mortality"),
  list("label" = "Population Density", "value" = "pop_density")
)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccDropdown(
        id='col-select',
        options = metrics,
        value='life_expectancy'),
      dccGraph(id='plot-area')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    gap_10 <- gap %>%
      filter(year == 2010) %>%
      arrange(desc(!!sym(xcol))) %>%
      slice(1:10)
    p <- ggplot(gap_10, aes(x = !!sym(xcol),
                            y = reorder(country, !!sym(xcol)),
                            fill = country,
                            name = country,
                            count = !!sym(xcol))) +
      geom_bar(stat='identity') +
      labs(title = paste0(labels[[xcol]], " - Top 10 Country for Year 2010"),
           y = "Country",
           x = labels[[xcol]],
           colour = "Country") +
      theme_classic()

    ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')
