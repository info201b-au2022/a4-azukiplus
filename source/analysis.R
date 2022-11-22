library(tidyverse)
library(ggplot2)
library(plotly)

# The functions might be useful for A4
source("../source/a4-helpers.R")
# source("C:/Users/ashle/Documents/info201/assignments/a4-azukiplus/source/a4-helpers.R")


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Summary of values in the dataset that are relevant
#----------------------------------------------------------------------------#
incarceration_df <- get_data()
summary_list <- list()

# Average % of female population in jail in 2018
summary_list$avg_female_2018 <- incarceration_df %>%
  filter(
    year == 2018,
    !is.na(total_pop_15to64),
    !is.na(male_jail_pop),
    !is.na(female_jail_pop)
  ) %>%
  mutate(percent_female = female_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_female = mean(percent_female)) %>%
  pull(avg_percent_female)

# Average % of male population in jail in 2018
summary_list$avg_male_2018 <- incarceration_df %>%
  filter(year == 2018) %>%
  filter(!is.na(total_pop_15to64)) %>%
  filter(!is.na(male_jail_pop)) %>%
  filter(!is.na(female_jail_pop)) %>%
  mutate(percent_male = male_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_male = mean(percent_male)) %>%
  pull(avg_percent_male)

# Average % of female population in jail in 2010
summary_list$avg_female_2010 <- incarceration_df %>%
  filter(year == 2010) %>%
  filter(!is.na(total_pop_15to64)) %>%
  filter(!is.na(male_jail_pop)) %>%
  filter(!is.na(female_jail_pop)) %>%
  mutate(percent_female = female_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_female = mean(percent_female)) %>%
  pull(avg_percent_female)

# Average % of male population in jail in 2010
summary_list$avg_male_2010 <- incarceration_df %>%
  filter(year == 2010) %>%
  filter(!is.na(total_pop_15to64)) %>%
  filter(!is.na(male_jail_pop)) %>%
  filter(!is.na(female_jail_pop)) %>%
  mutate(percent_male = male_jail_pop / total_pop_15to64) %>%
  summarize(avg_percent_male = mean(percent_male)) %>%
  pull(avg_percent_male)

# Jails with more female prisoners every year from 1995 to 2018
summary_list$more_female_recent <- incarceration_df %>%
  filter(
    2000 <= year, year <= 2018,
    !is.na(male_jail_pop),
    !is.na(female_jail_pop),
    male_jail_pop < female_jail_pop
  ) %>%
  count() %>%
  pull(n)

# Jails with more female prisoners every year from 1970 to 1994
summary_list$more_female_before <- incarceration_df %>%
  filter(
    1970 <= year, year <= 2000,
    !is.na(male_jail_pop),
    !is.na(female_jail_pop),
    male_jail_pop < female_jail_pop
  ) %>%
  count() %>%
  pull(n)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# 
#----------------------------------------------------------------------------#

# This function gets the population of people who were incarcerated in the U.S. by year
get_year_jail_pop <- function() {
  res <- incarceration_df %>%
  group_by(year) %>%
  summarize(sum_jail_pop = sum(total_jail_pop, na.rm = t))
  return(res)
}

# This function plots the total population of people who were incarcerated in the U.S. from 1970 to 2018
plot_jail_pop_for_us <- function() {
  chart <- ggplot(data = get_year_jail_pop()) +
  geom_col(
    mapping = aes(
      x = year, 
      y = sum_jail_pop)) +
  labs(
    x = "Year", 
    y = "Jail Population (people)", 
    title = "Jail population totals in the U.S. from 1970-2018", 
    caption = "This graph shows the increasing trend of total jail population over the years 1970 to 2018.") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::label_comma()) # remove scientific notation
  return(chart)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#----------------------------------------------------------------------------#

# This function gets the population of people who were incarcerated in state(s) by year
# Parameters:
#   states - the states of which the populations will be looked at
get_jail_pop_for_states <- function(states) {
  res <- incarceration_df %>%
    filter(state %in% states) %>% 
    group_by(state, year) %>%
    summarize(sum_jail_pop = sum(total_jail_pop, na.rm = t), .groups = "keep")
  return(res)
}

states <- get_jail_pop_for_states(c('CA', 'WA'))

# This function graphs the total population of people who were incarcerated in state(s) from 1970 to 2018
# Parameters: 
#   states - the states of which the populations will be graphed
plot_jail_pop_for_states <- function(states) {
  chart <- ggplot(get_jail_pop_for_states(states)) +
    geom_line(
      mapping = aes(
        x = year, 
        y = sum_jail_pop, 
        color = state)) +
    labs(
      x = "Year", 
      y = "Jail Population (people)", 
      title = "Jail population totals in the West Coast from 1970-2018",
      caption = "The graph shows the increasing trend of West Coast state jail populations from 1970-2018.") +
    scale_fill_hue(c=40) +
    scale_y_continuous(labels = scales::label_comma()) # remove scientific notation
  return(chart)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Mapping the gender differences across prison populations in the U.S.
# 
#----------------------------------------------------------------------------#
# This function gets the gender ratio in jails by year
get_year_gender_jail_ratio <- function() {
  res <- incarceration_df %>%
    group_by(year) %>%
    summarize(
      sum_jail_pop = sum(female_jail_pop, na.rm = t) + sum(male_jail_pop, na.rm = t),
      ratio_female_jail = sum(female_jail_pop, na.rm = t) / sum_jail_pop,
      ratio_male_jail = sum(male_jail_pop, na.rm = t) / sum_jail_pop
    )
  return(res)
}

# This function uses the information about gender ratios from the helper function above
# and plots them onto a graph
plot_year_gender_jail_ratio <- function() {
  data <- get_year_gender_jail_ratio()
  fig <- plot_ly(
    data = data,
    x = ~year,
    y = ~ratio_female_jail,
    type = "bar",
    name = "Female",
    marker = list(color = "#d48166")) %>% add_trace(
    y = ~ratio_male_jail,
    name = "Male",
    marker = list(color = "#373a36")) %>% layout(
    title = "Gender Ratios in Incarcerated Populations (1970-2018)",
    xaxis = list(title = "Year", range = list(1969, 2019)),
    yaxis = list(title = "Ratio"),
    barmode = "stack",
    margin = list(l = 0, r = 0, t = 50, b = 100),
    paper_bgcolor = "#e6e2dd",
    plot_bgcolor = "#e6e2dd",
    annotations = list(
      text = "Shows the gender ratio in incarcerated populations in the U.S. from 1970 to 2018.",
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "bottom",
      x = 1, y = -0.2, showarrow = FALSE
    )
  )
  return(fig)
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# Mapping gender differences in prison populations geographically by state
# get_year_gender_jail_ratio2 -> gets the gender percentage ratio in jails by state
# map_year_gender_jail_ratio -> maps the above info
#----------------------------------------------------------------------------#

# Gets the gender percentage ratio in jails by state
get_year_gender_jail_ratio2 <- function() {
  res <- incarceration_df %>%
    group_by(year, state) %>%
    summarize(
      sum_jail_pop = sum(female_jail_pop, na.rm = t) + sum(male_jail_pop, na.rm = t),
      percent_female_jail = sum(female_jail_pop, na.rm = t) * 100 / sum_jail_pop
    )
  return(res)
}

# Maps gender percentage ratio in jails by state
map_year_gender_jail_ratio <- function() {
  data <- get_year_gender_jail_ratio2()
  
  fig <- plot_geo(
    data = data,
    locationmode = "USA-states"
  ) %>%
    add_trace(
      locations = ~state,
      frame = ~year,
      z = ~percent_female_jail,
      zmin = 0, zmax = 30,
      marker = list(line = list(width = 0))
    ) %>%
    colorbar(title = "Female Jail Population (%)") %>%
    layout(
      title = "Female Incarcerated Population Percentage Ratios By State",
      geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        bgcolor = "#e6e2dd"
      ),
      paper_bgcolor = "#e6e2dd",
      plot_bgcolor = "#e6e2dd",
      margin = list(l = 50, r = 50, t = 50, b = 120),
      annotations = list(
        text = "Shows the ratio percentage of the incarcerated population \n who are female in each U.S. state from 1970-2018.",
        xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "bottom",
        x = 1, y = -0.1, showarrow = FALSE
      )
    )
  return(fig)
}

