library(tidyverse)
netflix_raw <- read_csv("data/netflix_titles.csv")
glimpse(netflix_raw)
install.packages("tidyverse")

library(tidyverse)

netflix_raw <- read_csv("data/netflix_titles.csv")

glimpse(netflix_raw)

glimpse(netflix_raw)
summary(netflix_raw)
colSums(is.na(netflix_raw))

# Cleaning missing values

netflix_clean <- netflix_raw %>%
  
  # Replace missing categorical values
  mutate(
    director = if_else(is.na(director), "Unknown", director),
    cast = if_else(is.na(cast), "Unknown", cast),
    country = if_else(is.na(country), "Unknown", country),
    rating = if_else(is.na(rating), "Unknown", rating)
  ) %>%
  
  # Remove rows with missing important values
  filter(!is.na(date_added), !is.na(duration))

colSums(is.na(netflix_clean))

# Check exact duplicate rows
sum(duplicated(netflix_clean))

# Since there are no duplicates we can skip your steps
# Now we will create one value in one row

# Separate countries into rows
netflix_countries <- netflix_clean %>%
  separate_rows(country, sep = ", ")

# Comparing number of rows 
nrow(netflix_clean)
nrow(netflix_countries)

# Visual check that shows more rows
head(netflix_countries$country, 20)

netflix_countries %>%
  filter(str_detect(country, ","))
# This should prove that there are no multiple countries in one column
# Date got mixed into, so we have to separate rows


separate_rows(country, sep = ", ")

# so we have to fix now the error, cause now it says the object country is not found


netflix_countries <- netflix_clean %>%
  separate_rows(country, sep = ", ")

colnames(netflix_clean)


library(tidyverse)

netflix_countries <- netflix_clean %>%
  separate_rows(country, sep = ", ")

head(netflix_countries)

# now we check that it actually worked the separation

nrow(netflix_clean)
nrow(netflix_countries)

# running again, to make sure it returns to 0 rows

netflix_countries %>%
  filter(str_detect(country, ","))

# we now have to clean it again, cause 5 remaining entries still contain the commas and are not properly cleaned

# manual cleaning for these 5 entries

# Inspect problematic country values
netflix_countries %>%
  filter(str_detect(country, ",")) %>%
  select(title, country)

netflix_countries <- netflix_countries %>%
  mutate(country = case_when(
    str_detect(country, "United States") ~ "United States",
    str_detect(country, "Cambodia") ~ "Cambodia",
    str_detect(country, "Poland") ~ "Poland",
    TRUE ~ country
  ))

netflix_countries %>%
  filter(str_detect(country, ","))

# so we tried cleaning it manually and 2 entries are still left, we will try to fix these too

netflix_countries <- netflix_countries %>%
  mutate(country = str_extract(country, "^[^,]+"))

netflix_countries %>%
  filter(str_detect(country, ","))

# so after multiple tries it finally came to 0
# ensuring that each entry contains a valid and atomic country value
# now we do the same things for genres, we have to do the separation process for that as well

netflix_genres <- netflix_clean %>%
  separate_rows(listed_in, sep = ", ")

head(netflix_genres$listed_in, 20)
# now we have to check if everything is right and if it states 0 again then we dont have to do manual cleaning


netflix_genres %>%
  filter(str_detect(listed_in, ","))

# it gaves us no results, as i mean zero, so wee are good to go to the next step


# data transformation and cleaning final step, now we want to change the date should be no text anymore, and the duration same

# with the next function it will generate our wished results

library(lubridate)

netflix_clean <- netflix_clean %>%
  mutate(date_added = mdy(date_added))

class(netflix_clean$date_added)

# now it shows us the result "DATE"

# next step, split into type and number

netflix_clean <- netflix_clean %>%
  mutate(
    duration_number = as.numeric(str_extract(duration, "\\d+")),
    duration_type = case_when(
      str_detect(duration, "Season") ~ "Seasons",
      str_detect(duration, "min") ~ "Minutes",
      TRUE ~ "Unknown"
    )
  )

# now we check if the duration number and type works

head(netflix_clean[, c("duration", "duration_number", "duration_type")])

# now we can compare movie lenghts, tv seasons, and we can also analyse trends


# RESEARCH QUESTIONS 1
# How has Netflix content changed over time?
# We want to count : how many Movies vs TV Shows per year

content_over_time <- netflix_clean %>%
  group_by(release_year, type) %>%
  summarise(count = n(), .groups = "drop")

# groups data by type and year, and how many entries per group are there, now visualization creation for it

# we decided to use ggplot, to create a line chart with movies and tv shows over time


ggplot(content_over_time, aes(x = release_year, y = count, color = type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Netflix Content Over Time",
    x = "Release Year",
    y = "Number of Titles",
    color = "Type"
  )

# now we want to change the graph with a theme

ggplot(content_over_time, aes(x = release_year, y = count, color = type)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Netflix Content Over Time",
    x = "Release Year",
    y = "Number of Titles",
    color = "Type"
  )

#Netflix content has increased significantly over time, with a notable rise in TV shows after 2015, suggesting a strategic shift toward serialized content.

#The slight decline in the most recent year is likely due to incomplete data rather than an actual decrease in content.

# RESEARCH QUESTION 2
# How does Netflix content differ across countries?
# We will also create a graph for this, with the previously cleaned dataset

netflix_countries
top_countries <- netflix_countries %>%
  filter(country != "Unknown") %>%
  count(country, sort = TRUE)

# this counts how many titles are there per country and also sorts from highest to lowest

# We have to select the top 10 cause otherwise there would be just way too much entries

top_countries_10 <- top_countries %>%
  slice_head(n = 10)

# now we create the chart w the same ggplot

ggplot(top_countries_10, aes(x = reorder(country, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Countries by Netflix Content",
    x = "Country",
    y = "Number of Titles"
  )



# the unknown countries we will remove, cause those are all the other countries together that had missing values

# now we will update it, and we will overwrite with the new code, what we did first was this code
# top_countries <- netflix_countries %>%
# count(country, sort = TRUE)

# this will be now overwritten above 
# now we ran all the codes below the overwritten code and now we dont have an unknown country anymore

# RESEARCH QUESTION 3
# Which genres are most common on Netflix?
# We will use again the same cleaned data, one genre per row, and it will be good for counting

netflix_genres
top_genres <- netflix_genres %>%
  count(listed_in, sort = TRUE)

# now we do basically the same steps but with genres

top_genres_10 <- top_genres %>%
  slice_head(n = 10)

ggplot(top_genres_10, aes(x = reorder(listed_in, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Netflix Genres",
    x = "Genre",
    y = "Number of Titles"
  )

# now we got the next chart
# The presence of “International” categories highlights Netflix’s global strategy and diverse content offering.
# This indicates that Netflix focuses on widely appealing genres with broad audience reach.


# Now the R script is done, next step is presentation. We have all the data now for it.


# Extra recommendation from Coaching session, now we will continue the code and add the ratings of the movies/series, Advanced Analysis

# Rating distribution: Which ratings appear most often on Netflix

# The ratings include what age group they reffer to or are most suitable for. 

rating_distribution <- netflix_clean %>%
  filter(rating != "Unknown") %>%
  count(rating, sort = TRUE)

rating_distribution

# now wee have the data we create visuals w ggplot

ggplot(rating_distribution, aes(x = reorder(rating, n), y = n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Ratings on Netflix",
    x = "Rating",
    y = "Number of Titles"
  )

# This visualization shows which age ratings appear most frequently in the Netflix dataset.
# The dominant ratings indicate which audience groups Netflix mainly targets.
#A high number of mature ratings, such as TV-MA or TV-14, would suggest that Netflix offers a large amount of content for older teenagers and adults.



# Rating comparison by content type : Do Movies and TV shows differ in their rating distribution
rating_by_type <- netflix_clean %>%
  filter(rating != "Unknown") %>%
  count(type, rating)

rating_by_type


ggplot(rating_by_type, aes(x = reorder(rating, n), y = n, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Ratings by Content Type",
    x = "Rating",
    y = "Number of Titles",
    fill = "Type"
  )


# This chart compares how ratings are distributed between Movies and TV Shows. It helps identify whether certain ratings are more common for one content type.
# This is useful because it shows whether Netflix targets similar or different audience groups with movies and series.

# Scatter plot : Relationship between relase_year and duration_number

ggplot(netflix_clean, aes(x = release_year, y = duration_number)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  theme_minimal() +
  labs(
    title = "Relationship Between Release Year and Duration",
    x = "Release Year",
    y = "Duration"
  )

# This scatter plot shows the relationship between release year and content duration. The regression line indicates the overall trend. The data points are widely scattered, suggesting that there is only a weak relationship between the two variables.
# This means that content duration has not significantly changed over time.

# FUN EXTRA: Reccomendation in genres for different content preferences
netflix_reco <- netflix_clean %>%
  mutate(age_group = case_when(
    rating %in% c("G", "PG") ~ "Kids",
    rating %in% c("PG-13", "TV-14") ~ "Teens",
    rating %in% c("TV-MA", "R") ~ "Adults",
    TRUE ~ "Other"
  ))

recommendations <- netflix_reco %>%
  filter(age_group != "Other") %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(age_group, listed_in, sort = TRUE)


recommendations %>%
  group_by(age_group) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(listed_in, n), y = n, fill = age_group)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~age_group, scales = "free") +
  theme_minimal() +
  labs(
    title = "Recommended Genres by Age Group",
    x = "Genre",
    y = "Number of Titles"
  )


# We dont like this, because it recommends just the genres, we want titles so we will recreate this.

netflix_reco <- netflix_clean %>%
  mutate(age_group = case_when(
    rating %in% c("G", "PG") ~ "Kids",
    rating %in% c("PG-13", "TV-14") ~ "Teens",
    rating %in% c("TV-MA", "R") ~ "Adults",
    TRUE ~ "Other"
  ))

recommendation_titles <- netflix_reco %>%
  filter(age_group != "Other") %>%
  group_by(age_group) %>%
  slice_sample(n = 5) %>%
  select(age_group, title, type, listed_in)

recommendation_titles


# instead we want to use 1 tv show and 1 movie for ach age group

recommendation_titles <- netflix_reco %>%
  filter(age_group != "Other") %>%
  group_by(age_group, type) %>%   # <-- important change
  slice_sample(n = 1) %>%         # 1 per type
  ungroup() %>%
  select(age_group, type, title, rating, listed_in)

recommendation_titles

# so for kids there were no or very few tv shows which didnt get included in our reccomendation

#Now I will do ggplot changes to color them
ggplot(top_countries,
       aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "#E50914") +
  coord_flip() +
  labs(
    title = "Top Countries Producing Netflix Content",
    x = "",
    y = "Number of Titles"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey25"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "#E50914",
                              size = 20,
                              face = "bold"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white")
  )


netflix_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.grid.major = element_line(color = "grey25"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "#E50914", size = 20, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 13),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )


top_5_countries <- netflix_clean %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 5)
top_5_countries <- netflix_clean %>%
  filter(country != "Unknown") %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 5)

ggplot(top_5_countries,
       aes(x = reorder(country, n),
           y = n)) +
  geom_col(fill = "#E50914", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 5 Netflix Countries",
    subtitle = "Countries with the most Netflix titles",
    x = "",
    y = ""
  ) +
  netflix_theme +
  theme(
    plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
  )

type_count <- netflix_clean %>%
  count(type)


ggplot(type_count,
       aes(x = 2, y = n, fill = type)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c(
    "#E50914",
    "#B81D24"
  )) +
  labs(
    title = "Movies vs TV Shows"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_blank(),
    plot.title = element_text(
      color = "#E50914",
      size = 18,
      face = "bold",
      hjust = 0.5
    )
  )



top_5_genres <- netflix_clean %>%
  count(listed_in, sort = TRUE) %>%
  slice_head(n = 5)


ggplot(top_5_genres,
       aes(x = reorder(listed_in, n),
           y = n)) +
  geom_col(fill = "#E50914", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 5 Netflix Genres",
    subtitle = "International Movies lead the platform",
    x = "",
    y = ""
  ) +
  netflix_theme


top_5_genres <- netflix_clean %>%
  count(listed_in, sort = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(
    listed_in = recode(
      listed_in,
      "International Movies" = "Intl. Movies",
      "Dramas, International Movies" = "Dramas Intl.",
      "Comedies, International Movies" = "Comedies Intl.",
      "Documentaries" = "Docs"
    )
  )

ggplot(top_5_genres,
       aes(x = reorder(listed_in, n),
           y = n)) +
  geom_col(fill = "#E50914", width = 0.6) +
  coord_flip() +
  labs(
    title = "Top 5 Netflix Genres",
    subtitle = "International Movies lead the platform",
    x = "",
    y = ""
  ) +
  netflix_theme +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 8),
    plot.margin = margin(t = 40, r = 20, b = 20, l = 20)
  )


library(tidyverse)

genre_counts <- netflix_clean %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE)

top_5_genres <- genre_counts %>%
  slice_head(n = 5)

ggplot(top_5_genres,
       aes(x = reorder(listed_in, n),
           y = n)) +
  geom_col(fill = "#E50914", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 5 Netflix Genres",
    subtitle = "Most common genres on Netflix",
    x = "",
    y = ""
  ) +
  netflix_theme

content_by_year <- netflix_clean %>%
  mutate(year_added = lubridate::year(date_added)) %>%
  count(year_added)

ggplot(content_by_year,
       aes(x = year_added,
           y = n)) +
  geom_line(
    color = "#E50914",
    linewidth = 2
  ) +
  geom_point(
    color = "#E50914",
    size = 3
  ) +
  labs(
    title = "Netflix's Rapid Expansion",
    subtitle = "Titles added per year",
    x = "",
    y = ""
  ) +
  netflix_theme


# Linear regression

# Growth of Nwtflix content over time

titles_per_year <- netflix_clean %>%
  filter(!is.na(release_year)) %>%
  count(release_year)

ggplot(titles_per_year, aes(x = release_year, y = n)) +
  geom_point(color = "#E50914", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#E50914") +
  labs(
    title = "Growth of Netflix Content Over Time",
    subtitle = "Linear regression showing the trend in released titles",
    x = "Release Year",
    y = "Number of Titles"
  ) +
  theme_minimal()

# Future Prediction using Linear Regression
model_titles <- lm(n ~ release_year, data = titles_per_year)

future_years <- data.frame(
  release_year = c(2022, 2023, 2024, 2025)
)

prediction_table <- future_years %>%
  mutate(
    predicted_titles = round(predict(model_titles, newdata = future_years), 0)
  )

prediction_table

model_titles <- lm(n ~ release_year, data = titles_per_year)

future_years <- data.frame(
  release_year = c(2022, 2023, 2024, 2025)
)

prediction_table <- future_years %>%
  mutate(
    predicted_titles = round(predict(model_titles, newdata = future_years), 0)
  )

prediction_table %>%
  knitr::kable(
    col.names = c("Year", "Predicted Titles"),
    align = "cc"
  )

prediction_table %>%
  knitr::kable(
    col.names = c("Year", "Predicted Titles"),
    align = "cc"
  ) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    font_size = 40
  )

# TOP PRODUCING COUNTRIES OVER TIME
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

country_year <- netflix_clean %>%
  filter(!is.na(country),
         country != "Unknown",
         !is.na(release_year)) %>%
  count(release_year, country, sort = TRUE) %>%
  group_by(release_year) %>%
  slice_max(n, n = 5) %>%
  ungroup()

ggplot(country_year,
       aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top Producing Countries Over Time: {closest_state}",
    subtitle = "Top 5 countries by number of Netflix titles per release year",
    x = "Country",
    y = "Number of Titles"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    plot.title = element_text(color = "#E50914", size = 20, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 12),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid.major = element_line(color = "grey30"),
    panel.grid.minor = element_blank()
  ) +
  transition_states(
    release_year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("cubic-in-out")

# NEW TRY
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)

top5_countries <- netflix_clean %>%
  filter(!is.na(country),
         country != "Unknown",
         country != "") %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(country)

country_year <- netflix_clean %>%
  filter(!is.na(country),
         country != "Unknown",
         country != "",
         country %in% top5_countries,
         !is.na(release_year),
         release_year >= 2000) %>%
  count(release_year, country)

ggplot(country_year,
       aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "#E50914") +
  coord_flip() +
  labs(
    title = "Top 5 Countries on Netflix",
    subtitle = "Release year: {closest_state}",
    x = "Country",
    y = "Number of Titles"
  ) +
  theme_minimal() +
  transition_states(
    release_year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("cubic-in-out")

# Here the code should be done. :)
