# run this code by chunks to get familiar with chaining (%>%) and dplyr
# iris is a data set available in R; no need to read in anything


library(dplyr)

# the beginning: save the head of the iris dataset into a variable called 'data'
data = head(iris); data

# select just the columns you want
data = head(iris) %>%
  select(Species, Sepal.Length, Sepal.Width); data

# chained functions can also be on multiple lines for easier reading
data = head(iris) %>%
  select(Species,
         Sepal.Length,
         Sepal.Width); data

# can also rename within the select function
data = head(iris) %>%
  select(species = Species,
         sepal_length = Sepal.Length,
         sepal_width = Sepal.Width); data


# style: it's easier to read if the '=' symbols line up
data = head(iris) %>%
  select(species      = Species,
         sepal_length = Sepal.Length,
         sepal_width  = Sepal.Width); data

# mutate: add a new column that is based on other columns
data = head(iris) %>%
  select(species      = Species,
         sepal_length = Sepal.Length,
         sepal_width  = Sepal.Width) %>%
  mutate(sepal_area   = sepal_length * sepal_width); data


# mutate: add a new column that is all the same value
data = head(iris) %>%
  select(species      = Species,
         sepal_length = Sepal.Length,
         sepal_width  = Sepal.Width) %>%
  mutate(sepal_area   = sepal_length * sepal_width,
         observer     = 'Professor Jones'); data


# arrange: order by the largest area (descending) #default is ascending
data = head(iris) %>%
  select(species      = Species,
         sepal_length = Sepal.Length,
         sepal_width  = Sepal.Width) %>%
  mutate(sepal_area   = sepal_length * sepal_width,
         observer     = 'Professor Jones') %>%
  arrange(desc(sepal_area)); data



