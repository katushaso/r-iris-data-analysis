# номер 19, вариант 1
library(tidyverse)

# 0
data("iris")

# 1.1
iris_data <- iris %>%
  rename(
    sepal_length = Sepal.Length,
    sepal_width = Sepal.Width,
    petal_length = Petal.Length,
    petal_width = Petal.Width,
    species = Species
  )

# 1.2
iris_data <- iris_data %>%
  mutate(
    species = as.factor(species)
  )

# 1.3
iris_data <- iris_data %>%
  mutate(
    sepal_ratio = sepal_length / sepal_width,
    petal_area = petal_length * petal_width
  )

# 1.4
iris_data <- iris_data %>%
  arrange(desc(sepal_ratio))

# 1.5
iris_data <- iris_data %>%
  mutate(
    id = row_number()
  )

# 2.1
iris_summary <- iris_data %>%
  group_by(species) %>%
  summarise(
    across(
      c(
        sepal_length,
        sepal_width,
        petal_length,
        petal_width,
        sepal_ratio,
        petal_area
      ),
      list(
        mean = ~ mean(.x),
        sd = ~ sd(.x),
        min = ~ min(.x),
        max = ~ max(.x)
      )
    )
  )

# 2.2
iris_summary <- iris_summary %>%
  left_join(
    iris_data %>%
      count(species, name = "count"),
    by = "species"
  )

# 3.1
iris_long <- iris_data %>%
  pivot_longer(
    cols = c(
      sepal_length,
      sepal_width,
      petal_length,
      petal_width,
      sepal_ratio,
      petal_area
    ),
    names_to = "measurement",
    values_to = "value"
  ) %>%
  select(id, species, measurement, value)

# 3.2
iris_long_summary <- iris_long %>%
  group_by(species, measurement) %>%
  summarise(
    mean_value = mean(value),
    .groups = "drop"
  )

# 3.3
iris_wide <- iris_long_summary %>%
  pivot_wider(
    names_from = species,
    values_from = mean_value
  )

# 4.1
iris_filtered <- iris_data %>%
  filter(
    sepal_length > 3,
    sepal_width > 2,
    petal_length > 3
  )

# 4.2
iris_max_sepal <- iris_data %>%
  group_by(species) %>%
  slice_max(
    order_by = sepal_length,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

# 4.3
iris_comparison <- iris_data %>%
  group_by(species) %>%
  mutate(
    sepal_length_comparison = if_else(
      sepal_length > mean(sepal_length),
      "больше среднего",
      "меньше или равно среднему"
    )
  ) %>%
  ungroup()
