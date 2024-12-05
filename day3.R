library(tidyverse)
test <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
dat <- readLines("data/day3.txt")

# Part 1
tibble(
  raw = str_extract_all(dat, "mul\\([\\d]{1,3},[\\d]{1,3}\\)") |>
    unlist(),
  first = str_extract(raw,"(?<=\\()[\\d]{1,3}") |> as.numeric(),
  last = str_extract(raw,"[\\d]{1,3}(?=\\))") |> as.numeric(),
  multi = first*last
) |>
  summarise(tot = sum(multi))

# Part 2
raw = str_extract_all(dat, "mul\\([\\d]{1,3},[\\d]{1,3}\\)|do\\(\\)|don't\\(\\)") |>
  unlist()

tibble(raw) |>
  mutate(
    status = case_when(
      raw == "don't()" ~ F,
      raw == "do()" ~ T,
      row_number() ==1 ~ T
    ),
    num = str_detect(raw, "mul\\([\\d]{1,3},[\\d]{1,3}\\)"),
    first = str_extract(raw,"(?<=\\()[\\d]{1,3}") |> as.numeric(),
    last = str_extract(raw,"[\\d]{1,3}(?=\\))") |> as.numeric(),
    multi = first*last
  ) |>
  fill(status) |>
  filter(status, num) |>
  summarise(tot = sum(multi))
