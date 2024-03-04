library(tidyverse)

#' ## Problem
#'
#' We want to keep track of the state of a table at different points in time.
#' The table has a primary key `id` and a column `x` that we want to keep track of.
#' In this example, `V1` is the initial state of the table, `V2` is the state of the table after adding a row, `V3` is the state of the table after modifying a row, and `V4` is the state of the table after deleting a row.

V1 <- tibble(id = 1L, x = letters[1])
V1

# Adding a row
V2 <- tibble(id = 1:2, x = letters[1:2])
V2

# Modifying a row
V3 <- tibble(id = 1:2, x = letters[3:2])
V3

# Deleting a row
V4 <- tibble(id = 2L, x = letters[2])
V4

#' ## History table
#'
#' At each point in time, there is a table `H` that contains the history of the table `V` at that point in time.
#' The table `H` has columns `from` and `to` that define the time interval for which the row is valid.
#' The table `H` also contains the details from table `V` at that point in time.

H1 <- tibble(from = 1L, to = NA_integer_, V1)
H2 <- tibble(from = 1:2, to = NA_integer_, V2)
H3 <- tibble(
  from = 1:3,
  to = c(3L, NA_integer_, NA_integer_),
  bind_rows(V1[1, ], V3[2:1, ])
)
H4 <- tibble(
  from = 1:3,
  to = c(3L, NA_integer_, 4L),
  bind_rows(V1[1, ], V3[2:1, ])
)

#' With that, we can define a function `at_time_simple()` that takes a history table and a point in time, and returns the observation table at that point in time.
at_time_simple <- function(V, time) {
  V |>
    filter(coalesce(from <= !!time, TRUE), coalesce(to > !!time, TRUE)) |>
    select(-from, -to)
}

H1 |>
  at_time_simple(1) |>
  waldo::compare(V1)

H2 |>
  at_time_simple(2) |>
  waldo::compare(V2)

H2 |>
  at_time_simple(1) |>
  waldo::compare(V1)

H3 |>
  at_time_simple(3) |>
  waldo::compare(V3)

H3 |>
  at_time_simple(2) |>
  waldo::compare(V2)

H3 |>
  at_time_simple(1) |>
  waldo::compare(V1)

H4 |>
  at_time_simple(4) |>
  waldo::compare(V4)

H4 |>
  at_time_simple(3) |>
  waldo::compare(V3)

H4 |>
  at_time_simple(2) |>
  waldo::compare(V2)

H4 |>
  at_time_simple(1) |>
  waldo::compare(V1)

#' ## Decomposition
#'
#' The history tables can be decomposed into two tables: `O` (observation) and `D` (difference).
#' The observation table contains the details from the history table at the point in time, and is identical to the data at that point in time, save for the `from` and `to` columns.

O1 <- H1
O1 |>
  select(-from, -to) |>
  waldo::compare(V1)

D1 <- H2[0, ]

O2 <- H2
O2 |>
  select(-from, -to) |>
  waldo::compare(V2)

D2 <- H3[0, ]

O3 <- H3[3:2, ]
O3 |>
  select(-from, -to) |>
  waldo::compare(V3)

D3 <- H4[c(1, 3), ]

O4 <- H4[2, ]

O4 |>
  select(-from, -to) |>
  waldo::compare(V4)

#' Binding the an observed table and past history tables give a superset of the history table at that point in time.

bind_rows(O1) |>
  anti_join(H1, by = names(O1))

bind_rows(O2, D1) |>
  anti_join(H2, by = names(O2))

bind_rows(O3, D2, D1) |>
  anti_join(H3, by = names(O3))

bind_rows(O4, D3, D2, D1) |>
  anti_join(H4, by = names(O4))

#' This allows constructing a function `at_time()` that takes an observation table and a difference table, and returns the observation table at that point in time.
#' The only additional step is to keep the first row for each id.
#' If the order plays a role, the rows can be sorted by `id`.

at_time <- function(V, time) {
  V |>
    filter(coalesce(from <= !!time, TRUE), coalesce(to > !!time, TRUE)) |>
    # Keep the first row for each id
    distinct(id, .keep_all = TRUE) |>
    select(-from, -to) |>
    arrange(id)
}

O4 |>
  at_time(4)

bind_rows(O4, D3) |>
  at_time(3)

bind_rows(O4, D3, D2) |>
  at_time(2)

bind_rows(O4, D3, D2, D1) |>
  at_time(1)

#' ## Generalization
#'
#' Because observation and difference tables are a superset of history tables, combining, e.g., one observation table and two difference tables allows reconstructing the original data for three points in time in the past.

bind_rows(O4, D3, D2, D1) |>
  at_time(1) |>
  waldo::compare(V1)

bind_rows(O4, D3, D2, D1) |>
  at_time(2) |>
  waldo::compare(V2)

bind_rows(O4, D3, D2) |>
  at_time(2) |>
  waldo::compare(V2)

bind_rows(O4, D3, D2) |>
  at_time(3) |>
  waldo::compare(V3)

bind_rows(O4, D3) |>
  at_time(3) |>
  waldo::compare(V3)

bind_rows(O4, D3) |>
  at_time(4) |>
  waldo::compare(V4)
