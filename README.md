
# historian

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Problem

We want to keep track of the state of a table at different points in
time. The table has a primary key `id` and a column `x` that we want to
keep track of. In this example, `V1` is the initial state of the table,
`V2` is the state of the table after adding a row, `V3` is the state of
the table after modifying a row, and `V4` is the state of the table
after deleting a row.

``` r
V1 <- tibble(id = 1L, x = letters[1])
V1
```

    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 a

``` r
# Adding a row
V2 <- tibble(id = 1:2, x = letters[1:2])
V2
```

    ## # A tibble: 2 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 a    
    ## 2     2 b

``` r
# Modifying a row
V3 <- tibble(id = 1:2, x = letters[3:2])
V3
```

    ## # A tibble: 2 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 c    
    ## 2     2 b

``` r
# Deleting a row
V4 <- tibble(id = 2L, x = letters[2])
V4
```

    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     2 b

## History table

At each point in time, there is a table `H` that contains the history of
the table `V` at that point in time. The table `H` has columns `from`
and `to` that define the time interval for which the row is valid. The
table `H` also contains the details from table `V` at that point in
time.

``` r
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
```

With that, we can define a function `at_time_simple()` that takes a
history table and a point in time, and returns the observation table at
that point in time.

``` r
at_time_simple <- function(V, time) {
  V |>
    filter(coalesce(from <= !!time, TRUE), coalesce(to > !!time, TRUE)) |>
    select(-from, -to) |>
    arrange(id)
}

H1 |>
  at_time_simple(1) |>
  waldo::compare(V1)
```

    ## ✔ No differences

``` r
H2 |>
  at_time_simple(2) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
H2 |>
  at_time_simple(1) |>
  waldo::compare(V1)
```

    ## ✔ No differences

``` r
H3 |>
  at_time_simple(3) |>
  waldo::compare(V3)
```

    ## ✔ No differences

``` r
H3 |>
  at_time_simple(2) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
H3 |>
  at_time_simple(1) |>
  waldo::compare(V1)
```

    ## ✔ No differences

``` r
H4 |>
  at_time_simple(4) |>
  waldo::compare(V4)
```

    ## ✔ No differences

``` r
H4 |>
  at_time_simple(3) |>
  waldo::compare(V3)
```

    ## ✔ No differences

``` r
H4 |>
  at_time_simple(2) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
H4 |>
  at_time_simple(1) |>
  waldo::compare(V1)
```

    ## ✔ No differences

## Decomposition

The history tables can be decomposed into two tables: `O` (observation)
and `D` (difference). The observation table contains the details from
the history table at the point in time, and is identical to the data at
that point in time, save for the `from` and `to` columns. The difference
table contains the changes that happened compared to the prior point in
time.

``` r
O1 <- H1
O1 |>
  select(-from, -to) |>
  waldo::compare(V1)
```

    ## ✔ No differences

``` r
D1 <- H2[0, ]

O2 <- H2
O2 |>
  select(-from, -to) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
D2 <- H3[1, ]

O3 <- H3[3:2, ]
O3 |>
  select(-from, -to) |>
  waldo::compare(V3)
```

    ## ✔ No differences

``` r
D3 <- H4[3, ]

O4 <- H4[2, ]

O4 |>
  select(-from, -to) |>
  waldo::compare(V4)
```

    ## ✔ No differences

Binding the an observed table and past history tables give a superset of
the history table at that point in time.

``` r
bind_rows(O1) |>
  distinct(id, .keep_all = TRUE) |>
  arrange(from, id) |>
  waldo::compare(H1)
```

    ## ✔ No differences

``` r
bind_rows(O2, D1) |>
  distinct(id, .keep_all = TRUE) |>
  arrange(from, id) |>
  waldo::compare(H2)
```

    ## ✔ No differences

``` r
bind_rows(O3, D2, D1) |>
  arrange(from, id) |>
  waldo::compare(H3)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3, D2, D1) |>
  arrange(from, id) |>
  waldo::compare(H4)
```

    ## ✔ No differences

This allows constructing a function `at_time()` that takes an
observation table and a difference table, and returns the observation
table at that point in time. The only additional step is to keep the
first row for each id. If the order plays a role, the rows can be sorted
by `id`.

``` r
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
```

    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     2 b

``` r
bind_rows(O4, D3) |>
  at_time(3)
```

    ## # A tibble: 2 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 c    
    ## 2     2 b

``` r
bind_rows(O4, D3, D2) |>
  at_time(2)
```

    ## # A tibble: 2 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 a    
    ## 2     2 b

``` r
bind_rows(O4, D3, D2, D1) |>
  at_time(1)
```

    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 a

## Generalization

Because observation and difference tables are a superset of history
tables, combining, e.g., one observation table and two difference tables
allows reconstructing the original data for three points in time in the
past.

``` r
bind_rows(O4, D3, D2, D1) |>
  at_time(1) |>
  waldo::compare(V1)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3, D2, D1) |>
  at_time(2) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3, D2) |>
  at_time(2) |>
  waldo::compare(V2)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3, D2) |>
  at_time(3) |>
  waldo::compare(V3)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3) |>
  at_time(3) |>
  waldo::compare(V3)
```

    ## ✔ No differences

``` r
bind_rows(O4, D3) |>
  at_time(4) |>
  waldo::compare(V4)
```

    ## ✔ No differences

## Updating observation and difference tables

How to construct `O4` and `D3` from `O3`, `D2`, `D1`, and `V4`? Same
question for constructing `O3` and `D2` from `O2`, `D1`, and `V3`? Or
for constructing `O2` and `D1` from `O1` and `V2`? Or for the
initialization, constructing `O1` from `V1`?

We know that we can reconstruct the history table from the observation
and difference tables. This then boils down to the question of how to
construct `O4` and `D3` from `O3`, `H3`, and `V4`.

``` r
O4
```

    ## # A tibble: 1 × 4
    ##    from    to    id x    
    ##   <int> <int> <int> <chr>
    ## 1     2    NA     2 b

``` r
D3
```

    ## # A tibble: 1 × 4
    ##    from    to    id x    
    ##   <int> <int> <int> <chr>
    ## 1     3     4     1 c

``` r
O3
```

    ## # A tibble: 2 × 4
    ##    from    to    id x    
    ##   <int> <int> <int> <chr>
    ## 1     3    NA     1 c    
    ## 2     2    NA     2 b

``` r
H3
```

    ## # A tibble: 3 × 4
    ##    from    to    id x    
    ##   <int> <int> <int> <chr>
    ## 1     1     3     1 a    
    ## 2     2    NA     2 b    
    ## 3     3    NA     1 c

``` r
V4
```

    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     2 b

We know how to extract `V3` from `O3`:

``` r
O3 |>
  at_time_simple(3) |>
  waldo::compare(V3)
```

    ## ✔ No differences

We then can compute the new or updated, and deleted rows. We also define
`V0` and `O0` as the empty tables.

``` r
V0 <- V1[0, ]
O0 <- O1[0, ]

compute_diff <- function(old, new) {
  # Contains both new and updated rows
  P <-
    new |>
    anti_join(old, by = names(new))

  # The id values of the deleted rows
  M <-
    old |>
    anti_join(new, by = "id") |>
    select(id)

  PM <-
    P |>
    select(id) |>
    bind_rows(M)

  list(P = P, M = M, PM = PM)
}

X4 <- compute_diff(V3, V4)
X4
```

    ## $P
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: id <int>, x <chr>
    ## 
    ## $M
    ## # A tibble: 1 × 1
    ##      id
    ##   <int>
    ## 1     1
    ## 
    ## $PM
    ## # A tibble: 1 × 1
    ##      id
    ##   <int>
    ## 1     1

``` r
X3 <- compute_diff(V2, V3)
X3
```

    ## $P
    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 c    
    ## 
    ## $M
    ## # A tibble: 0 × 1
    ## # ℹ 1 variable: id <int>
    ## 
    ## $PM
    ## # A tibble: 1 × 1
    ##      id
    ##   <int>
    ## 1     1

``` r
X2 <- compute_diff(V1, V2)
X2
```

    ## $P
    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     2 b    
    ## 
    ## $M
    ## # A tibble: 0 × 1
    ## # ℹ 1 variable: id <int>
    ## 
    ## $PM
    ## # A tibble: 1 × 1
    ##      id
    ##   <int>
    ## 1     2

``` r
X1 <- compute_diff(V0, V1)
X1
```

    ## $P
    ## # A tibble: 1 × 2
    ##      id x    
    ##   <int> <chr>
    ## 1     1 a    
    ## 
    ## $M
    ## # A tibble: 0 × 1
    ## # ℹ 1 variable: id <int>
    ## 
    ## $PM
    ## # A tibble: 1 × 1
    ##      id
    ##   <int>
    ## 1     1

The observation table is the same as the new table with `from` and `to`
set to the relevant points in time. For new and updated rows, `from` is
set to the current point in time; otherwise, the point in time from the
old observation table is used. Deleted rows must be removed from the
observation table.

``` r
X4$P |>
  mutate(from = 4L, to = NA_integer_, .before = 1) |>
  bind_rows(O3) |>
  distinct(id, .keep_all = TRUE) |>
  anti_join(X4$M, by = "id") |>
  arrange(id) |>
  waldo::compare(O4)
```

    ## ✔ No differences

``` r
X3$P |>
  mutate(from = 3L, to = NA_integer_, .before = 1) |>
  bind_rows(O2) |>
  distinct(id, .keep_all = TRUE) |>
  anti_join(X3$M, by = "id") |>
  arrange(id) |>
  waldo::compare(O3)
```

    ## ✔ No differences

``` r
X2$P |>
  mutate(from = 2L, to = NA_integer_, .before = 1) |>
  bind_rows(O1) |>
  distinct(id, .keep_all = TRUE) |>
  anti_join(X2$M, by = "id") |>
  arrange(id) |>
  waldo::compare(O2)
```

    ## ✔ No differences

``` r
X1$P |>
  mutate(from = 1L, to = NA_integer_, .before = 1) |>
  bind_rows(O0) |>
  distinct(id, .keep_all = TRUE) |>
  anti_join(X1$M, by = "id") |>
  arrange(id) |>
  waldo::compare(O1)
```

    ## ✔ No differences

The new difference table is the history table with the changed rows and
`to` set to the current point in time.

``` r
H3 |>
  semi_join(X4$PM, by = "id") |>
  filter(.by = id, row_number(from) == n()) |>
  mutate(to = 4L) |>
  waldo::compare(D3)
```

    ## ✔ No differences

``` r
H2 |>
  semi_join(X3$PM, by = "id") |>
  filter(.by = id, row_number(from) == n()) |>
  mutate(to = 3L) |>
  waldo::compare(D2)
```

    ## ✔ No differences

``` r
H1 |>
  semi_join(X2$PM, by = "id") |>
  filter(.by = id, row_number(from) == n()) |>
  mutate(to = 2L) |>
  waldo::compare(D1)
```

    ## ✔ No differences

The first observation table is the same as the first table with `from`
set to the first point in time and `to` set to missing.

``` r
V1 |>
  mutate(from = 1L, to = NA_integer_, .before = 1) |>
  waldo::compare(O1)
```

    ## ✔ No differences

This defines a process for efficiently maintaining the observation and
difference tables as new data arrives.