# TEST A 
library(readxl)
library(dplyr)

# Load data
lottery <- read_excel("All.Lottery.Results.xlsx")

# Rename columns
names(lottery) <- c(
  "no", "date", "x1", "x2", "x3", "x4", "x5", "x6",
  "bonus", "jackpot", "wins", "machine", "set"
)

# Clean draw number
lottery$draw_no <- as.numeric(gsub("[^0-9]", "", as.character(lottery$no)))

# Remove summary rows
lottery <- lottery %>%
  filter(!is.na(draw_no))

# Split datasets
haigh96 <- lottery %>%
  filter(draw_no >= 1 & draw_no <= 96)

lotto49 <- lottery %>%
  filter(draw_no >= 1 & draw_no <= 2065)

lotto59 <- lottery %>%
  filter(draw_no >= 2066)

# Check sizes
nrow(haigh96)
nrow(lotto49)
nrow(lotto59)

# Extract main numbers
nums96 <- unlist(haigh96[, c("x1", "x2", "x3", "x4", "x5", "x6")])
nums49 <- unlist(lotto49[, c("x1", "x2", "x3", "x4", "x5", "x6")])
nums59 <- unlist(lotto59[, c("x1", "x2", "x3", "x4", "x5", "x6")])

# Test A function
run_test_A <- function(numbers, M) {
  
  counts <- table(factor(numbers, levels = 1:M))
  
  D <- length(numbers) / 6
  m <- 6
  
  expected <- (m * D) / M
  
  W <- (M * (M - 1) / ((M - m) * D * m)) *
    (sum(counts^2) - ((m^2 * D^2) / M))
  
  p_value <- pchisq(W, df = M - 1, lower.tail = FALSE)
  
  list(
    draws = D,
    statistic = W,
    p_value = p_value,
    frequencies = data.frame(
      Number = 1:M,
      Observed = as.numeric(counts),
      Expected = round(expected, 2)
    )
  )
}

# Run Test A
A96 <- run_test_A(nums96, 49)
A49 <- run_test_A(nums49, 49)
A59 <- run_test_A(nums59, 59)

# Test A summary
test_A_summary <- data.frame(
  Dataset = c("First 96 draws", "6/49 era", "6/59 era"),
  Draws = c(A96$draws, A49$draws, A59$draws),
  Max_Number = c(49, 49, 59),
  Test_Statistic_W = round(c(A96$statistic, A49$statistic, A59$statistic), 4),
  P_Value = round(c(A96$p_value, A49$p_value, A59$p_value), 4)
)

test_A_summary

# Frequency tables
A96$frequencies
A49$frequencies
A59$frequencies


# TEST B
# Make sure datasets are in chronological order

haigh96 <- lottery %>%
  filter(draw_no >= 1 & draw_no <= 96) %>%
  arrange(draw_no)

lotto49 <- lottery %>%
  filter(draw_no >= 1 & draw_no <= 2065) %>%
  arrange(draw_no)

lotto59 <- lottery %>%
  filter(draw_no >= 2066) %>%
  arrange(draw_no)
run_test_B <- function(draw_data, M) {
  
  draws <- draw_data[, c("x1", "x2", "x3", "x4", "x5", "x6")]
  
  all_gaps <- c()
  
  for (i in 1:M) {
    
    positions <- which(apply(draws, 1, function(row) i %in% row))
    
    if (length(positions) > 0) {
      
      first_gap <- positions[1]
      later_gaps <- diff(positions)
      
      all_gaps <- c(all_gaps, first_gap, later_gaps)
    }
  }
  
  observed <- table(all_gaps)
  
  gap_sizes <- as.numeric(names(observed))
  
  probabilities <- ((M - 6) / M)^(gap_sizes - 1) * (6 / M)
  
  expected <- probabilities * sum(observed)
  
  data.frame(
    Gap_Size = gap_sizes,
    Observed = as.numeric(observed),
    Expected = round(expected, 2)
  )
}
B96 <- run_test_B(haigh96, 49)
B96
B49 <- run_test_B(lotto49, 49)
B49
B59 <- run_test_B(lotto59, 59)
B59
# TEST C — Sum of the Numbers

run_test_C <- function(draw_data, M) {
  
  # Number of balls drawn
  m <- 6
  
  # Calculate sums for each draw
  sums <- rowSums(
    draw_data[, c("x1", "x2", "x3", "x4", "x5", "x6")],
    na.rm = TRUE
  )
  
  # Number of draws
  D <- length(sums)
  
  # Theoretical mean
  mu <- m * (M + 1) / 2
  
  # Theoretical variance
  sigma2 <- m * (M + 1) * (M - m) / 12
  
  # Theoretical standard deviation
  sigma <- sqrt(sigma2)
  
  # Sample mean and variance
  U <- mean(sums)
  V <- var(sums)
  
  # Test statistics
  Z <- (U - mu) / (sigma / sqrt(D))
  
  chi_stat <- ((D - 1) * V) / sigma2
  
  # p-values
  p_value_Z <- 2 * (1 - pnorm(abs(Z)))
  
  p_value_chi <- pchisq(
    chi_stat,
    df = D - 1,
    lower.tail = FALSE
  )
  
  # Results
  return(list(
    sums = sums,
    theoretical_mean = mu,
    sample_mean = U,
    theoretical_variance = sigma2,
    sample_variance = V,
    Z_statistic = Z,
    Z_p_value = p_value_Z,
    chi_square = chi_stat,
    chi_p_value = p_value_chi
  ))
}

# Run Test C
C96 <- run_test_C(haigh96, 49)

C49 <- run_test_C(lotto49, 49)

C59 <- run_test_C(lotto59, 59)

# View results
C96
C49
C59


# TEST D — Odd-Even Combinations

run_test_D <- function(draw_data, M) {
  
  # Extract lottery numbers
  draws <- draw_data[, c("x1", "x2", "x3", "x4", "x5", "x6")]
  
  # Count even numbers in each draw
  even_counts <- apply(draws, 1, function(row) {
    sum(row %% 2 == 0)
  })
  
  # Observed frequencies
  observed <- table(factor(even_counts, levels = 0:6))
  
  # Number of even and odd numbers
  even_total <- floor(M / 2)
  odd_total <- M - even_total
  
  # Expected probabilities (hypergeometric)
  expected_probs <- dhyper(
    0:6,
    even_total,
    odd_total,
    6
  )
  
  # Expected frequencies
  expected <- expected_probs * length(even_counts)
  
  # Chi-square statistic
  chi_square <- sum((observed - expected)^2 / expected)
  
  # p-value
  p_value <- pchisq(
    chi_square,
    df = 6,
    lower.tail = FALSE
  )
  
  # Results table
  results <- data.frame(
    Even_Numbers = 0:6,
    Observed = as.numeric(observed),
    Expected = round(expected, 2)
  )
  
  return(list(
    table = results,
    chi_square = chi_square,
    p_value = p_value
  ))
}

# Run Test D
D96 <- run_test_D(haigh96, 49)

D49 <- run_test_D(lotto49, 49)

D59 <- run_test_D(lotto59, 59)

# View results
D96$table
D49$table
D59$table

D96$chi_square
D96$p_value

D49$chi_square
D49$p_value

D59$chi_square
D59$p_value

# TABLE 4 — Haigh-style analysis

# Load ticket sales data
sales <- read_excel("Lottery ticket sales.xlsx", skip = 1)

# Keep date and sales columns only
sales <- sales[, 1:2]
names(sales) <- c("date", "ticket_sales")

# Clean dates and sales
clean_date <- function(x) {
  x <- gsub("\u00A0", " ", as.character(x))
  x <- trimws(x)
  as.Date(x, format = "%a %d %b %Y")
}

sales <- sales %>%
  mutate(
    date_clean = clean_date(date),
    ticket_sales = as.numeric(gsub("[^0-9]", "", as.character(ticket_sales)))
  ) %>%
  filter(!is.na(date_clean), !is.na(ticket_sales))

# Clean date in lottery data
lottery <- lottery %>%
  mutate(date_clean = clean_date(date))

# Merge ticket sales into lottery results
lottery_sales <- lottery %>%
  left_join(sales, by = "date_clean")

# Re-split eras using same method as before
haigh96_sales <- lottery_sales %>%
  filter(draw_no >= 1 & draw_no <= 96) %>%
  arrange(draw_no)

lotto49_sales <- lottery_sales %>%
  filter(draw_no >= 1 & draw_no <= 2065) %>%
  arrange(draw_no)

lotto59_sales <- lottery_sales %>%
  filter(draw_no >= 2066) %>%
  arrange(draw_no)

# Check missing ticket sales
sum(is.na(haigh96_sales$ticket_sales))
sum(is.na(lotto49_sales$ticket_sales))
sum(is.na(lotto59_sales$ticket_sales))

make_table4 <- function(draw_data, M, high_cutoff, max_sep_cutoff) {
  
  draws <- draw_data[, c("x1", "x2", "x3", "x4", "x5", "x6")]
  draws <- as.data.frame(lapply(draws, as.numeric))
  
  winners <- as.numeric(draw_data$wins)
  tickets <- as.numeric(draw_data$ticket_sales)
  
  valid <- !is.na(winners) & !is.na(tickets)
  draws <- draws[valid, ]
  winners <- winners[valid]
  tickets <- tickets[valid]
  
  total_combinations <- choose(M, 6)
  
  expected_winners <- tickets / total_combinations
  
  min_sep <- apply(draws, 1, function(x) {
    x <- sort(x)
    min(diff(x))
  })
  
  max_sep <- apply(draws, 1, function(x) {
    x <- sort(x)
    gaps <- c(x[1], diff(x), M + 1 - x[6])
    max(gaps)
  })
  
  high_numbers <- apply(draws, 1, function(x) {
    sum(x > high_cutoff) >= 2
  })
  
  make_row <- function(label, condition) {
    
    freq <- sum(condition)
    actual <- sum(winners[condition])
    expected <- sum(expected_winners[condition])
    
    data.frame(
      Criterion = label,
      Frequency_Occurred = freq,
      Actual_Number_Sharing_Jackpot = actual,
      Expected_Number_Sharing_Jackpot = round(expected, 2),
      Ratio = round(actual / expected, 2)
    )
  }
  
  rows <- list()
  
  for (k in sort(unique(min_sep))) {
    rows[[length(rows) + 1]] <- make_row(
      paste0("m(t) = ", k),
      min_sep == k
    )
  }
  
  rows[[length(rows) + 1]] <- make_row(
    "Total",
    rep(TRUE, length(winners))
  )
  
  rows[[length(rows) + 1]] <- make_row(
    paste0("At least two exceed ", high_cutoff),
    high_numbers
  )
  
  rows[[length(rows) + 1]] <- make_row(
    paste0("Maximum separation at least ", max_sep_cutoff),
    max_sep >= max_sep_cutoff
  )
  
  do.call(rbind, rows)
}

table4_96 <- make_table4(
  haigh96_sales,
  M = 49,
  high_cutoff = 40,
  max_sep_cutoff = 24
)

table4_49 <- make_table4(
  lotto49_sales,
  M = 49,
  high_cutoff = 40,
  max_sep_cutoff = 24
)

table4_59 <- make_table4(
  lotto59_sales,
  M = 59,
  high_cutoff = 50,
  max_sep_cutoff = 29
)

table4_96
table4_49
table4_59
