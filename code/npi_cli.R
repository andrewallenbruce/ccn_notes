explode <- function(x) {
  as.integer(
    unlist(
      strsplit(
        as.character(x),
        split = "",
        fixed = TRUE
      ), use.names = FALSE
    )
  )
}

npi_cli <- function(x) {
  
  if (!is.integer(x)) {
    x <- as.integer(x)
  }
  
  f <- \(...) paste0(..., collapse = ' ')
  f2 <- \(...) paste0(..., collapse = '')
  
  IDX <- c(1L, 3L, 5L, 7L, 9L)
  
  # Save the original check digit
  o <- x %% 10L
  
  # Create the "payload"
  p <- x %/% 10L
  cli::cli_alert("1. Create {.emph Payload}: {.strong {.val {p}}}")
  
  p <- cheapr::rev_(explode(p))
  cli::cli_alert("2. Reverse Digits: {.strong {.field {f2(p)}}}")
  
  p[IDX] <- p[IDX] * 2L
  cli::cli_alert("3. Double Every Other Digit: {.strong {.field {f(p)}}}")
  
  p[IDX] <- cheapr::if_else_(p[IDX] > 9L, p[IDX] - 9L, p[IDX])
  cli::cli_alert("4. Reduce Double Digits: {.strong {.field {f2(p)}}}")
  
  p <- sum(p) + 24L
  cli::cli_alert("5. Sum Digits & Add 24: {.strong {.field {f(p - 24, ' + 24 = ', p)}}}")
  
  CEIL <- cheapr::ceiling_(p / 10L) * 10L
  cli::cli_alert("6. Calculate Ceiling: {.strong {.field {f('\u2308', p, '\u2309', ' = ', CEIL)}}}")
  
  p <- as.integer(CEIL - p)
  cli::cli_alert("7. Compute Check Digit: {.strong {.field {f(p)}}}")
  
  cli::cli_alert("8. Compare: {.strong {.field {f(p, ' = ', o)}}}")
}
