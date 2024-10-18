test_that("state_to_abb works", {
  df <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "MA"),
    "State_misspell" = c("RI", "foo")
  )

  df2 <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_name" = c("RI", "MA"),
    "State_mix" = c("RI", "MA"),
    "State_misspell" = c("RI", "foo")
  )

  df <- state_to_abb(df, "State_abb")
  df <- state_to_abb(df, "State_name")
  df <- state_to_abb(df, "State_mix")
  df <- state_to_abb(df, "State_misspell")

  expect_equal(df, df2)
})

test_that("state_to_name works", {
  df <- data.frame(
    "State_abb" = c("RI", "MA"),
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "MA"),
    "State_misspell" = c("RI", "foo")
  )

  df2 <- data.frame(
    "State_abb" = c("Rhode Island", "Massachusetts"),
    "State_name" = c("Rhode Island", "Massachusetts"),
    "State_mix" = c("Rhode Island", "Massachusetts"),
    "State_misspell" = c("Rhode Island", "foo")
  )

  df <- state_to_name(df, "State_abb")
  df <- state_to_name(df, "State_name")
  df <- state_to_name(df, "State_mix")
  df <- state_to_name(df, "State_misspell")

  expect_equal(df, df2)
})

