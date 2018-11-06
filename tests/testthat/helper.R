# Shared test functions

### check for error dataframe
is_error_dataframe <- function(df) {
  return(rlang::has_name(df, "error") && !is.na(df$error))
}
