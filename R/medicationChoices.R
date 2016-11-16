processMedicationChoiceAnswers <- function(json_file) {
  tryCatch({
    d <- jsonlite::fromJSON(json_file)
    data.frame(medication = paste(unique(d$identifier),
                                  collapse = ", "), medicationTime = paste(unique(d$answer),
                                                                          collapse = ", "))
  }, error = function(err) {
    data.frame(medication = "NA", medicationTime = "NA")
  })
}

