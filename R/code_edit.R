#' @importFrom diffr diffr
#' @export
compare_text <- function(text1, text2) {
  file1 <- tempfile()
  file2 <- tempfile()
  on.exit({
    unlink(file1)
    unlink(file2)
  })
  writeLines(text1, con = file1)
  writeLines(text2, con = file2)
  comparison <- diffr(file1 = file1, file2 = file2, before = "Input", after = "Output")
  return(comparison)
}

#' @export
code_edit <- function(code, prompt, instruction, explain = FALSE, ...) {
  messages <- list(
    list(
      "role" = "system",
      "content" = paste0(prompt, if (isFALSE(explain)) (" Just give me the code without any explanation.") else NULL)
    ),
    list(
      "role" = "user",
      "content" = paste0(instruction, if (isFALSE(explain)) " Return in standard R code format without any explanation." else NULL, " Code:\n\n", code, "\n\n")
    )
  )
  response <- create_chat_completion(messages = messages, ...)
  choice <- response$extract("choices")[1]
  comparison <- compare_text(text1 = code, text2 = choice)
  result <- new("TextCompletion",
    response = response,
    input = code, output = choice,
    difference = comparison
  )
  return(result)
}

#' @export
code_document <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Generate complete roxygen2 documentation for R code.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_check <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Check if there are any issues with the following R code.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_improve <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Rewrite the following R code to improve its efficiency.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_comment <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Add inline comments to the following R code to improve its readability.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_refactor <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Review the following R code and refactor it to improve its efficiency and readability.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_explain <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Review the following R code and provide an explanation of how it works.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}

#' @export
code_create_test <- function(code, explain = TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0("Generate test units for the following R code using the testthat 3e package.")
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, ...)
  return(result)
}
