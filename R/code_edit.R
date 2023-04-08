#' Compare Two Texts
#'
#' Compares two texts and returns a difference report.
#'
#' @param text1,text2 Character vectors of text to be compared.
#' @return A difference report between the two texts.
#' @importFrom diffr diffr
#' @export
#' @examples
#' text1 <- "Lorem ipsum dolor sit amet."
#' text2 <- "Lorem ipsum sit amet."
#' compare_text(text1, text2)
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

#' Edit R code with a user prompt and system message
#'
#' This function returns a text editing object that allows the user to edit R code based on a prompt and message from the system. The user can choose to receive explanation or not. The edited code is then compared to the original code and the differences are returned.
#'
#' @param code R code to be edited
#' @param prompt System prompt message, character vector
#' @param instruction User instruction message, character vector
#' @param explain Logical, indicating whether to include explanation messages
#' @param ... Additional arguments to pass to \code{\link{create_chat_completion}}
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
#'
code_edit <- function(code, prompt, instruction, explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  if (isTRUE(explain)) {
    instruction <- paste0(instruction, " The output should be in standard R code format with the explanation.")
  } else {
    instruction <- paste0(instruction, " Do not write explanations.")
  }
  if (isTRUE(simplify)) {
    instruction <- paste0(instruction, " Do not provide me with any R code that does not require modification.")
  }
  messages <- list(
    list(
      "role" = "system",
      "content" = prompt
    ),
    list(
      "role" = "user",
      "content" = paste0(instruction, " Code:\n\n", code, "\n\n")
    )
  )
  file <- tempfile()
  on.exit({
    unlink(file)
  })
  response <- create_chat_completion(messages = messages, stream = TRUE, stream_file = file, ...)
  return(TextEditing$new(input = code, response = response))
}

#' Generate complete roxygen2 documentation for R code.
#'
#' @inheritParams code_edit
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_document <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Generate complete roxygen2 documentation for R code, including title, description, parameters, returns, examples, and more.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Check R code for issues
#'
#' This function checks R code for any issues present in the code.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_check <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Check if there are any issues or bugs with the following R code.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Improve R code efficiency
#'
#' This function takes R code as input and edits it to improve its efficiency.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_improve <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Rewrite the following R code to improve its efficiency.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Add inline comments to the following R code to improve its readability.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_comment <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Add inline comments to the following R code to improve its readability.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Refactor R code for efficiency and readability
#'
#' This function takes in R code and refactors it to improve its efficiency and readability.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_refactor <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Review the following R code and refactor it to improve its efficiency and readability.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Explain R code
#'
#' Review the following R code and provide an explanation of how it works.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_explain <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Review the following R code and provide an explanation of how it works.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}

#' Create Test Units for R Code
#'
#' Given a code snippet, generates test units using the testthat package.
#'
#' @inheritParams code_edit
#'
#' @return A \code{\link{TextEditing-class}} object
#'
#' @export
code_create_test <- function(code, additional_instructions = getOption("openapi_additional_instructions"), explain = getOption("openapi_explain") %||% TRUE, simplify = getOption("openapi_simplify") %||% TRUE, ...) {
  prompt <- prompts[prompts[["act"]] == "R Package Development Assistant", "prompt"]
  instruction <- paste0(
    "Generate test units for the following R code using the testthat 3e package.",
    additional_instructions
  )
  result <- code_edit(code = code, prompt = prompt, instruction = instruction, explain = explain, simplify = simplify, ...)
  return(result)
}
