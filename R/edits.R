#' Create/Edit Text using OpenAI API
#'
#' This function allows you to generate or edit text using the OpenAIAPI.
#'
#' @param endpoint A character string indicating the endpoint for the OpenAI API. Default is "edits".
#' @param model A character string describing the model to be used for text generation. Default is "text-davinci-edit-001".
#' @param input A character string representing the input text to be used as the basis for edits or continuation. Default is "".
#' @param instruction A character string representing the edit or continuation to be made to the input text.
#' @param n An integer indicating how many completions to generate for the input. Default is 1.
#' @param temperature A numeric value indicating the "creativity" of the generated text. Values closer to 0 produce more conservative text, values closer to 1 more creative text. Default is 1.
#' @param top_p A numeric value indicating the likelihood that the next word in the generated text will be among the most likely candidates. Values closer to 0 are more conservative, values closer to 1 are more creative. Default is 1.
#' @param api_base A character string representing the URL for the OpenAI API. Default is NULL.
#' @param api_key A character string representing the API key. Default is NULL.
#' @param organization A character string representing the name of the organization associated with the API key. Default is NULL.
#' @param max_tries An integer indicating the maximum number of attempts to make a successful API call. Default is 1.
#' @param timeout An integer indicating the maximum wait time in seconds for the API to return a response. Default is 300.
#' @return  Response object of class 'CompletionResponse'.
#' @export
create_edit <- function(endpoint = "edits",
                        model = "text-davinci-edit-001",
                        input = "",
                        instruction,
                        n = 1,
                        temperature = 1,
                        top_p = 1,
                        max_tries = 1,
                        timeout = 300,
                        ...) {
  data <- list()
  data[["model"]] <- model
  data[["input"]] <- input
  data[["instruction"]] <- instruction
  data[["n"]] <- n
  data[["top_p"]] <- top_p

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, CompletionResponse))
}
