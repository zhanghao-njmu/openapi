#' Create a completion using the OpenAI API.
#'
#' @param endpoint The endpoint of the API (default is "v1/completions").
#' @param model The ID of the model to use.
#' @param prompt The prompt(s) to use.
#' @param suffix The suffix(es) to use.
#' @param max_tokens The maximum number of tokens to generate for each prompt.
#' @param temperature Controls the randomness of the generated tokens. Higher values means more randomness.
#' @param top_p Controls the diversity of the generated tokens.
#' @param n The number of completions to generate for each prompt.
#' @param stream Whether to use streaming to receive the response.
#' @param logprobs Whether to include the log probabilities for each token.
#' @param echo Whether to include the prompt and the completion together in the returned object.
#' @param stop The stopping sequence to use.
#' @param presence_penalty Control the degree to which model should avoid generating words that were already in the prompt.
#' @param frequency_penalty Control the degree to which model should avoid generating words which appeared frequently in the past texts.
#' @param best_of Return the N best completions
#' @param logit_bias A dictionary of logit bias values to add to the logits of the token.
#' @param user A unique identifier for the user requesting the completion.
#' @param api_url The API URL to use (override the default URL).
#' @param api_key The API key to use.
#' @param organization The organization ID to use.
#' @param max_tries The maximum number of times to try the request again in case of failure.
#' @param timeout The maximum amount of time to wait for the request to complete.
#' @param ... Additional arguments to pass to making_requests().
#'
#' @return Response object of class 'CompletionResponse'.
#' @export
create_completion <- function(endpoint = "v1/completions",
                              model = "text-davinci-003",
                              prompt = "<|endoftext|>",
                              suffix = NULL,
                              max_tokens = 2048,
                              temperature = 1,
                              top_p = 1,
                              n = 1,
                              stream = FALSE,
                              logprobs = NULL,
                              echo = FALSE,
                              stop = NULL,
                              presence_penalty = 0,
                              frequency_penalty = 0,
                              best_of = 1,
                              logit_bias = NULL,
                              user = NULL,
                              max_tries = 1,
                              timeout = 300,
                              ...) {
  data <- list()
  data[["model"]] <- model
  data[["prompt"]] <- prompt
  data[["suffix"]] <- suffix
  data[["max_tokens"]] <- max_tokens
  data[["temperature"]] <- temperature
  data[["top_p"]] <- top_p
  data[["n"]] <- n
  data[["stream"]] <- stream
  data[["logprobs"]] <- logprobs
  data[["echo"]] <- echo
  data[["stop"]] <- stop
  data[["presence_penalty"]] <- presence_penalty
  data[["frequency_penalty"]] <- frequency_penalty
  data[["best_of"]] <- best_of
  data[["logit_bias"]] <- logit_bias
  data[["user"]] <- user

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    stream = stream,
    stream_type = "completion",
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, CompletionResponse))
}
