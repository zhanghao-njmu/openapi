#' Create chat completions using the OpenAI API.
#'
#' This function allows users to generate a response to a given conversation history using OpenAI's API.
#'
#' @param endpoint Endpoint for the OpenAI API. Default is "v1/chat/completions".
#' @param model ID of the GPT-3 model to be used. Default is "gpt-3.5-turbo".
#' @param messages A string or a list of strings representing the conversation history.
#' @param temperature Controls the degree of randomness of the response. Default is 1.
#' @param top_p Controls the number of tokens to consider for each response. Default is 1.
#' @param n Number of completions to generate. Default is 1.
#' @param stream Logical, whether to use websocket or not for communicating with OpenAI API. Default is FALSE.
#' @param stop One or more strings where each string represents a sequence at which the response should end.
#' @param max_tokens Controls the maximum number of tokens to generate in the response. Default is NULL (unrestricted).
#' @param presence_penalty Controls the degree to which the response is influenced by the presence of certain words in the conversation history. Default is 0.
#' @param frequency_penalty Controls the degree to which the response is influenced by the frequency of certain words in the conversation history. Default is 0.
#' @param logit_bias Offset the logits that OpenAI calculates for each token. Default is NULL.
#' @param user A string identifying the user whose conversation history is being used. Default is NULL.
#' @param api_url URL to the OpenAI API. Can be set to a custom endpoint. Default is NULL.
#' @param api_key API key for authentication with the OpenAI API. Default is NULL.
#' @param organization Organization ID for authentication with the OpenAI API. Default is NULL.
#' @param max_tries The maximum number of attempts to make for a single API call. Default is 1.
#' @param timeout The maximum time in seconds to wait for a response from the OpenAI API. Default is 300.
#' @param ... Additional arguments to be passed to 'making_requests()' function.
#' @return Response object of class 'CompletionResponse'.
#' @export
create_chat_completion <- function(endpoint = "v1/chat/completions",
                                   model = "gpt-3.5-turbo",
                                   messages,
                                   temperature = 1,
                                   top_p = 1,
                                   n = 1,
                                   stream = FALSE,
                                   stop = NULL,
                                   max_tokens = NULL,
                                   presence_penalty = 0,
                                   frequency_penalty = 0,
                                   logit_bias = NULL,
                                   user = NULL,
                                   api_url = NULL,
                                   api_key = NULL,
                                   organization = NULL,
                                   max_tries = 1,
                                   timeout = 300,
                                   ...) {
  data <- list()
  data[["model"]] <- model
  data[["messages"]] <- messages
  data[["temperature"]] <- temperature
  data[["top_p"]] <- top_p
  data[["n"]] <- n
  data[["stream"]] <- stream
  data[["stop"]] <- stop
  data[["max_tokens"]] <- max_tokens
  data[["presence_penalty"]] <- presence_penalty
  data[["frequency_penalty"]] <- frequency_penalty
  data[["logit_bias"]] <- logit_bias
  data[["user"]] <- user

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    stream = stream,
    stream_type = "chat_completion",
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, CompletionResponse))
}
