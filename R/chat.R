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
