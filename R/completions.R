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
                              api_url = NULL,
                              api_key = NULL,
                              organization = NULL,
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
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, CompletionResponse))
}
