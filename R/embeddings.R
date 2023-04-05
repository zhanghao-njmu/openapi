#' Create embeddings using OpenAI API
#'
#' This function uses the OpenAI API to create embeddings for the given input text.
#'
#' @param endpoint The API endpoint. Default: "v1/embeddings".
#' @param model The model used for generating embeddings. Default: "text-embedding-ada-002".
#' @param input The text input for which embeddings need to be generated.
#' @param user Optional user identifier.
#' @param api_url The OpenAI API URL. Default: NULL.
#' @param api_key Optional API key for authentication.
#' @param organization Optional organization identifier.
#' @param max_tries Maximum number of attempts to make the API request in case of failure. Default: 1.
#' @param timeout Timeout in seconds for the API request. Default: 300.
#'
#' @return An object of class "EmbeddingsResponse".
#' @export
create_embeddings <- function(endpoint = "v1/embeddings",
                              model = "text-embedding-ada-002",
                              input,
                              user = NULL,
                              api_url = NULL,
                              api_key = NULL,
                              organization = NULL,
                              max_tries = 1,
                              timeout = 300) {
  data <- list()
  data[["model"]] <- model
  data[["input"]] <- input
  data[["user"]] <- user

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response, EmbeddingsResponse))
}
