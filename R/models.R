#' List Models
#'
#' Retrieves a list of available models from OpenAI API.
#'
#' @param endpoint The endpoint for the API request. Default is "models"
#' @param api_base The base URL of the OpenAI API. Default is NULL.
#' @param api_key Your API key for authentication. Default is NULL.
#' @param organization Organization ID for using resources from a specific organization. Default is NULL.
#' @param max_tries Maximum number of attempts for making the request. Default is 1.
#' @param timeout Timeout limit for the API request in seconds. Default is 300.
#'
#' @return Response object of class 'ModelsResponse'.
#' @export
list_models <- function(endpoint = "models",
                        max_tries = 1,
                        timeout = 300,
                        ...) {
  response <- making_requests(
    method = "GET",
    endpoint = endpoint,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, ModelsResponse))
}

#' Retrieve OpenAI Model
#'
#' Retrieves details about a specific OpenAI model.
#'
#' @param endpoint a character string indicating the endpoint, default is "models"
#' @param model a character string indicating the model name, default is "text-davinci-003"
#' @param api_base a character string indicating the OpenAI API URL, default is NULL
#' @param api_key a character string for the OpenAI API key, default is NULL
#' @param organization a character string indicating the API organization, default is NULL
#' @param max_tries an integer value indicating maximum number of tries, default is 1
#' @param timeout a numeric value indicating request timeout in seconds, default is 300
#'
#' @return Response object of class 'ModelsResponse'.
#' @export
retrieve_model <- function(endpoint = "models",
                           model = "text-davinci-003",
                           max_tries = 1,
                           timeout = 300,
                           ...) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, model, sep = "/"),
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, ModelsResponse))
}
