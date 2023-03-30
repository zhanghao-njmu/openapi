#' @export
list_models <- function(endpoint = "v1/models",
                        api_url = NULL,
                        api_key = NULL,
                        organization = NULL,
                        max_tries = 1,
                        timeout = 300) {
  response <- making_requests(
    method = "GET",
    endpoint = endpoint,
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}

#' @export
retrieve_model <- function(endpoint = "v1/models",
                           model = "text-davinci-003",
                           api_url = NULL,
                           api_key = NULL,
                           organization = NULL,
                           max_tries = 1,
                           timeout = 300) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, model, sep = "/"),
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}
