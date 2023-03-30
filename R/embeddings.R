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
