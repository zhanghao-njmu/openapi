#' @export
create_moderation <- function(endpoint = "v1/moderations",
                              input,
                              model = "text-moderation-latest",
                              api_url = NULL,
                              api_key = NULL,
                              organization = NULL,
                              max_tries = 1,
                              timeout = 300) {
  data <- list()
  data[["input"]] <- input
  data[["model"]] <- model

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
  return(parse_response(response))
}
