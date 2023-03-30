#' @export
create_edit <- function(endpoint = "v1/edits",
                        model = "text-davinci-edit-001",
                        input = "",
                        instruction,
                        n = 1,
                        temperature = 1,
                        top_p = 1,
                        api_url = NULL,
                        api_key = NULL,
                        organization = NULL,
                        max_tries = 1,
                        timeout = 300) {
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
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response, CompletionResponse))
}
