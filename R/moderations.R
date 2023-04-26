#' @export
create_moderation <- function(endpoint = "moderations",
                              input,
                              model = "text-moderation-latest",
                              max_tries = 1,
                              timeout = 300,
                              ...) {
  data <- list()
  data[["input"]] <- input
  data[["model"]] <- model

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}
