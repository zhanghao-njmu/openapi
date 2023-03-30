#' @export
list_files <- function(endpoint = "v1/files",
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
upload_file <- function(endpoint = "v1/files",
                        file,
                        purpose = "fine-tune",
                        api_url = NULL,
                        api_key = NULL,
                        organization = NULL,
                        max_tries = 1,
                        timeout = 300) {
  data <- list()
  data[["file"]] <- httr::upload_file(file)
  data[["purpose"]] <- purpose

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    encode = "multipart",
    post_type = "multipart/form-data",
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}

#' @export
delete_file <- function(endpoint = "v1/files",
                        file_id,
                        purpose = "fine-tune",
                        api_url = NULL,
                        api_key = NULL,
                        organization = NULL,
                        max_tries = 1,
                        timeout = 300) {
  response <- making_requests(
    method = "DELETE",
    endpoint = paste(endpoint, file_id, sep = "/"),
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}

#' @export
retrieve_file <- function(endpoint = "v1/files",
                          file_id,
                          api_url = NULL,
                          api_key = NULL,
                          organization = NULL,
                          max_tries = 1,
                          timeout = 300) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, file_id, sep = "/"),
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}

#' @export
retrieve_file_content <- function(endpoint = "v1/files",
                                  file_id,
                                  api_url = NULL,
                                  api_key = NULL,
                                  organization = NULL,
                                  max_tries = 1,
                                  timeout = 300) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, file_id, "content", sep = "/"),
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}
