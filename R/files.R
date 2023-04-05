#' List Files
#'
#' A function to list files from an API endpoint.
#'
#' @param endpoint The API endpoint. Default is "v1/files".
#' @param api_url The URL of the OpenAI API (default: NULL)
#' @param api_key The API key for accessing the OpenAI API (default: NULL)
#' @param organization The organization associated with the API key (default: NULL)
#' @param max_tries The maximum number of times to attempt the API call (default: 1)
#' @param timeout The timeout value for the API call in seconds (default: 300)
#'
#' @export
list_files <- function(endpoint = "v1/files",
                       api_url = "https://api.openai.com",
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

#' Upload File to OpenAI API
#'
#' Uploads a file to the OpenAI API for fine-tuning models, among other purposes.
#'
#' @param endpoint The API endpoint to use (default: "v1/files")
#' @param file The file path to upload
#' @param purpose The intended purpose for uploading the file (default: "fine-tune")
#' @param api_url The URL of the OpenAI API (default: NULL)
#' @param api_key The API key for accessing the OpenAI API (default: NULL)
#' @param organization The organization associated with the API key (default: NULL)
#' @param max_tries The maximum number of times to attempt the API call (default: 1)
#' @param timeout The timeout value for the API call in seconds (default: 300)
#'
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

#' Delete a file from OpenAI API
#'
#' This function allows you to delete a file stored in OpenAI API.
#'
#' @param endpoint The API endpoint. Default is "v1/files".
#' @param file_id The ID of the file to be deleted.
#' @param purpose The purpose of the file. Default is "fine-tune".
#' @param api_url The OpenAI API URL. Default is "NULL.
#' @param api_key The OpenAI API key. By default is NULL.
#' @param organization The OpenAI organization. By default is NULL.
#' @param max_tries The maximum number of times to try the API request. Default is 1.
#' @param timeout The timeout in seconds for API requests. Default is 300 seconds.
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

#' Retrieve a file based on the file ID
#'
#' This function retrieves a specific file from the OpenAI API based on the file ID.
#' @inheritParams delete_file
#'
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

#' Retrieve File Content
#'
#' This function retrieves the content of a file from the OpenAI API.
#'
#' @inheritParams delete_file
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
