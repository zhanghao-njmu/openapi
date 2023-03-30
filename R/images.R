#' @export
create_image <- function(endpoint = "v1/images/generations",
                         prompt,
                         n = 1,
                         size = c("1024x1024", "512x512", "256x256"),
                         response_format = c("url", "b64_json"),
                         user = NULL,
                         api_url = NULL,
                         api_key = NULL,
                         organization = NULL,
                         max_tries = 1,
                         timeout = 300) {
  size <- match.arg(size)
  response_format <- match.arg(response_format)

  data <- list()
  data[["prompt"]] <- prompt
  data[["n"]] <- n
  data[["size"]] <- size
  data[["response_format"]] <- response_format
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
  return(parse_response(response, ImagesResponse))
}

#' @export
create_image_edit <- function(endpoint = "v1/images/edits",
                              image,
                              mask = NULL,
                              prompt,
                              n = 1,
                              size = c("1024x1024", "512x512", "256x256"),
                              response_format = c("url", "b64_json"),
                              user = NULL,
                              api_url = NULL,
                              api_key = NULL,
                              organization = NULL,
                              max_tries = 1,
                              timeout = 300) {
  size <- match.arg(size)
  response_format <- match.arg(response_format)

  data <- list()
  data[["image"]] <- httr::upload_file(image)
  if (!is.null(mask)) {
    data[["mask"]] <- httr::upload_file(mask)
  }
  data[["prompt"]] <- prompt
  data[["n"]] <- n
  data[["size"]] <- size
  data[["response_format"]] <- response_format
  data[["user"]] <- user

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
  return(parse_response(response, ImagesResponse))
}

#' @export
create_image_variation <- function(endpoint = "v1/images/variations ",
                                   image,
                                   n = 1,
                                   size = c("1024x1024", "512x512", "256x256"),
                                   response_format = c("url", "b64_json"),
                                   user = NULL,
                                   api_url = NULL,
                                   api_key = NULL,
                                   organization = NULL,
                                   max_tries = 1,
                                   timeout = 300) {
  size <- match.arg(size)
  response_format <- match.arg(response_format)

  data <- list()
  data[["image"]] <- httr::upload_file(image)
  data[["n"]] <- n
  data[["size"]] <- size
  data[["response_format"]] <- response_format
  data[["user"]] <- user

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
  return(parse_response(response, ImagesResponse))
}
