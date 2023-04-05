#' Create image using OpenAI's DALL-E API
#'
#' This function creates images using OpenAI's DALL-E API by sending the prompt to the API and specifying the size, number of images to generate and response format.
#'
#' @param endpoint the DALL-E API endpoint to use. Default is "v1/images/generations".
#' @param prompt the prompt to send to the API. Required.
#' @param n the number of images to generate. Default is 1.
#' @param size the size of the image(s) to generate. Allowed values are "1024x1024", "512x512", and "256x256". Default is "1024x1024".
#' @param response_format the response format. Allowed values are "url" and "b64_json". Default is "url".
#' @param user the user to whom the API key belongs. Optional.
#' @param api_url the url of the DALL-E API server. Optional.
#' @param api_key the API key to use for the API. Required.
#' @param organization the organization to which the API key belongs. Optional.
#' @param max_tries maximum number of retries in case of a network error. Default is 1.
#' @param timeout time in seconds to wait for the API response. Default is 300 seconds.
#'
#' @return An object of class "ImagesResponse".
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

#' Create Image Edit
#'
#' Allows you to create an edited image by providing a raw image and a prompt that describes the edits you want to make. You can optionally include a mask image to control where the edits apply.
#'
#' @param endpoint endpoint for API request (default: "v1/images/edits")
#' @param image raw image data or a file path to the image.
#' @param mask raw image data or path to mask image.
#' @param prompt prompt describing edits to make to image.
#' @param n integer, number of images to generate (default: 1)
#' @param size dimensions of image, must be one of c("1024x1024", "512x512", "256x256") (default: "1024x1024")
#' @param response_format format for the response, must be one of c("url", "b64_json") (default: "url")
#' @param user user the request is associated with, if any.
#' @param api_url The API URL to query. The default value is "https://api.openai.com".
#' @param api_key Authentication key for API.
#' @param organization The name of the organization to use with this request. \code{api_key} must have permission to use this organization.
#' @param max_tries Maximum number of tries to make request.
#' @param timeout Maximum time to wait for response.
#'
#' @return An object of class "ImagesResponse".
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

#' Create Image Variation
#'
#' The function creates variations of an image based on size parameter and returns it in URL or base64 encoded JSON format.
#'
#' @param endpoint The API endpoint to use (default is \code{"v1/images/variations"}).
#' @param image The path to the image file.
#' @param n The number of variations to create (default is 1).
#' @param size A character string giving the desired size of the image variation (default is \code{"1024x1024"}). Possible values are \code{"1024x1024"}, \code{"512x512"}, and \code{"256x256"}.
#' @param response_format The format in which the image variation is returned (default is \code{"url"}). Possible values are \code{"url"} (returns the URL to the image variation) and \code{"b64_json"} (returns the base64 encoded JSON object to the image variation).
#' @param user The user making the request (default is NULL).
#' @param api_url The URL of the API (default is NULL).
#' @param api_key The API key to use for authentication (default is NULL).
#' @param organization The organization making the request (default is NULL).
#' @param max_tries The maximum number of times to try the request (default is 1).
#' @param timeout The timeout for the request in seconds (default is 300).
#'
#' @return An object of class "ImagesResponse".
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
