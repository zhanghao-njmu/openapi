#' Making Requests Function
#'
#' This function allows users to make requests to OpenAI's API endpoint using various http methods.
#'
#' @param method A character string indicating the http method to be used (default is "POST").
#' @param endpoint A character string indicating the endpoint to which the request should be sent (default is "v1/chat/completions").
#' @param data A list object containing the data to be sent with the request (default is NULL).
#' @param encode A character string specifying the encoding type for the data to be sent (default is "json").
#' @param stream A logical value indicating whether the response should be streamed or not (default is FALSE).
#' @param stream_type A character string specifying the type of stream content to expect in the response (default is "completion").
#' @param stream_file A character string indicating the file path to save the streamed content to (default is NULL).
#' @param post_type A character string specifying the content type of the request body (default is "application/json").
#' @param response_type A character string specifying the content type of the response body (default is "application/json").
#' @param api_url A character string indicating the OpenAI API endpoint URL (default is NULL).
#' @param api_key A character string indicating the user's API key (default is NULL).
#' @param organization A character string indicating the name of the OpenAI organization being accessed (default is NULL).
#' @param max_tries An integer specifying the maximum number of tries allowed for a given request before an error is thrown (default is 1).
#' @param timeout An integer specifying the timeout window for the request in seconds (default is 300).
#' @param debug A logical value indicating whether debug mode should be initiated (default is FALSE).
#'
#' @return A response object containing the http status code and response content.
#'
#' @importFrom httr POST GET DELETE content add_headers timeout
#' @importFrom RCurl curlPerform
#' @importFrom jsonlite toJSON fromJSON
#' @export
making_requests <- function(method = c("POST", "GET", "DELETE"), endpoint = "v1/chat/completions",
                            data = NULL, encode = "json",
                            stream = FALSE, stream_type = c("completion", "chat_completion"), stream_file = NULL,
                            post_type = "application/json", response_type = "application/json",
                            api_url = NULL, api_key = NULL, organization = NULL,
                            max_tries = 1, timeout = 300, debug = FALSE) {
  method <- match.arg(method)
  stream_type <- match.arg(stream_type)
  api_url <- api_url %||% getOption("openapi_api_url")
  api_key <- api_key %||% getOption("openapi_api_key")
  organization <- organization %||% getOption("openapi_organization")
  if (is.null(api_url) || is.null(api_key)) {
    stop("api_url or api_key is not defined, please run the api_setup function to configure them.")
  }
  url <- paste(api_url, endpoint, sep = "/")

  headers <- add_headers("Content-Type" = post_type)
  if (!is.null(api_key)) {
    headers[["headers"]]["Authorization"] <- paste("Bearer", api_key)
  }
  if (!is.null(organization)) {
    headers[["headers"]]["OpenAI-Organization"] <- organization
  }
  if (!is.null(data)) {
    args <- list(url = url, headers, body = data, encode = encode)
  } else {
    args <- list(url = url, headers)
  }

  if (isTRUE(stream)) {
    options(RCurlOptions = list(timeout = timeout))
    stream_content <- NULL
    stream_residual <- NULL
    if (!is.null(stream_file)) {
      file.create(stream_file)
    }
    stream_fun <- function(x) {
      split_content <- strsplit(ifelse(grepl("^data:", x), x, paste0(stream_residual, x)), "\n\n")[[1]]
      split_content <- split_content[split_content != ""]
      if (is.null(split_content)) {
        NULL
      } else {
        for (content in split_content) {
          if (!grepl("data: [DONE]", content, fixed = TRUE)) {
            content_json <- tryCatch(fromJSON(sub("([^\\{\\}]*)(\\{.*\\})([^\\{\\}]*)", "\\2", content), simplifyVector = FALSE), error = identity)
            if (!inherits(content_json, "error")) {
              if (stream_type == "completion") {
                x_content <- content_json[["choices"]][[1]][["text"]]
                stream_content <<- c(stream_content, x_content)
                if (!is.null(stream_file)) {
                  cat(paste0(x_content, collapse = ""), file = stream_file, append = TRUE, sep = "")
                } else {
                  cat(x_content, sep = "")
                }
              } else if (stream_type == "chat_completion") {
                if ("role" %in% names(content_json[["choices"]][[1]][["delta"]])) {
                  if (debug) {
                    x_content <- content_json[["choices"]][[1]][["delta"]][["role"]]
                    cat("role:", x_content, "\n")
                  } else {
                    NULL
                  }
                } else {
                  x_content <- content_json[["choices"]][[1]][["delta"]][["content"]]
                  stream_content <<- c(stream_content, x_content)
                  if (!is.null(stream_file)) {
                    cat(paste0(x_content, collapse = ""), file = stream_file, append = TRUE, sep = "")
                  } else {
                    cat(x_content, sep = "")
                  }
                }
              }
            } else {
              stream_residual <<- content
              if (debug) {
                cat(content, "\n", sep = "")
                # assign("content", content, envir = .GlobalEnv)
                # assign("content_json", content_json, envir = .GlobalEnv)
              } else {
                NULL
              }
            }
          } else {
            if (!is.null(stream_file)) {
              cat("\ndata: [DONE]\n", file = stream_file, append = TRUE, sep = "")
            } else {
              cat("\n")
            }
          }
        }
      }
    }
    result <- try_get(curlPerform(
      url = url, postfields = toJSON(data, auto_unbox = TRUE, digits = 22),
      httpheader = as.list(headers$headers), .encoding = "UTF-8",
      writefunction = stream_fun
    ), max_tries = max_tries)
    if (result != 0) {
      stop("Error occurred while executing CURL request.")
      return(result)
    } else {
      headers[["Content-Type"]] <- response_type <- "application/json"
      if (stream_type == "completion") {
        content <- charToRaw(toJSON(list(
          object = "text_completion",
          model = data$model,
          choices = list(
            list(
              text = paste0(stream_content, collapse = ""),
              finish_reason = "stop"
            )
          )
        ), auto_unbox = TRUE, digits = 22))
      } else {
        content <- charToRaw(toJSON(list(
          object = "chat.completion",
          model = data$model,
          choices = list(
            list(
              message = list(content = paste0(stream_content, collapse = "")),
              finish_reason = "stop"
            )
          )
        ), auto_unbox = TRUE, digits = 22))
      }
      req <- httr:::request_build("POST", url, httr:::body_config(data, encode))
      resp <- httr:::response(
        url = url, status_code = result,
        headers = headers, all_headers = NULL, cookies = NULL,
        content = content, date = Sys.time(), times = NULL,
        request = req, handle = NULL
      )
    }
  } else {
    args <- c(args, list(timeout(timeout)))
    # assign("args", args, envir = .GlobalEnv)
    resp <- try_get(do.call(method, args), max_tries = max_tries)
  }

  status <- check_response(resp, content_type = response_type)
  attr(resp, "status") <- status
  return(resp)
}

#' Check Response
#'
#' Checks if the response was successful and matches the given content type.
#'
#' @param resp HTTP response object returned by 'httr' package function.
#' @param content_type Expected content type of the response.
#' @return Returns a string with value "ok" if response status code is not an error
#' code and the content type of the response matches the expected content type.
#' Otherwise it returns "notok".
#' @examples
#' check_response(resp = httr::POST("http://httpbin.org/post", body = list(a = 1, b = "xyz")))
#' check_response(resp = httr::POST("http://httpbin.org/status/404"), content_type = "application/json")
#' @importFrom httr http_error http_type content message_for_status
#' @export
check_response <- function(resp, content_type = "application/json") {
  if (isTRUE(http_error(resp))) {
    message("Request failed:")
    message_for_status(resp)
    message(content(resp, as = "text", encoding = "UTF-8"))
    return("notok")
  } else if (!identical(http_type(resp), content_type)) {
    message("The content type of the response is not '", content_type, "'")
    return("notok")
  } else {
    return("ok")
  }
}

#' Parse Response
#'
#' This function parses a response using the specified class.
#'
#' @param resp Response object to parse.
#' @param class Class to use for parsing.
#'
#' @return Object parsed with the specified class if class is specified and response is JSON, otherwise the contents of the response object.
#'
#' @examples
#' \dontrun{
#' # parse response with specified class
#' parse_response(resp, MyResponseClass)
#' # parse response without class
#' parse_response(resp)
#' }
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export
parse_response <- function(resp, class = NULL) {
  if (identical(attr(resp, "status"), "notok") || !inherits(resp, "response")) {
    return(resp)
  } else {
    if (identical(http_type(resp), "application/json") && !is.null(class)) {
      return(class$new(resp))
    } else {
      return(content(resp))
    }
  }
}


# making_requests <- function(method = c("POST", "GET", "DELETE"), endpoint = "v1/chat/completions",
#                             data = NULL, type = c("json", "multipart", "file", "form", "raw"),
#                             stream = FALSE, stream_type = c("completion", "chat_completion"), stream_content = NULL,
#                             post_type = "application/json", response_type = "application/json", ...,
#                             api_url = NULL, api_key = NULL, organization = NULL, max_tries = 1) {
#   method <- match.arg(method)
#   type <- match.arg(type)
#   api_url <- api_url %||% getOption("openapi_api_url")
#   api_key <- api_key %||% getOption("openapi_api_key")
#   organization <- organization %||% getOption("openapi_organization")
#   url <- paste(api_url, endpoint, sep = "/")
#
#   req <- request(url) %>% req_headers("Content-Type" = post_type)
#   if (!is.null(api_key)) {
#     req <- req %>% req_headers("Authorization" = paste("Bearer", api_key))
#   }
#   if (!is.null(organization)) {
#     req <- req %>% req_headers("OpenAI-Organization" = organization)
#   }
#   if (!is.null(data)) {
#     body_method <- paste0("req_body_", type)
#     req <- do.call(what = body_method, list(req, data))
#   }
#
#   req <- req %>%
#     req_method(method = method) %>%
#     req_retry(max_tries = max_tries)
#
#   if (isTRUE(stream)) {
#     response_type <- "text/event-stream"
#     residual <- NULL
#     stream_content <- NULL # only for test
#     completion_stream <- function(x) {
#       x_char <- paste0(residual, rawToChar(x), collapse = "")
#       split_content <- strsplit(x_char, "\n")[[1]]
#       split_content <- split_content[split_content != ""]
#       for (i in seq_along(split_content)) {
#         content_json <- tryCatch(fromJSON(sub("([^\\{\\}]*)(\\{.*\\})([^\\{\\}]*)", "\\2", split_content[[i]]), simplifyVector = FALSE), error = identity)
#         if (inherits(content_json, "error")) {
#           residual <<- paste0(split_content[i:length(split_content)], collapse = "\n")
#           return(TRUE)
#         } else {
#           stream_header <<- content_json
#           x_content <- content_json[["choices"]][[1]][["text"]]
#           stream_content <<- c(stream_content, x_content)
#           cat(x_content, sep = "")
#         }
#       }
#       return(TRUE)
#     }
#     chat_completion_stream <- function(x) {
#       print(Sys.time())
#       x_char <- paste0(residual, rawToChar(x), collapse = "")
#       split_content <- strsplit(x_char, "\n")[[1]]
#       split_content <- split_content[split_content != ""]
#       for (i in seq_along(split_content)) {
#         content_json <- tryCatch(fromJSON(sub("([^\\{\\}]*)(\\{.*\\})([^\\{\\}]*)", "\\2", split_content[[i]]), simplifyVector = FALSE), error = identity)
#         if (inherits(content_json, "error")) {
#           residual <<- paste0(split_content[i:length(split_content)], collapse = "\n")
#           return(TRUE)
#         } else {
#           if ("role" %in% names(content_json[["choices"]][[1]][["delta"]])) {
#             stream_header <- content_json
#             stream_header[["choices"]][[1]][["delta"]] <- NULL
#             stream_header[["choices"]][[1]][["message"]][["role"]] <- content_json[["choices"]][[1]][["delta"]][["role"]]
#             stream_header <<- stream_header
#           } else {
#             x_content <- content_json[["choices"]][[1]][["delta"]][["content"]]
#             stream_content <<- c(stream_content, x_content)
#             cat(x_content, sep = "")
#           }
#         }
#       }
#       return(TRUE)
#     }
#
#     if (stream_type == "completion") {
#       resp <- req_stream(req, completion_stream, buffer_kb = 1)
#       cat("\n")
#       stream_all <- stream_header
#       stream_all[["choices"]][[1]][["text"]] <- paste0(stream_content, collapse = "")
#       resp$content <- charToRaw(toJSON(stream_all))
#     } else {
#       resp <- req_stream(req, chat_completion_stream, buffer_kb = 1)
#       cat("\n")
#       stream_all <- stream_header
#       stream_all[["choices"]][[1]][["message"]][["content"]] <- paste0(stream_content, collapse = "")
#       resp$content <- charToRaw(toJSON(stream_all))
#     }
#   } else {
#     resp <- req_perform(req)
#   }
#
#   status <- check_response(resp, content_type = response_type)
#   attr(resp, "status") <- status
#   return(resp)
# }
#
# check_response <- function(resp, content_type = "application/json") {
#   if (resp_is_error(resp)) {
#     message("Request failed:")
#     message(resp_status_desc(resp))
#     return("notok")
#   } else if (!identical(resp_content_type(resp), content_type)) {
#     message("The content type of the response '", resp_content_type(resp), "' is not as expected:'", content_type, "'")
#     return("notok")
#   } else {
#     return("ok")
#   }
# }
#
# parse_response <- function(resp, type = c("json", "_html", "xml", "string", "raw"), ...) {
#   type <- match.arg(type)
#   if (attr(resp, "status") == "ok") {
#     body_method <- paste0("resp_body_", type)
#     content <- do.call(what = body_method, list(resp))
#     return(content)
#   } else {
#     return(resp)
#   }
# }
