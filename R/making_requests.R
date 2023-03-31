#' @importFrom httr POST GET DELETE content add_headers timeout
#' @importFrom RCurl curlPerform
#' @importFrom jsonlite toJSON fromJSON
making_requests <- function(method = c("POST", "GET", "DELETE"), endpoint = "v1/chat/completions",
                            data = NULL, encode = "json",
                            stream = FALSE, stream_type = c("completion", "chat_completion"),
                            post_type = "application/json", response_type = "application/json",
                            api_url = NULL, api_key = NULL, organization = NULL,
                            max_tries = 1, timeout = 300) {
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
    # if (!is.null(stream_output)) {
    #   file.create(stream_output)
    # }
    stream_fun <- function(x) {
      split_content <- strsplit(x, "\n\n")[[1]]
      split_content <- split_content[split_content != ""]
      if (is.null(split_content)) {
        cat(x, sep = "")
      } else {
        for (content in split_content) {
          if (!grepl("data:", content, fixed = TRUE)) {
            NULL
            # cat(content, sep = "")
          }
          if (!grepl("data: [DONE]", content, fixed = TRUE)) {
            content_json <- tryCatch(fromJSON(sub("([^\\{\\}]*)(\\{.*\\})([^\\{\\}]*)", "\\2", content), simplifyVector = FALSE), error = identity)
            if (!inherits(content_json, "error")) {
              if (stream_type == "completion") {
                x_content <- content_json[["choices"]][[1]][["text"]]
                stream_content <<- c(stream_content, x_content)
                cat(x_content, sep = "")
                # if (!is.null(stream_output)) {
                #   write(paste0(x_content,collapse = ""),file=stream_output,append=TRUE)
                # }
              } else {
                if ("role" %in% names(content_json[["choices"]][[1]][["delta"]])) {
                  NULL
                  # x_content <- content_json[["choices"]][[1]][["delta"]][["role"]]
                  # cat("role:", x_content, "\n")
                } else {
                  x_content <- content_json[["choices"]][[1]][["delta"]][["content"]]
                  stream_content <<- c(stream_content, x_content)
                  cat(x_content, sep = "")
                  # if (!is.null(stream_output)) {
                  #   write(paste0(x_content,collapse = ""),file=stream_output,append=TRUE)
                  # }
                }
              }
            } else {
              # e_x <<- content
              # e <<- content_json
              cat("*", sep = "")
            }
          } else {
            cat("\n")
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
      status <- check_response(resp, content_type = response_type)
      attr(resp, "status") <- status
      return(resp)
    }
  } else {
    args <- c(args, list(timeout(timeout)))
    resp <- try_get(do.call(method, args), max_tries = max_tries)
    status <- check_response(resp, content_type = response_type)
    attr(resp, "status") <- status
    return(resp)
  }
}

#' @importFrom httr http_error http_type content message_for_status
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

#' @importFrom httr content
#' @importFrom jsonlite fromJSON
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
