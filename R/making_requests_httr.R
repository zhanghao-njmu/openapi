#' @importFrom httr POST GET DELETE content add_headers
#' @importFrom RCurl curlPerform
#' @importFrom jsonlite toJSON fromJSON
making_requests <- function(method = c("POST", "GET", "DELETE"), endpoint = "v1/chat/completions",
                            data = NULL, encode = "json",
                            stream = FALSE, stream_type = c("completion", "chat_completion"),
                            post_type = "application/json", response_type = "application/json", ...,
                            api_url = NULL, api_key = NULL, organization = NULL,
                            max_tries = 1, timeout = 300) {
  method <- match.arg(method)
  stream_type <- match.arg(stream_type)
  api_url <- api_url %||% getOption("openapi_api_url")
  api_key <- api_key %||% getOption("openapi_api_key")
  organization <- organization %||% getOption("openapi_organization")
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
    stream_fun <- function(x) {
      split_content <- strsplit(x, "\n\n")[[1]]
      split_content <- split_content[split_content != ""]
      if (is.null(split_content)) {
        cat(x, sep = "")
      } else {
        for (content in split_content) {
          if (!grepl("data:", content, fixed = TRUE)) {
            stream_content <<- content_json
            cat(content, sep = "")
          }
          if (!grepl("data: [DONE]", content, fixed = TRUE)) {
            content_json <- tryCatch(fromJSON(sub("([^\\{\\}]*)(\\{.*\\})([^\\{\\}]*)", "\\2", content), simplifyVector = FALSE), error = identity)
            if (!inherits(content_json, "error")) {
              if (stream_type == "completion") {
                x_content <- content_json[["choices"]][[1]][["text"]]
                stream_content <<- c(stream_content, x_content)
                cat(x_content, sep = "")
              } else {
                if ("role" %in% names(content_json[["choices"]][[1]][["delta"]])) {
                  cat("role:", content_json[["choices"]][[1]][["delta"]][["role"]], "\n")
                } else {
                  x_content <- content_json[["choices"]][[1]][["delta"]][["content"]]
                  stream_content <<- c(stream_content, x_content)
                  cat(x_content, sep = "")
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
      return(paste0(stream_content, collapse = ""))
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
    message(content(resp)$error$message)
    message_for_status(resp)
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
parse_response <- function(resp, ...) {
  if (identical(attr(resp, "status"), "notok") || !inherits(resp, "response")) {
    return(resp)
  } else {
    if (identical(http_type(resp), "application/json")) {
      resp_content <- content(resp, as = "text", encoding = "UTF-8", ...)
      resp_content <- fromJSON(resp_content, simplifyVector = FALSE)
    } else {
      resp_content <- content(resp, ...)
    }
    return(resp_content)
  }
}
