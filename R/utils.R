#' @importFrom httr GET
#' @export
api_setup <- function(api_url = "https://api.openai.com", api_key = NULL, organization = NULL, check_url = TRUE) {
  api_url <- api_url %||% getOption("openapi_api_url")
  api_key <- api_key %||% getOption("openapi_api_key")
  openapi_organization <- organization %||% getOption("openapi_organization")
  if (is.null(api_url)) {
    stop("Please specify the API URL.")
  }
  if (is.null(api_key)) {
    stop("Please obtain an API key from https://platform.openai.com/ or the mirror sites.")
  }
  options(openapi_api_url = api_url)
  options(openapi_api_key = api_key)
  options(openapi_organization = organization)
  if (isTRUE(check_url)) {
    try_get(GET(api_url), error_message = paste0("Unable to establish a connection with api_url: ", api_url))
  }
  return(invisible(NULL))
}

try_get <- function(expr, max_tries = 1, sleep = 1, error_message = "", retry_message = "Retrying...") {
  out <- simpleError("start")
  ntry <- 0
  while (inherits(out, "error")) {
    ntry <- ntry + 1
    out <- tryCatch(
      expr = eval.parent(substitute(expr)),
      error = function(error) {
        message(error_message)
        Sys.sleep(sleep)
        return(error)
      }
    )
    if (inherits(out, "error") && ntry >= max_tries) {
      stop(out)
    } else {
      if (!inherits(out, "error")) {
        break
      } else {
        message(retry_message)
      }
    }
  }
  return(out)
}

#' Download File from the Internet
#'
#' @inheritParams utils::download.file
#' @param methods Methods to be used for downloading files. The default is to try different download methods in turn until the download is successfully completed.
#' @param max_tries Number of tries for each download method.
#' @param ... Other arguments passed to \code{\link[utils]{download.file}}
#'
#' @importFrom utils download.file
#' @export
download <- function(url, destfile, methods = c("auto", "wget", "libcurl", "curl", "wininet", "internal"), quiet = TRUE, ..., max_tries = 1) {
  if (missing(url) || missing(destfile)) {
    stop("'url' and 'destfile' must be both provided.")
  }
  ntry <- 0
  status <- NULL
  while (is.null(status)) {
    for (method in methods) {
      status <- tryCatch(expr = {
        suppressWarnings(download.file(url, destfile = destfile, method = method, quiet = quiet, ...))
        status <- 1
      }, error = function(error) {
        message(error)
        message("Cannot download from the url: ", url)
        message("Failed to download using \"", method, "\". Retry...\n")
        Sys.sleep(1)
        return(NULL)
      })
      if (!is.null(status)) {
        break
      }
    }
    ntry <- ntry + 1
    if (is.null(status) && ntry >= max_tries) {
      stop("Download failed.")
    }
  }
  return(invisible(NULL))
}

#' @importFrom httr parse_url
#' @importFrom png readPNG
#' @importFrom grid grid.newpage grid.draw rasterGrob
#' @export
fetch_image <- function(url, destfile = NULL, plot = TRUE) {
  parsed_url <- parse_url(url)
  fileext <- strsplit(parsed_url$path, "\\.")[[1]]
  fileext <- fileext[length(fileext)]
  if (is.null(destfile)) {
    destfile <- tempfile(fileext = paste0(".", fileext))
    on.exit(unlink(destfile))
  }
  if (!grepl(pattern = paste0(fileext, "$"), x = destfile)) {
    stop("The file extension does not match the URL: ", fileext)
  }
  download(url, destfile = destfile, mode = "wb")
  png <- readPNG(destfile)
  grob <- rasterGrob(png)
  if (isTRUE(plot)) {
    grid.newpage()
    grid.draw(grob)
  }
  return(grob)
}

#' @importFrom png readPNG writePNG
#' @importFrom grid rasterGrob
#' @export
mask_image <- function(image, height_range = NULL, width_range = NULL, R_range = NULL, G_range = NULL, B_range = NULL, destfile = NULL, plot = TRUE) {
  png <- readPNG(image)
  if (dim(png)[3] < 4) {
    png <- array(png, dim = c(dim(png)[1], dim(png)[2], 4))
    png[, , 4] <- 1
  }
  if (!is.null(height_range) || !is.null(width_range)) {
    height_range <- height_range %||% c(1, dim(png)[1])
    width_range <- width_range %||% c(1, dim(png)[2])
    png[seq.int(height_range[1], height_range[2]), seq.int(width_range[1], width_range[2]), 4] <- 0
  }
  if (!is.null(R_range) || !is.null(G_range) || !is.null(B_range)) {
    R_range <- R_range %||% c(1, 0)
    G_range <- G_range %||% c(1, 0)
    B_range <- B_range %||% c(1, 0)
    RGB_mask <- (png[, , 1] > R_range[1] & png[, , 1] < R_range[2]) |
      (png[, , 2] > G_range[1] & png[, , 2] < G_range[2]) |
      (png[, , 3] > B_range[1] & png[, , 3] < B_range[2])
    png[, , 4][RGB_mask] <- 0
  }

  if (!is.null(destfile)) {
    writePNG(png, target = destfile)
  }
  grob <- rasterGrob(png)
  if (isTRUE(plot)) {
    grid.newpage()
    grid.draw(grob)
  }
  return(grob)
}

truncate_text <- function(text, max_length = 50) {
  truncated_text <- sapply(text, function(x) {
    if (nchar(x) > max_length) {
      paste0(substr(x, 1, max_length - 3), "...(", nchar(x) - max_length, " characters omitted)")
    } else {
      x
    }
  })
  return(truncated_text)
}

#' @export
fetch_prompts <- function(language = c("en", "zh")) {
  language <- match.arg(language)
  url <- switch(language,
    en = "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv",
    zh = "https://raw.githubusercontent.com/PlexPt/awesome-chatgpt-prompts-zh/main/prompts-zh.json"
  )
  tmp <- tempfile()
  on.exit(unlink(tmp))
  download(url, destfile = tmp)
  data <- switch(language,
    en = read.csv(tmp),
    zh = do.call(rbind.data.frame, fromJSON(readLines(tmp), simplifyVector = FALSE))
  )
  data[["act"]] <- trimws(gsub("(Act as)|(充当)|(担任)|(作为)|(扮演)", "", data[["act"]]))
  return(data)
}

#' @examples
#' q <- ChatGPT$new(act_as = "Prompt generater")$chat("Act as an R Package Development Assistant")
#' prompt1 <- q$last()
#' q <- ChatGPT$new()$chat(prompt1, role = "system")
#'
#' prompt2 <- generate_prompts("Act as an R Package Development Assistant")
#' q <- ChatGPT$new()$chat(prompt2, role = "system")
#'
#' @export
generate_prompts <- function(prompt = "Act as an R Package Development Assistant", ...) {
  messages <- list(
    list(
      "role" = "system",
      "content" = paste0(prompts[prompts[["act"]] == "ChatGPT prompt generator", "prompt"], "(Give me prompt only)")
    ),
    list(
      "role" = "user",
      "content" = prompt
    )
  )
  response <- create_chat_completion(messages = messages, ...)
  return(response$extract("choices")[1])
}
