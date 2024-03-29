#' API Setup
#'
#' This function sets up the API URL, API key, and organization for OpenAI's services.
#'
#' @param api_base The URL of the OpenAI API. Default is "https://api.openai.com".
#' @param api_key The API key for accessing the OpenAI API.
#' @param organization The organization name associated with the OpenAI API key.
#' @param check_url Logical to check the validity of the specified URL. Default is TRUE.
#'
#' @importFrom httr GET
#' @export
#'
#' @return The function returns nothing (invisible NULL).
#'
#' @examples
#' \dontrun{
#' ## Official OpenAI API
#' api_base <- "https://api.openai.com"
#' api_key <- "Bearer sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#' api_setup(api_base = api_base, api_key = api_key)
#'
#' ## Azure OpenAI API
#' api_base <- "https://xxxxxx.openai.azure.com"
#' api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
#' api_setup(api_base = api_base, api_key = api_key, key_nm = "api-key")
#' }
#' @export
#' @importFrom httr GET
api_setup <- function(api_base = "https://api.openai.com/v1", api_key = NULL, organization = NULL,
                      api_type = c("open_ai", "azure"), api_version = NULL, azure_deployment = "openai/deployments/gpt3",
                      check_url = TRUE) {
  if (is.null(api_base)) {
    stop("Please specify the API URL.")
  }
  if (is.null(api_key)) {
    stop("Please obtain an API key from https://platform.openai.com/.")
  }
  api_type <- match.arg(api_type)
  options(openapi_api_base = api_base)
  options(openapi_api_key = api_key)
  options(openapi_api_type = api_type)
  options(openapi_api_version = switch(api_type,
    "open_ai" = NULL,
    "azure" = api_version %||% "2023-03-15-preview"
  ))
  options(openapi_organization = organization)
  options(openapi_azure_deployment = azure_deployment)

  if (isTRUE(check_url)) {
    try_get(GET(api_base), error_message = paste0("Unable to establish a connection with api_base: ", api_base))
  }
  return(invisible(NULL))
}

#' Try evaluating an expression with a specified number of tries.
#'
#' @param expr the expression to evaluate
#' @param max_tries the maximum number of tries before an error is returned
#' @param sleep the number of seconds to wait before retrying
#' @param error_message the message to print when an error is caught
#' @param retry_message the message to print when a retry is attempted
#'
#' @return Returns the output of the evaluated expression or an error message.
#'
#' @export
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

#' Download a file from a URL
#'
#' Downloads a file from a given URL and saves it to a specified destination file.
#'
#' @param url a character string specifying the URL to download the file from
#' @param destfile a character string specifying the name of the file in which to save the download.
#' @param methods A character string or vector specifying the methods to be used for the transfer. Default is "auto".
#' @param quiet a logical indicating whether to run quietly, default TRUE
#' @param ... optional additional arguments passed to download.file.
#' @param max_tries an integer indicating the maximum number of retries to attempt for downloading the file. Default is 1.
#'
#' @return `NULL`. The function invisibly returns NULL.
#' @export download
#'
#' @examples
#' \dontrun{
#' # Download MP3 file:
#' download("http://www.example.com/audio.mp3", "audio.mp3")
#' # Download CSV file:
#' download("http://www.example.com/mydata.csv", "mydata.csv")
#' }
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

#' Downloads an image from a given URL and displays it if desired.
#'
#' @param url the URL of the image to download.
#' @param destfile (optional) path to the destination file where the downloaded image should be saved. If not specified, a temporary file with a file extension that matches the URL will be created, and automatically deleted when the function finishes.
#' @param plot (optional) whether to display the downloaded image or not. Default is TRUE.
#'
#' @return An image object of class "grob".
#'
#' @examples
#' fetch_image("https://www.r-project.org/logo/Rlogo.png")
#'
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

#' Mask an image based on RGBA ranges
#'
#' @param image The path to the image file
#' @param height_range The range of pixel heights to mask out (optional)
#' @param width_range The range of pixel widths to mask out (optional)
#' @param R_range The range of red pixel values to mask out (optional)
#' @param G_range The range of green pixel values to mask out (optional)
#' @param B_range The range of blue pixel values to mask out (optional)
#' @param destfile The path and filename to save the masked image (optional)
#' @param plot Logical value indicating whether to plot the masked image (default = TRUE)
#'
#' @return A rasterGrob object containing the masked image
#'
#' @examples
#' \dontrun{
#' # Mask out a range of pixel heights and widths
#' mask_image("example.png",
#'   height_range = c(100, 200), width_range = c(50, 150)
#' )
#' # Mask out a range of RGB values
#' mask_image("example.png",
#'   R_range = c(0.2, 0.4), G_range = c(0, 0.3), B_range = c(0.1, 0.5)
#' )
#' # Save the masked image
#' mask_image("example.png",
#'   height_range = c(100, 200), width_range = c(50, 150),
#'   destfile = "masked_image.png"
#' )
#' }
#'
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

#' Truncate Text to a Maximum Length
#'
#' Truncates text to a maximum length and adds an indicator of truncation.
#'
#' @param text Character vector of text to truncate.
#' @param max_length Maximum desired length of the truncated text. Defaults to 50.
#'
#' @return A character vector of the truncated text.
#'
#' @examples
#' truncate_text(c("This is a long piece of text", "Shorter text"), max_length = 10)
#'
#' @export
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


#' Fetch Chat Prompts
#'
#' This function fetches chat prompts from Github for various languages.
#'
#' @param language A character string specifying the language of the chat prompts you want to fetch (default "en").
#' @return A data frame containing the chat prompts in the specified language.
#' @export
#' @examples
#' prompts <- fetch_prompts(language = "en")
#' head(prompts)
#' @importFrom jsonlite fromJSON
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
  data[["act"]] <- trimws(gsub("(Act as)|(\u5145\u5F53|\u62C5\u4EFB|\u4F5C\u4E3A|\u626E\u6F14)", "", data[["act"]]))
  data[["language"]] <- language
  return(data)
}

#' Generate Prompts
#'
#' This function generates prompts for the user to engage in a chat with a chatbot.
#'
#' @param prompt A string indicating the prompt for the chat.
#' @param ... Additional arguments.
#'
#' @return A prompt for the user to engage in a chat with a chatbot.
#'
#' @examples
#' \dontrun{
#' q <- ChatGPT$new(act_as = "Prompt generater")$chat("Act as an R Package Development Assistant")
#' prompt1 <- q$last()
#' q <- ChatGPT$new()$chat(prompt1, role = "system")
#'
#' prompt2 <- generate_prompts("Act as an R Package Development Assistant")
#' q <- ChatGPT$new()$chat(prompt2, role = "system")
#' }
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

#' A collection of prompts
#'
#' @format A \code{data.frame} object.
#' @concept data
#' @source \url{https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv} \url{https://raw.githubusercontent.com/PlexPt/awesome-chatgpt-prompts-zh/main/prompts-zh.json}
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   prompts_zh <- fetch_prompts(language = "zh")
#'   prompts_en <- fetch_prompts(language = "en")
#'   prompt_custom <- data.frame(
#'     act = "R Package Development Assistant",
#'     prompt = "I want you to act as a knowledgeable R Package development assistant, capable of understanding all the nuances and intricacies of developing R packages. Enlighten me on where to start when developing a new R package, provide suggestions on which tools can help to simplify the process, the secrets of writing robust functions, how to document them along with the whole package, strategies to handle dependencies, and finally, how to test the package to ensure it is working correctly. Prepare for potential troubleshooting scenarios to guide me through to completion of the package.",
#'     language = "en"
#'   )
#'   prompts <- do.call(rbind, list(prompts_zh, prompts_en, prompt_custom))
#'   # usethis::use_data(prompts)
#' }
#' }
"prompts"
