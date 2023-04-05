#' A Shiny Gadget for AI Chatbot based on OpenAI API
#'
#' This function generates a Shiny Gadget for an AI Chatbot based on the OpenAI API. The user can interact with the chatbot using prompts/questions and receive intelligent responses.
#'
#' @param viewer A viewer pane to use for displaying the gadget (defaults to paneViewer(minHeight = 300)).
#' @param api_url The URL of the OpenAI API.
#' @param api_key The API key to access the OpenAI API.
#' @param organization The ID of the organization used to access the OpenAI API.
#' @param ... Additional arguments to pass to the ChatGPT class constructor.
#'
#' @import shiny
#' @import miniUI
#' @importFrom future plan future value multisession
#' @export
ChatGPT_gadget <- function(viewer = NULL, api_url = NULL, api_key = NULL, organization = NULL, ...) {
  plan(multisession, workers = 2)
  colors <- c(dark = "#202123", darkchat = "#353541", lightchat = "#444653", input = "#41404e")
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_content <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "speech-bubble-line-icon.svg", package = "openapi")
  user_content <- readLines(user_path, warn = FALSE)
  jscode <- "
    $(function() {
      var $els = $(\"[data-proxy-click]\");
      $.each(
        $els,
        function(idx, el) {
          var $el = $(el);
          var $proxy = $(\"#\" + $el.data(\"proxyClick\"));
          $el.keydown(function(e) {
            if (e.keyCode === 13 && !e.shiftKey) {
              e.preventDefault();
              e.stopPropagation();
              setTimeout(function() {
               $proxy.click();
              }, 500);
            }
          });
        }
      );
    });
  "

  ui <- miniPage(
    id = "addin",
    # tags$style(
    #   type = "text/css",
    #   paste0("#addin { background-color: ",colors["dark"],"; }"),
    # ),
    miniTitleBar(tags$strong("ChatGPT"), right = miniTitleBarButton("exit", label = "Exit")),
    miniTabstripPanel(
      miniTabPanel("Chat",
        icon = icon("comments"),
        tags$head(tags$script(HTML(jscode))),
        miniContentPanel(
          fillCol(
            flex = c(1, NA),
            div(
              id = "chat_output_container", style = "height: 100%; max-height: 100%; overflow-y: auto;",
              fillRow(
                flex = c(NA, NA, 1),
                div(HTML(paste0(openai_content, collapse = "\n")), style = "width:30px; height:30px"),
                div(style = "width:5px"),
                div(
                  verbatimTextOutput(
                    outputId = "chat_output", placeholder = TRUE
                  ),
                  tags$head(tags$style(
                    paste0("#chat_output{color:white; background: ", colors["lightchat"], "; font-size:12px;
                             white-space: pre-wrap; overflow-wrap: break-word; max-width: 100%; max-height: 100%;}")
                  )),
                  div(actionButton("chat_regenerate", label = "Regenerate", icon = icon("repeat"), width = "120px"), style = "display:inline-block; float:right")
                )
              )
            ),
            div(
              id = "chat_input_container", style = "bottom: 0; height: 100%; max-height: 100%; width: 100%;",
              div(style = "border-top: 1px solid #CDD2D4"),
              div(style = "height:10px"),
              fillRow(
                flex = c(1, NA),
                tagAppendAttributes(
                  div(
                    textAreaInput(
                      inputId = "chat_input",
                      label = NULL,
                      value = NULL,
                      resize = "vertical",
                      width = "99%",
                      height = "100%",
                      placeholder = "Enter your prompts here (Press Enter + Shift to start a new line)"
                    ),
                    tags$head(tags$style(
                      paste0("#chat_input{color:white; background: ", colors["input"], "; font-size:12px;
                    height:auto; min-height:36px; max-height: 100%;
                    white-space: pre-wrap; overflow-wrap: break-word;}")
                    )),
                    tags$script("
                      var textarea = document.getElementById(\"chat_input\");
                      textarea.addEventListener(\"input\", function() {
                          $(this).height(0);
                          $(this).height(this.scrollHeight);
                      });
                  ")
                  ),
                  "data-proxy-click" = "chat_submit"
                ),
                fillCol(
                  flex = c(NA, NA, NA),
                  actionButton("chat_submit", label = "Send", icon = icon("paper-plane"), width = "105px", style = "text-align: center;"),
                  div(style = "height:3px"),
                  actionButton("chat_clear", label = "Clear chat", icon = icon("rotate-right"), width = "105px", style = "text-align: center;")
                )
              )
            )
          )
        )
      ),
      miniTabPanel("Image",
        icon = icon("image"),
        miniContentPanel()
      ),
      miniTabPanel("Audio",
        icon = shiny::icon("microphone"),
        miniContentPanel()
      ),
      miniTabPanel("Help",
        icon = icon("circle-info"),
        miniContentPanel()
      )
    )
  )
  server <- function(input, output, session) {
    api_url <- api_url %||% getOption("openapi_api_url")
    api_key <- api_key %||% getOption("openapi_api_key")
    organization <- organization %||% getOption("openapi_organization")
    if (is.null(api_url) || is.null(api_key)) {
      warning("api_url or api_key is not defined, please run the api_setup function to configure them.\n Exiting ChatGPT...")
      stopApp()
    }

    conversation <- tempfile(fileext = ".txt")
    file.create(conversation)
    file <- file(conversation)
    onStop(function() {
      close(file)
      unlink(conversation)
    })

    r <- reactiveValues(chat = ChatGPT$new(), async = NULL)

    observe({
      chat_input <- input$chat_input
      updateTextAreaInput(session, inputId = "chat_input", value = "")
      if (inherits(r$async, "Future")) {
        r$chat <- value(r$async)
      }
      rchat <- r$chat
      r$async <- future(rchat$chat(chat_input,
        stream = TRUE,
        stream_file = conversation,
        api_url = api_url,
        api_key = api_key,
        organization = organization,
        ...
      ))
      NULL
    }) %>% bindEvent(input$chat_submit)

    observe({
      if (inherits(r$async, "Future")) {
        r$chat <- value(r$async)
      }
      rchat <- r$chat
      r$async <- future(rchat$regenerate(
        stream = TRUE,
        stream_file = conversation,
        api_url = api_url,
        api_key = api_key,
        organization = organization,
        ...
      ))
      NULL
    }) %>% bindEvent(input$chat_regenerate)

    output$chat_output <- renderText({
      invalidateLater(50)
      if (is.null(r$async)) {
        text <- "Welcome to the ChatGPT RStudio add-in! This is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.\n\nPlease enter your questions or topics in the input box below and press \"Enter\" to start chatting with the chatbot.\n\nWe hope you enjoy using it!"
      } else {
        text <- readLines(file, warn = FALSE)
        if (identical(text[length(text)], "data: [DONE]")) {
          text <- text[-length(text)]
        }
      }
      paste0(text, collapse = "\n")
    })

    observe({
      r$chat <- ChatGPT$new()
      r$async <- ""
      writeLines("This is a new chat.\n\nPlease enter your questions or topics in the input box below and press \"Enter\" to start chatting with the chatbot.", con = file)
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      stopApp()
    }) %>% bindEvent(input$exit)
  }
  if (is.null(viewer)) {
    viewer <- paneViewer(minHeight = 300)
  }
  runGadget(ui, server, viewer = viewer)
}

#' Run ChatGPT job
#'
#' The function runs a ChatGPT job by calling the openapi::ChatGPT_gadget function with the specified parameters defined in the arguments. It also creates an R script file and writes the ChatGPT function code into the file. The R script file is then run as a job through the jobRunScript() function, which creates a background process that listens for user inputs and generates responses using OpenAI API.
#'
#' @inheritParams ChatGPT_gadget
#'
#' @import rstudioapi
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up OpenAI API endpoint URL and API key
#' api_url <- "https://api.openai.com"
#' api_key <- "my-secret-api-key"
#'
#' # Start a ChatGPT job
#' ChatGPT_job(api_url = api_url, api_key = api_key)
#' }
#' @seealso
#' \code{\link{ChatGPT_gadget}} function
#' @import rstudioapi
#' @export
ChatGPT_job <- function(viewer = "rstudioapi::viewer", api_url = NULL, api_key = NULL, organization = NULL) {
  api_url <- api_url %||% getOption("openapi_api_url")
  api_key <- api_key %||% getOption("openapi_api_key")
  organization <- organization %||% getOption("openapi_organization")
  if (is.null(api_url) || is.null(api_key)) {
    stop("api_url or api_key is not defined, please run the api_setup function to configure them.")
  }
  try_get(GET(api_url), error_message = paste0("Unable to establish a connection with api_url: ", api_url))

  jobscript <- tempfile(fileext = ".R")
  file.create(jobscript)
  file <- file(jobscript)
  writeLines(
    text = paste0(
      "openapi::ChatGPT_gadget(viewer = ", viewer, ",api_url=\"", api_url, "\",api_key=\"", api_key, "\"",
      if (!is.null(organization)) paste0("\",organization=\"", organization, "\""), ")"
    ),
    con = jobscript
  )
  close(file)
  jobid <- jobRunScript(path = jobscript, name = "ChatGPT_addin")
  message("ChatGPT job ID : ", jobid)
  return(invisible(NULL))
}

#' @import rstudioapi
quote_selection_to_console <- function() {
  doc <- getActiveDocumentContext()
  doc_range <- doc$selection[[1]]$range
  selected_text <- doc$selection[[1]]$text
  if (all(nchar(selected_text) == 0L)) {
    message("No code selected")
    return(invisible(NULL))
  }
  if (nchar(selected_text) > 3800) {

  }
  quoted_text <- deparse(selected_text)
  sendToConsole(paste0("code <- ", quoted_text), execute = FALSE)
  return(invisible(NULL))
}

#' @import rstudioapi
code_xxx_addin <- function(fun) {
  cat("Edit selection using the", fun, "function ...\n")
  doc <- getActiveDocumentContext()
  selected_text <- doc$selection[[1]]$text
  if (all(nchar(selected_text) == 0L)) {
    cat("No code selected.\n")
    return(invisible(NULL))
  }
  res <- do.call(fun, args = list(code = selected_text))
  if (!is.null(res$extract("difference"))) {
    print(res$extract("difference"))
  }
  cat(fun, "finished.\n")
  return(invisible(NULL))
}

code_document_addin <- function() {
  code_xxx_addin("code_document")
}
code_check_addin <- function() {
  code_xxx_addin("code_check")
}
code_improve_addin <- function() {
  code_xxx_addin("code_improve")
}
code_comment_addin <- function() {
  code_xxx_addin("code_comment")
}
code_refactor_addin <- function() {
  code_xxx_addin("code_refactor")
}
code_explain_addin <- function() {
  code_xxx_addin("code_explain")
}
code_create_test_addin <- function() {
  code_xxx_addin("code_create_test")
}
