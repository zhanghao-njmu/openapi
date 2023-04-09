#' Create a list of HTML divs for a chat application
#'
#' This function takes in a list of messages and creates HTML divs
#' for each message in a chat application. It also includes the
#' HTML div for displaying ChatGPT output.
#'
#' @param messages A list of messages containing content, role, and time.
#' @param openai_logo OpenAI logo content.
#' @param user_logo User logo content.
#'
#' @return A list of HTML divs.
#'
#' @import shiny
#'
div_create <- function(messages, openai_logo, user_logo) {
  divs <- list()
  if (length(messages) > 0) {
    content <- sapply(messages, function(x) x[["content"]])
    role <- sapply(messages, function(x) x[["role"]])
    time <- sapply(messages, function(x) x[["time"]])

    # remove assistant's last message
    if (role[length(role)] == "assistant") {
      content <- content[-length(content)]
      role <- role[-length(role)]
      time <- time[-length(time)]
    }

    # create HTML divs for each message
    for (i in seq_along(content)) {
      if (role[i] == "user") {
        divs <- c(divs, list(
          div(
            div(div(HTML(paste0(user_logo, collapse = "\n")), style = "width:30px; height:30px"),
              div(style = "width:5px;"),
              style = "display: flex; flex-direction: row;"
            ),
            div(
              div(time[i] %||% as.character(Sys.time()), style = "height:20px;"),
              div(class = "chat_input", gsub("\\n$", "", markdown(content[i])))
            ),
            style = "display: flex; flex-direction: row;"
          ),
          div(style = "height:20px;")
        ))
      } else {
        divs <- c(divs, list(
          div(
            div(div(HTML(paste0(openai_logo, collapse = "\n")), style = "width:30px; height:30px"),
              div(style = "width:5px;"),
              style = "display: flex; flex-direction: row;"
            ),
            div(
              div(time[i] %||% as.character(Sys.time()), style = "height:20px;"),
              div(class = "chat_output", gsub("\\n$", "", markdown(content[i])))
            ),
            style = "display: flex; flex-direction: row;"
          ),
          div(style = "height:20px;")
        ))
      }
    }
  }

  # add final HTML div for ChatGPT output
  divs <- c(divs, list(
    div(
      div(div(HTML(paste0(openai_logo, collapse = "\n")), style = "width:30px; height:30px"),
        div(style = "width:5px;"),
        style = "display: flex; flex-direction: row;"
      ),
      div(
        div(as.character(Sys.time()), style = "height:20px;"),
        uiOutput(outputId = "chat_output_last", class = "chat_output")
      ),
      style = "display: flex; flex-direction: row;"
    ),
    div(style = "height:20px;")
  ))

  return(divs)
}

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
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom future plan future value resolved
#' @importFrom future.callr callr
#' @export
ChatGPT_gadget <- function(viewer = NULL, api_url = NULL, api_key = NULL, organization = NULL, ...) {
  colors <- c(dark = "#202123", darkchat = "#353541", lightchat = "#4D4F5C", input = "#41404e")
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_logo <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "my-account-icon.svg", package = "openapi")
  user_logo <- readLines(user_path, warn = FALSE)

  ui <- miniPage(
    useShinyjs(),
    id = "addin",
    # tags$style(
    #   type = "text/css",
    #   paste0("#addin { background-color: ",colors["dark"],"; }"),
    # ),
    miniTitleBar(tags$strong("ChatGPT"), right = miniTitleBarButton("exit", label = "Exit")),
    miniTabstripPanel(
      miniTabPanel("Chat",
        icon = icon("comments"),
        tags$head(
          tags$style(
            paste0("#chat_input{color:white; background: ", colors["input"], "; font-size:12px;
                    height:auto; min-height:100px; max-height: 100%;
                    white-space: pre-wrap; overflow-wrap: break-word;}")
          )
        ),
        # tags$head(
        #   tags$script("
        #               Shiny.addCustomMessageHandler(\"scrollCallback\",
        #                   function(x) {
        #                     var objDiv = document.getElementById(\"chat_output_container\");
        #                     objDiv.scrollTop = objDiv.scrollHeight;
        #                   }
        #               );")
        # ),
        tags$head(
          tags$script(HTML("
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
                    "))
        ),
        miniContentPanel(
          fillCol(
            flex = c(1, NA),
            div(
              id = "chat_output_container", style = "height: 100%; max-height: 100%; overflow-y: auto;",
              uiOutput("chat_output"),
              div(style = "border-bottom: 1px solid #ccc;"),
              div(style = "height:5px;"),
              div(actionButton("chat_regenerate", label = "Regenerate", icon = icon("repeat"), width = "120px"), style = "float:right;"),
              div(style = "height:50px")
              # div(style = "border-top: 2px solid #202123"),
              # div(style = "height:5px;")
            ),
            div(
              id = "chat_input_container", style = "bottom: 0; height: 100%; max-height: 100%; width: 100%;",
              uiOutput("chat_stop_ui"),
              div(style = "border-top: 2px solid #202123"),
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
                    tags$script("
                      var textarea = document.getElementById(\"chat_input\");
                      textarea.addEventListener(\"input\", function() {
                          $(this).height(0);
                          $(this).height(this.scrollHeight);
                      });")
                  ),
                  "data-proxy-click" = "chat_submit"
                ),
                fillCol(
                  flex = NA,
                  materialSwitch("chat_continuous", label = "Continuous", status = "primary", value = TRUE, width = "130px"),
                  actionButton("chat_submit", label = "Send", icon = icon("paper-plane"), width = "130px", style = "text-align: center;"),
                  div(style = "height:3px"),
                  actionButton("chat_clear", label = "Clear chat", icon = icon("rotate-right"), width = "130px", style = "text-align: center;")
                )
              )
            )
          )
        )
      ),
      miniTabPanel("Image(dev)",
        icon = icon("image"),
        miniContentPanel()
      ),
      miniTabPanel("Audio(dev)",
        icon = shiny::icon("microphone"),
        miniContentPanel()
      ),
      miniTabPanel("Help(dev)",
        icon = icon("circle-info"),
        miniContentPanel()
      )
    )
  )

  server <- function(input, output, session) {
    plan(callr)

    api_url <- api_url %||% getOption("openapi_api_url")
    api_key <- api_key %||% getOption("openapi_api_key")
    organization <- organization %||% getOption("openapi_organization")
    if (is.null(api_url) || is.null(api_key)) {
      warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
      stopApp()
    }

    conversation <- tempfile(fileext = ".txt")
    file.create(conversation)
    file <- file(conversation)
    onStop(function() {
      close(file)
      unlink(conversation)
    })

    r <- reactiveValues(chat = ChatGPT$new(), messages = NULL, input = NULL, continuous = TRUE, async = NULL, processing = FALSE, stop = NULL)

    observe({
      if (input$chat_input != "" && isFALSE(r$processing)) {
        disable("chat_submit")
        disable("chat_regenerate")
        disable("chat_continuous")

        r$input <- input$chat_input
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        r$messages <- c(r$messages, list(list("role" = "user", "content" = r$input, "time" = as.character(Sys.time()))))
        writeLines("", file)

        rchat <- r$chat
        rinput <- r$input
        rcontinuous <- input$chat_continuous
        r$async <- future(rchat$chat(rinput,
          stream = TRUE,
          stream_file = conversation,
          continuous = rcontinuous,
          api_url = api_url,
          api_key = api_key,
          organization = organization,
          ...
        ))
        r$processing <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    observe({
      disable("chat_submit")
      disable("chat_regenerate")
      disable("chat_continuous")

      if (inherits(r$async, "Future")) {
        r$chat <- value(r$async)
      }
      writeLines("", file)

      r$messages <- r$messages[-length(r$messages)]
      rchat <- r$chat
      rcontinuous <- input$chat_continuous
      r$async <- future(rchat$regenerate(
        stream = TRUE,
        stream_file = conversation,
        continuous = rcontinuous,
        api_url = api_url,
        api_key = api_key,
        organization = organization,
        ...
      ))
      r$processing <- TRUE
      NULL
    }) %>% bindEvent(input$chat_regenerate)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")

      if (inherits(r$async, "Future")) {
        r$async$process$kill()
        r$text <- paste0(r$text, "[The message was interrupted]")
        r$messages <- c(r$messages, list(list("role" = "assistant", "content" = r$text, "time" = as.character(Sys.time()))))
        r$chat$messages <- r$messages
        r$chat$index <- length(r$chat$messages)
      }
      r$async <- NULL
      r$processing <- FALSE
      r$stop <- NULL
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")

      if (inherits(r$async, "Future")) {
        r$async$process$kill()
      }
      r$async <- NULL
      r$processing <- FALSE
      r$stop <- NULL
      r$chat <- ChatGPT$new()
      r$messages <- NULL
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      if (is.null(r$messages)) {
        text <- "Welcome to the ChatGPT!\n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.\n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.\n\nHave a great time!"
      } else {
        if (isTRUE(r$processing)) {
          r$stop <- div(
            div(actionButton("chat_stop", label = "Stop generating", icon = icon("stop"), width = "150px"), style = "text-align: center;"),
            div(style = "height:10px")
          )
          invalidateLater(50)
          text <- readLines(file, warn = FALSE)
          if (identical(text, "") || length(text) == 0) {
            text <- "..."
          }
          if (identical(text[length(text)], "data: [DONE]") || (resolved(r$async) && !is.null(r$async))) {
            # print(text)
            # print(resolved(r$async))
            # print(value(r$async))

            if (identical(text[length(text)], "data: [DONE]")) {
              text <- text[-length(text)]
            }
            if (inherits(r$async, "Future")) {
              r$chat <- value(r$async)
              new_messages <- r$chat$messages[length(r$chat$messages)]
              new_messages[[1]][["time"]] <- as.character(Sys.time())
              r$messages <- c(r$messages, new_messages)
            }
            r$processing <- FALSE
            r$stop <- NULL
            enable("chat_submit")
            enable("chat_regenerate")
            enable("chat_continuous")
          }
          # session$sendCustomMessage(type = "scrollCallback", 1)
        } else {
          text <- r$text
        }
      }
      # print(text)
      r$text <- gsub("\\n$", "", markdown(paste0(text, collapse = "\n")))
      NULL
    })

    output$chat_output <- renderUI({
      fluidPage(
        tagList(
          tags$head(tags$style(
            paste0(".chat_input {color:black; background-color: white;
                    padding: 10px 10px 0 10px; border-radius: 5px; border: 2px solid ", colors["lightchat"], ";
                    white-space: pre-wrap; overflow-wrap: break-word; display: inline-block;}")
          )),
          tags$head(tags$style(
            paste0(".chat_output {color:black; background-color: white;
                   padding: 10px 10px 0 10px; border-radius: 5px; border: 2px solid ", colors["darkchat"], ";
                   white-space: pre-wrap; overflow-wrap: break-word; display: inline-block;}")
          )),
          div_create(r$messages, openai_logo = openai_logo, user_logo = user_logo)
        )
      )
    })

    output$chat_output_last <- renderUI({
      r$text
    })

    output$chat_stop_ui <- renderUI({
      r$stop
    })

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
