div_create <- function(messages, openai_logo, user_logo) {
  # print(rnorm(1))
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

div_update <- function(messages, openai_logo, user_logo) {
  fluidPage(
    tagList(
      tags$head(tags$style(
        paste0(".chat_input {color:black; background-color: white;
                    padding: 10px 10px 0 10px; border-radius: 5px; border: 2px solid #4D4F5C;
                    white-space: pre-wrap; overflow-wrap: break-word; display: inline-block;}")
      )),
      tags$head(tags$style(
        paste0(".chat_output {color:black; background-color: white;
                   padding: 10px 10px 0 10px; border-radius: 5px; border: 2px solid #353541;
                   white-space: pre-wrap; overflow-wrap: break-word; display: inline-block;}")
      )),
      div_create(messages, openai_logo = openai_logo, user_logo = user_logo)
    )
  )
}

#' A Shiny Gadget for AI Chatbot based on OpenAI API
#'
#' This function generates a Shiny Gadget for an AI Chatbot based on the OpenAI API. The user can interact with the chatbot using prompts/questions and receive intelligent responses.
#'
#' @param viewer A viewer pane to use for displaying the gadget (defaults to paneViewer(minHeight = 300)).
#' @import shiny
#' @import miniUI
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom future plan future value resolved
#' @importFrom future.callr callr
#' @export
ChatGPT_gadget <- function(viewer = NULL, ...) {
  args <- as.list(match.call())[-1]
  args[["api_url"]] <- args[["api_url"]] %||% getOption("openapi_api_url")
  args[["api_key"]] <- args[["api_key"]] %||% getOption("openapi_api_key")
  args[["key_nm"]] <- args[["key_nm"]] %||% getOption("openapi_key_nm")
  args[["organization"]] <- args[["organization"]] %||% getOption("openapi_organization")
  args[["organization_nm"]] <- args[["organization_nm"]] %||% getOption("openapi_organization_nm")

  if (is.null(args[["api_url"]]) || is.null(args[["api_key"]])) {
    warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
    stopApp()
  }

  colors <- c(dark = "#202123", input = "#41404e")
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
                          textarea.style.height = 'auto';
                          textarea.style.overflowY = 'hidden';
                          textarea.style.height = `${textarea.scrollHeight}px`;
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
    args <- args[setdiff(names(args), "viewer")]
    r <- reactiveValues(
      room = ChatRoom$new(chat_params = args),
      refresh = FALSE
    )

    stopUI <- reactiveVal()
    historyUI <- reactiveVal()
    outputUI <- reactiveVal()

    observe({
      if (input$chat_input != "" && isFALSE(r$refresh)) {
        disable("chat_submit")
        disable("chat_regenerate")
        disable("chat_continuous")
        r$room$chat_submit(prompt = input$chat_input, role = "user", continuous = input$chat_continuous)
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        historyUI(div_update(r$room$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    observe({
      disable("chat_submit")
      disable("chat_regenerate")
      disable("chat_continuous")
      r$room$chat_regenerate(continuous = input$chat_continuous)
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_regenerate)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")
      r$room$chat_stop()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")
      r$room$chat_clear()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      r$refresh <- !resolved(r$room$async)
      # print(rnorm(1))
      if (is.null(r$room$history)) {
        r$room$text <- "Welcome to the ChatGPT!\n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.\n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.\n\nHave a great time!"
        historyUI(div_update(r$room$history, openai_logo = openai_logo, user_logo = user_logo))
      } else {
        if (isTRUE(r$refresh)) {
          disable("chat_submit")
          disable("chat_regenerate")
          disable("chat_continuous")
          invalidateLater(50)
          stopUI(div(
            div(actionButton("chat_stop", label = "Stop generating", icon = icon("stop"), width = "150px"), style = "text-align: center;"),
            div(style = "height:10px")
          ))
          r$room$streaming()
          outputUI(gsub("\\n$", "", markdown(r$room$text)))
          # session$sendCustomMessage(type = "scrollCallback", 1)
        } else {
          enable("chat_submit")
          enable("chat_regenerate")
          enable("chat_continuous")
          stopUI(NULL)
        }
      }
      if (isFALSE(r$refresh)) {
        outputUI(gsub("\\n$", "", markdown(r$room$text)))
      }
      NULL
    })

    output$chat_output <- renderUI({
      historyUI()
    })

    output$chat_output_last <- renderUI({
      outputUI()
    })

    output$chat_stop_ui <- renderUI({
      stopUI()
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
#' @seealso
#' \code{\link{ChatGPT_gadget}} function
#' @import rstudioapi
#' @importFrom httr GET
#' @export
ChatGPT_job <- function(viewer = rstudioapi::viewer, ...) {
  args <- as.list(match.call())[-1]
  api_url <- args[["api_url"]] %||% getOption("openapi_api_url")
  api_key <- args[["api_key"]] %||% getOption("openapi_api_key")
  key_nm <- args[["key_nm"]] %||% getOption("openapi_key_nm")
  organization <- args[["organization"]] %||% getOption("openapi_organization")
  organization_nm <- args[["organization_nm"]] %||% getOption("openapi_organization_nm")

  if (is.null(api_url) || is.null(api_key)) {
    stop("api_url or api_key is not defined, please run the api_setup function to configure them.")
  }
  try_get(GET(api_url), error_message = paste0("Unable to establish a connection with api_url: ", api_url))

  jobscript <- tempfile(fileext = ".R")
  file.create(jobscript)
  writeLines(
    deparse(substitute(openapi::ChatGPT_gadget(
      viewer = viewer, api_url = api_url, api_key = api_key, key_nm = key_nm,
      organization = organization, organization_nm = organization_nm
    ))),
    con = jobscript
  )
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
  quoted_text <- deparse(substitute(selected_text))
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
