div_create <- function(messages, openai_logo, user_logo) {
  # print(rnorm(1))
  divs <- list()
  time_chat_output <- NULL
  if (length(messages) > 0) {
    content <- sapply(messages, function(x) x[["content"]])
    role <- sapply(messages, function(x) x[["role"]])
    time <- sapply(messages, function(x) x[["time"]])

    # remove assistant's last message
    if (role[length(messages)] == "assistant") {
      content <- content[-length(messages)]
      time_chat_output <- time[length(messages)]
    }

    # create HTML divs for each message
    for (i in seq_along(content)) {
      if (role[i] == "user") {
        divs <- c(divs, list(
          div(
            style = "display: flex; flex-direction: row; justify-content: flex-end;",
            div(
              div(style = "height:20px; text-align: right;", time[i] %||% as.character(Sys.time())),
              div(
                style = "display: flex; flex-direction: row; align-items: end; justify-content: flex-end;",
                actionButton(
                  inputId = paste0("copy", "input_", i), label = NULL, icon = icon("copy"),
                  class = "clipboardButton", `data-clipboard-target` = paste0("#chat_input", i),
                  onmouseover = "this.style.borderColor='black';this.style.opacity=1;",
                  onmouseout = "this.style.borderColor='transparent';this.style.opacity=0.3;",
                  style = "opacity: 0.3;margin:1px; padding:2px;background-color:transparent;border-color:transparent;"
                ),
                div(style = "text-align: right;", div(class = "chat_input", id = paste0("chat_input", i), gsub("\\n$", "", markdown(content[i]))))
              )
            ),
            div(
              style = "display: flex; flex-direction: row; justify-content: flex-end;",
              div(style = "width:5px;"),
              div(style = "width:30px; height:30px;", HTML(paste0(user_logo, collapse = "\n")))
            )
          ),
          div(style = "height:20px;")
        ))
      } else {
        divs <- c(divs, list(
          div(
            style = "display: flex; flex-direction: row;",
            div(
              style = "display: flex; flex-direction: row;",
              div(style = "width:30px; height:30px;", HTML(paste0(openai_logo, collapse = "\n"))),
              div(style = "width:5px;")
            ),
            div(
              div(style = "height:20px;", time[i] %||% as.character(Sys.time())),
              div(
                style = "display: flex; flex-direction: row; align-items: end;",
                div(style = "text-align: left;", div(class = "chat_output", id = paste0("chat_output", i), gsub("\\n$", "", markdown(content[i])))),
                actionButton(
                  inputId = paste0("copy", "output_", i), label = NULL, icon = icon("copy"),
                  class = "clipboardButton", `data-clipboard-target` = paste0("#chat_output", i),
                  onmouseover = "this.style.borderColor='black';this.style.opacity=1;",
                  onmouseout = "this.style.borderColor='transparent';this.style.opacity=0.3;",
                  style = "opacity: 0.3;margin:1px; padding:2px;background-color:transparent;border-color:transparent;"
                ),
                tags$script(paste0("
              $(document).ready(function() {
                $('#", paste0("repeat", "output_", i), "').click(function() {
                  $('#hidden_repeat_text').val('", i, "');
                  $('#hidden_repeat_text').trigger('change');
                  $('#hidden_repeat_button').trigger('click');
                });
              });
            ", collapse = "")),
                actionButton(
                  inputId = paste0("repeat", "output_", i), label = NULL, icon = icon("repeat"),
                  onmouseover = "this.style.borderColor='black';this.style.opacity=1;",
                  onmouseout = "this.style.borderColor='transparent';this.style.opacity=0.3;",
                  style = "opacity: 0.3;margin:1px; padding:2px;background-color:transparent;border-color:transparent;"
                )
              )
            )
          ),
          div(style = "height:20px;")
        ))
      }
    }
  }

  # add final HTML div for ChatGPT output
  divs <- c(divs, list(
    div(
      style = "display: flex; flex-direction: row;",
      div(
        style = "display: flex; flex-direction: row;",
        div(
          style = "width:30px; height:30px;",
          HTML(paste0(openai_logo, collapse = "\n"))
        ),
        div(style = "width:5px;")
      ),
      div(
        div(style = "height:20px;", time_chat_output %||% as.character(Sys.time())),
        div(
          style = "display: flex; flex-direction: row; align-items: end;",
          div(style = "text-align: left;", uiOutput(outputId = "chat_output_last", class = "chat_output")),
          actionButton(
            inputId = paste0("copy", "output_last"), label = NULL, icon = icon("copy"),
            class = "clipboardButton", `data-clipboard-target` = "#chat_output_last",
            onmouseover = "this.style.borderColor='black';this.style.opacity=1;",
            onmouseout = "this.style.borderColor='transparent';this.style.opacity=0.3;",
            style = "opacity: 0.3;margin:1px; padding:2px;background-color:transparent;border-color:transparent;"
          ),
          tags$script(paste0("
              $(document).ready(function() {
                $('#", paste0("repeat", "output_last"), "').click(function() {
                  $('#hidden_repeat_text').val('", NULL, "');
                  $('#hidden_repeat_text').trigger('change');
                  $('#hidden_repeat_button').trigger('click');
                });
              });
            ", collapse = "")),
          actionButton(
            inputId = paste0("repeat", "output_last"), label = NULL, icon = icon("repeat"),
            onmouseover = "this.style.borderColor='black';this.style.opacity=1;",
            onmouseout = "this.style.borderColor='transparent';this.style.opacity=0.3;",
            style = "opacity: 0.3;margin:1px; padding:2px;background-color:transparent;border-color:transparent;"
          )
        )
      )
    ),
    div(style = "height:20px;")
  ))

  return(divs)
}

div_update <- function(messages, openai_logo, user_logo) {
  fluidPage(
    tagList(
      tags$head(tags$style(
        paste0(".chat_input {background-color: #95EC69;
                    padding: 10px 10px 0 10px; border-radius: 5px; text-align: right;
                    white-space: normal; overflow-wrap: break-word; display: inline-block;}")
      )),
      tags$head(tags$style(
        paste0(".chat_output {background-color: #FFFFFF;
                   padding: 10px 10px 0 10px; border-radius: 5px; text-align: left;
                   white-space: normal; overflow-wrap: break-word; display: inline-block;}")
      )),
      tags$head(
        tags$script(src = "lib/clipboard.min.js"),
        tags$script(HTML(
          "$(document).ready(function(){new ClipboardJS('.clipboardButton');});"
        ))
      ),
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
  args <- args[setdiff(names(args), "viewer")]
  chat_params <- getOption("openapi_chat_params") %||% list()
  for (nm in names(args)) {
    chat_params[[nm]] <- args[[nm]]
  }

  if (is.null(chat_params[["api_url"]]) || is.null(chat_params[["api_key"]])) {
    warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
    stopApp()
  }

  addResourcePath("lib", system.file("lib", package = "openapi"))
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_logo <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "user-icon.svg", package = "openapi")
  user_logo <- readLines(user_path, warn = FALSE)
  welcome_message <- "Welcome to the ChatGPT!
  \n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.
  \n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.
  \n\nHave a great time!"

  ui <- miniPage(
    useShinyjs(),
    id = "addin",
    # tags$style(
    #   type = "text/css",
    #   paste0("#addin { background-color: #4D4F5C; }"),
    # ),
    miniTitleBar(tags$strong("ChatGPT"), right = miniTitleBarButton("exit", label = "Exit")),
    miniTabstripPanel(
      miniTabPanel("Chat",
        icon = icon("comments"),
        tags$head(
          tags$style(
            paste0("#chat_input{color:white; background: #41404e; font-size:12px;
                    height:auto; min-height:100px; max-height: 100%;
                    white-space: normal; overflow-wrap: break-word;}")
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
          div(
            style = "display:none;",
            actionButton("hidden_repeat_button", label = NULL),
            textInput("hidden_repeat_text", value = NULL, label = NULL)
          ),
          fillCol(
            flex = c(1, NA),
            div(
              id = "chat_output_container", style = "background: #F5F5F5; border-radius: 5px; padding: 10px 10px 10px 0;
              height: 100%; max-height: 100%; overflow-y: auto;",
              uiOutput("chat_output"),
              div(style = "height:30px;")
            ),
            div(
              id = "chat_input_container", style = "bottom: 0; height: 100%; max-height: 100%; width: 100%;",
              uiOutput("chat_stop_ui", style = "position: absolute; bottom: 100px; left: 50%;transform: translate(-50%, -50%); z-index: 999;"),
              div(style = "border-top: 2px solid #202123;"),
              div(style = "height:10px;"),
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
                  materialSwitch("chat_continuous", label = "Continuous", status = "success", value = TRUE, width = "130px"),
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
    r <- reactiveValues(
      room = ChatRoom$new(chat_params = chat_params),
      refresh = FALSE
    )

    historyUI <- reactiveVal(div_update(NULL, openai_logo = openai_logo, user_logo = user_logo))
    outputUI <- reactiveVal()
    stopUI <- reactiveVal()

    observe({
      if (input$chat_input != "" && isFALSE(r$refresh)) {
        disable("chat_submit")
        disable("chat_continuous")
        disable("hidden_repeat_button")
        r$room$chat_submit(prompt = input$chat_input, role = "user", continuous = input$chat_continuous)
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        historyUI(div_update(r$room$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    confirmation <- reactiveVal(FALSE)
    observe({
      if (!is.null(r$room$history)) {
        disable("chat_submit")
        disable("chat_continuous")
        disable("hidden_repeat_button")
        confirmation(FALSE)
        index <- as.numeric(input$hidden_repeat_text)
        if (is.na(index)) {
          index <- NULL
          confirmation(TRUE)
        } else {
          ask_confirmation(
            inputId = "confirmation",
            text = "You'll lose all subsequent conversations.\nConfirm to regenerate this conversation?",
            type = "warning",
            btn_labels = c("No", "Yes"),
            btn_colors = c("#6e7d88", "#04B404")
          )
        }
      }
      NULL
    }) %>% bindEvent(input$hidden_repeat_button)

    observe({
      if (isTRUE(input$confirmation)) {
        confirmation(TRUE)
      } else {
        confirmation(FALSE)
      }
    }) %>% bindEvent(input$confirmation)

    observe({
      if (isTRUE(confirmation())) {
        index <- as.numeric(input$hidden_repeat_text)
        if (is.na(index)) {
          index <- NULL
        }
        r$room$chat_regenerate(index = index, continuous = input$chat_continuous)
        historyUI(div_update(r$room$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(confirmation())

    observe({
      enable("chat_submit")
      enable("chat_continuous")
      enable("hidden_repeat_button")
      r$room$chat_stop()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_continuous")
      enable("hidden_repeat_button")
      r$room$chat_clear()
      historyUI(div_update(NULL, openai_logo = openai_logo, user_logo = user_logo))
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      r$refresh <- !resolved(r$room$async)
      if (isTRUE(r$refresh)) {
        disable("chat_submit")
        disable("chat_continuous")
        disable("hidden_repeat_button")
        invalidateLater(50)
        stopUI(div(
          actionButton("chat_stop", label = "Stop generating", icon = icon("stop"), width = "150px", class = "btn-danger", style = "color:white;text-align: center;"),
          div(style = "height:10px")
        ))
      } else {
        enable("chat_submit")
        enable("chat_continuous")
        enable("hidden_repeat_button")
        stopUI(NULL)
      }
      r$room$streaming()
      if (is.null(r$room$history)) {
        r$room$text <- welcome_message
      }
      outputUI(gsub("\\n$", "", markdown(r$room$text)))
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
  args[["api_url"]] <- args[["api_url"]] %||% getOption("openapi_api_url")
  args[["api_key"]] <- args[["api_key"]] %||% getOption("openapi_api_key")
  args[["key_nm"]] <- args[["key_nm"]] %||% getOption("openapi_key_nm")
  args[["organization"]] <- args[["organization"]] %||% getOption("openapi_organization")
  args[["organization_nm"]] <- args[["organization_nm"]] %||% getOption("openapi_organization_nm")
  args <- args[setdiff(names(args), "viewer")]
  chat_params <- getOption("openapi_chat_params") %||% list()
  for (nm in names(args)) {
    chat_params[[nm]] <- args[[nm]]
  }

  if (is.null(chat_params[["api_url"]]) || is.null(chat_params[["api_key"]])) {
    stop("api_url or api_key is not defined, please run the api_setup function to configure them.")
  }
  try_get(GET(chat_params[["api_url"]]), error_message = paste0("Unable to establish a connection with api_url: ", chat_params[["api_url"]]))

  jobscript <- tempfile(fileext = ".R")
  file.create(jobscript)
  writeLines(
    deparse(substitute(
      do.call(openapi::ChatGPT_gadget, args = c(list(viewer = viewer), chat_params))
      #   openapi::ChatGPT_gadget(
      #   viewer = viewer, api_url = api_url, api_key = api_key, key_nm = key_nm,
      #   organization = organization, organization_nm = organization_nm
      # )
    )),
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
