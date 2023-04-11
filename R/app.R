#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody sidebarMenu menuItem menuSubItem tabItems tabItem updateTabItems
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinymanager secure_app secure_server check_credentials
#' @importFrom future plan future value resolved
#' @importFrom future.callr callr
#' @export
ChatGPT_app <- function(credentials = NULL, api_url = NULL, api_key = NULL, organization = NULL, ...) {
  colors <- c(dark = "#202123", darkchat = "#353541", lightchat = "#4D4F5C", input = "#41404e")
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_logo <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "my-account-icon.svg", package = "openapi")
  user_logo <- readLines(user_path, warn = FALSE)

  ui <- dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "Menu",
        menuItem("Chat rooms",
          tabName = "chatrooms", icon = icon("comments"),
          uiOutput("chatitems"),
          div(
            actionButton("addchat", label = "", icon = icon("add"), style = "width:100px;height:30px;"),
            actionButton("removechat", label = "", icon = icon("minus"), style = "width:100px;height:30px;"),
            style = "display:flex; justify-content:space-between;"
          )
        )
      )
    ),
    controlbar = dashboardControlbar(id = "controlbar"),
    skin = "blue",
    dashboardBody(
      tabItems(
        tabItem(
          "chat1",
          useShinyjs(),
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
          fillCol(
            flex = NA,
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
      )
    )
  )

  if (!is.null(credentials)) {
    ui <- secure_app(ui, theme = "darkly")
  }

  server <- function(input, output, session) {
    if (!is.null(credentials)) {
      res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
      )
    }

    plan(callr)

    api_url <- api_url %||% getOption("openapi_api_url")
    api_key <- api_key %||% getOption("openapi_api_key")
    organization <- organization %||% getOption("openapi_organization")
    if (is.null(api_url) || is.null(api_key)) {
      warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
      stopApp()
    }

    stream_file <- tempfile(fileext = ".txt")
    file.create(stream_file)
    onStop(function() {
      message("Remove stream file: ", stream_file)
      unlink(stream_file)
    })

    r <- reactiveValues(
      room = "chat1",
      chatrooms = list(chat1 = list(chat = ChatGPT$new(), messages = NULL, text = NULL, async = NULL, processing = FALSE, stop = NULL, stream_file = stream_file)),
    )

    observe({
      stream_file <- tempfile(fileext = ".txt")
      file.create(stream_file)
      onStop(function() {
        message("Remove stream file: ", stream_file)
        unlink(stream_file)
      })
      ids <- sapply(names(r[["chatrooms"]]), function(x) gsub("(^chat)(\\d+)", "\\2", x))
      r[["room"]] <- paste0("chat", max(as.numeric(ids)) + 1)
      r[["chatrooms"]][[r[["room"]]]] <- list(chat = ChatGPT$new(), messages = NULL, text = NULL, async = NULL, processing = FALSE, stop = NULL, stream_file = stream_file)
      updateTabItems(session, inputId = "Menu", selected = r[["room"]])
    }) %>% bindEvent(input$addchat)

    observe({
      room_remain <- setdiff(names(r[["chatrooms"]]), r[["room"]])
      if (length(room_remain) >= 1) {
        r[["chatrooms"]][[r[["room"]]]] <- NULL
        r[["room"]] <- room_remain[length(room_remain)]
        stream_file <- r[["chatrooms"]][[r[["room"]]]][["stream_file"]]
        message("Remove stream file: ", stream_file)
        unlink(stream_file)
        updateTabItems(session, inputId = "Menu", selected = r[["room"]])
      }
    }) %>% bindEvent(input$removechat)

    output$chatitems <- renderUI({
      div(
        lapply(names(r[["chatrooms"]]), function(x) {
          div(
            div(style = "height:5px;"),
            menuSubItem(x, tabName = x),
            div(style = "height:5px;")
          )
        })
      )
    })

    observe({
      r[["room"]] <- input[["Menu"]] %||% "chat1"
      # print(r[["room"]])
    })

    observe({
      if (input$chat_input != "" && isFALSE(r[["chatrooms"]][[r[["room"]]]][["processing"]])) {
        disable("chat_submit")
        disable("chat_regenerate")
        disable("chat_continuous")

        chat_input <- input$chat_input
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        r[["chatrooms"]][[r[["room"]]]][["messages"]] <- c(r[["chatrooms"]][[r[["room"]]]][["messages"]], list(list("role" = "user", "content" = chat_input, "time" = as.character(Sys.time()))))
        writeLines("", r[["chatrooms"]][[r[["room"]]]][["stream_file"]])

        rchat <- r[["chatrooms"]][[r[["room"]]]][["chat"]]
        chat_continuous <- input$chat_continuous
        stream_file <- r[["chatrooms"]][[r[["room"]]]][["stream_file"]]
        r[["chatrooms"]][[r[["room"]]]][["async"]] <- future(rchat$chat(chat_input,
          stream = TRUE,
          stream_file = stream_file,
          continuous = chat_continuous,
          api_url = api_url,
          api_key = api_key,
          organization = organization,
          ...
        ))
        r[["chatrooms"]][[r[["room"]]]][["processing"]] <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    observe({
      disable("chat_submit")
      disable("chat_regenerate")
      disable("chat_continuous")

      if (inherits(r[["chatrooms"]][[r[["room"]]]][["async"]], "Future")) {
        r[["chatrooms"]][[r[["room"]]]][["chat"]] <- value(r[["chatrooms"]][[r[["room"]]]][["async"]])
      }
      writeLines("", r[["chatrooms"]][[r[["room"]]]][["stream_file"]])

      r[["chatrooms"]][[r[["room"]]]][["messages"]] <- r[["chatrooms"]][[r[["room"]]]][["messages"]][-length(r[["chatrooms"]][[r[["room"]]]][["messages"]])]
      rchat <- r[["chatrooms"]][[r[["room"]]]][["chat"]]
      rcontinuous <- input$chat_continuous
      stream_file <- r[["chatrooms"]][[r[["room"]]]][["stream_file"]]
      r[["chatrooms"]][[r[["room"]]]][["async"]] <- future(rchat$regenerate(
        stream = TRUE,
        stream_file = stream_file,
        continuous = rcontinuous,
        api_url = api_url,
        api_key = api_key,
        organization = organization,
        ...
      ))
      r[["chatrooms"]][[r[["room"]]]][["processing"]] <- TRUE
      NULL
    }) %>% bindEvent(input$chat_regenerate)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")

      if (inherits(r[["chatrooms"]][[r[["room"]]]][["async"]], "Future")) {
        r[["chatrooms"]][[r[["room"]]]][["async"]]$process$kill()
        r[["chatrooms"]][[r[["room"]]]][["text"]] <- paste0(r[["chatrooms"]][[r[["room"]]]][["text"]], "[The message was interrupted]")
        r[["chatrooms"]][[r[["room"]]]][["messages"]] <- c(r[["chatrooms"]][[r[["room"]]]][["messages"]], list(list("role" = "assistant", "content" = r[["chatrooms"]][[r[["room"]]]][["text"]], "time" = as.character(Sys.time()))))
        r[["chatrooms"]][[r[["room"]]]][["chat"]]$messages <- r[["chatrooms"]][[r[["room"]]]][["messages"]]
        r[["chatrooms"]][[r[["room"]]]][["chat"]]$index <- length(r[["chatrooms"]][[r[["room"]]]][["chat"]]$messages)
      }
      r[["chatrooms"]][[r[["room"]]]][["async"]] <- NULL
      r[["chatrooms"]][[r[["room"]]]][["processing"]] <- FALSE
      r[["chatrooms"]][[r[["room"]]]][["stop"]] <- NULL
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")

      if (inherits(r[["chatrooms"]][[r[["room"]]]][["async"]], "Future")) {
        r[["chatrooms"]][[r[["room"]]]][["async"]]$process$kill()
      }
      r[["chatrooms"]][[r[["room"]]]][["async"]] <- NULL
      r[["chatrooms"]][[r[["room"]]]][["processing"]] <- FALSE
      r[["chatrooms"]][[r[["room"]]]][["stop"]] <- NULL
      r[["chatrooms"]][[r[["room"]]]][["chat"]] <- ChatGPT$new()
      r[["chatrooms"]][[r[["room"]]]][["messages"]] <- NULL
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      if (is.null(r[["chatrooms"]][[r[["room"]]]][["messages"]])) {
        text <- "Welcome to the ChatGPT!\n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.\n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.\n\nHave a great time!"
      } else {
        if (isTRUE(r[["chatrooms"]][[r[["room"]]]][["processing"]])) {
          r[["chatrooms"]][[r[["room"]]]][["stop"]] <- div(
            div(actionButton("chat_stop", label = "Stop generating", icon = icon("stop"), width = "150px"), style = "text-align: center;"),
            div(style = "height:10px")
          )
          invalidateLater(50)
          text <- readLines(r[["chatrooms"]][[r[["room"]]]][["stream_file"]], warn = FALSE)
          if (identical(text, "") || length(text) == 0) {
            text <- "..."
          }
          if (identical(text[length(text)], "data: [DONE]") || (resolved(r[["chatrooms"]][[r[["room"]]]][["async"]]) && !is.null(r[["chatrooms"]][[r[["room"]]]][["async"]]))) {
            # print(text)
            # print(resolved(r[["chatrooms"]][[r[["room"]]]][["async"]]))
            # print(value(r[["chatrooms"]][[r[["room"]]]][["async"]]))

            if (identical(text[length(text)], "data: [DONE]")) {
              text <- text[-length(text)]
            }
            if (inherits(r[["chatrooms"]][[r[["room"]]]][["async"]], "Future")) {
              r[["chatrooms"]][[r[["room"]]]][["chat"]] <- value(r[["chatrooms"]][[r[["room"]]]][["async"]])
              new_messages <- r[["chatrooms"]][[r[["room"]]]][["chat"]]$messages[length(r[["chatrooms"]][[r[["room"]]]][["chat"]]$messages)]
              new_messages[[1]][["time"]] <- as.character(Sys.time())
              r[["chatrooms"]][[r[["room"]]]][["messages"]] <- c(r[["chatrooms"]][[r[["room"]]]][["messages"]], new_messages)
            }
            r[["chatrooms"]][[r[["room"]]]][["processing"]] <- FALSE
            r[["chatrooms"]][[r[["room"]]]][["stop"]] <- NULL
            enable("chat_submit")
            enable("chat_regenerate")
            enable("chat_continuous")
          }
          # session$sendCustomMessage(type = "scrollCallback", 1)
        } else {
          text <- r[["chatrooms"]][[r[["room"]]]][["text"]]
        }
      }
      # print(text)
      r[["chatrooms"]][[r[["room"]]]][["text"]] <- gsub("\\n$", "", markdown(paste0(text, collapse = "\n")))
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
          div_create(r[["chatrooms"]][[r[["room"]]]][["messages"]], openai_logo = openai_logo, user_logo = user_logo)
        )
      )
    })

    output$chat_output_last <- renderUI({
      r[["chatrooms"]][[r[["room"]]]][["text"]]
    })

    output$chat_stop_ui <- renderUI({
      r[["chatrooms"]][[r[["room"]]]][["stop"]]
    })
  }

  shinyApp(ui = ui, server = server)
}
