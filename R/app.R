menus_create <- function(rooms, current = NULL) {
  div(
    lapply(seq_along(rooms), function(x) {
      room_id <- paste0("room", x)
      room_nm <- rooms[[x]]
      if (identical(room_nm, current)) {
        selected <- TRUE
        style <- "display: flex; flex-direction: row; margin:0; color: black; background-color: white;"
      } else {
        selected <- FALSE
        style <- "display: flex; flex-direction: row; margin:0;"
      }
      div(
        conditionalPanel(
          condition = paste0("input.rename_chatroom", room_id, " == input.save_chatroom", room_id),
          div(menuSubItem(tabName = room_id, text = room_nm, selected = selected), style = "width:100%;margin:15px;"),
          style = "display: flex; align-items: center; flex:1;"
        ),
        conditionalPanel(
          condition = paste0("input.rename_chatroom", room_id, " != input.save_chatroom", room_id),
          textInput(inputId = paste0("newname_", room_id), label = NULL, value = room_nm, width = "100%"),
          style = "display: flex; align-items: center; flex:1;"
        ),
        conditionalPanel(
          condition = paste0("input.rename_chatroom", room_id, " != input.save_chatroom", room_id),
          tags$script(paste0("
              $(document).ready(function() {
                $('#", paste0("save_chatroom", room_id), "').click(function() {
                  $('#hidden_text').val('", x, "');
                  $('#hidden_text').trigger('change');
                  $('#hidden_button').trigger('click');
                });
              });
            ", collapse = "")),
          actionButton(inputId = paste0("save_chatroom", room_id), label = NULL, icon = icon("save"), style = "margin:5px; padding:5px;"),
          style = "display: flex; align-items: center; flex:0;"
        ),
        conditionalPanel(
          condition = paste0("input.rename_chatroom", room_id, " == input.save_chatroom", room_id),
          actionButton(inputId = paste0("rename_chatroom", room_id), label = NULL, icon = icon("edit"), style = "margin:5px; padding:5px;"),
          style = "display: flex; align-items: center; flex:0;"
        ),
        style = style
      )
    })
  )
}

#' @import shiny
#' @import shinyWidgets
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody sidebarMenu menuItem menuSubItem tabItems tabItem updateTabItems
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinymanager secure_app secure_server check_credentials create_db
#' @importFrom future plan future value resolved
#' @importFrom future.callr callr
#' @importFrom DBI dbConnect dbListTables dbReadTable dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
ChatGPT_app <- function(db = NULL, ...) {
  args <- as.list(match.call())[-1]
  args[["api_url"]] <- args[["api_url"]] %||% getOption("openapi_api_url")
  args[["api_key"]] <- args[["api_key"]] %||% getOption("openapi_api_key")
  args[["key_nm"]] <- args[["key_nm"]] %||% getOption("openapi_key_nm")
  args[["organization"]] <- args[["organization"]] %||% getOption("openapi_organization")
  args[["organization_nm"]] <- args[["organization_nm"]] %||% getOption("openapi_organization_nm")
  args <- args[setdiff(names(args), "db")]
  chat_params <- getOption("openapi_chat_params") %||% list()
  for (nm in names(args)) {
    chat_params[[nm]] <- args[[nm]]
  }

  if (is.null(chat_params[["api_url"]]) || is.null(chat_params[["api_key"]])) {
    warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
    stopApp()
  }

  # colors <- c(dark = "#202123", darkchat = "#353541", lightchat = "#4D4F5C", input = "#41404e")
  addResourcePath("lib", system.file("lib", package = "openapi"))
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_logo <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "user-icon.svg", package = "openapi")
  user_logo <- readLines(user_path, warn = FALSE)
  welcome_message <- "Welcome to the ChatGPT!
  \n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.
  \n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.
  \n\nHave a great time!"

  ui <- shinydashboardPlus::dashboardPage(
    title = "ChatGPT",
    header = dashboardHeader(title = tagList(
      tags$span(
        class = "logo-mini", div(HTML(paste0(openai_logo, collapse = "\n")), style = "padding: 20%")
      ),
      tags$span(
        class = "logo-lg", "ChatGPT"
      )
    )),
    sidebar = dashboardSidebar(
      tags$head(
        tags$style(HTML("
                      .sidebar { height: 85vh; overflow-y: auto; }
                      "))
      ),
      sidebarMenu(
        id = "menu",
        menuItem(
          text = "Chat rooms", icon = icon("comments"), startExpanded = TRUE,
          uiOutput("chatitems"),
          div(
            style = "display:flex; justify-content:space-between;",
            actionButton("add_chatroom", label = NULL, icon = icon("add"), style = "width:100px;height:30px;"),
            actionButton("remove_chatroom", label = NULL, icon = icon("minus"), style = "width:100px;height:30px;"),
            div(
              style = "display:none;",
              actionButton("hidden_button", label = NULL),
              textInput("hidden_text", value = NULL, label = NULL)
            )
          )
        )
      )
    ),
    controlbar = dashboardControlbar(id = "controlbar"),
    skin = "black-light",
    dashboardBody(
      useShinyjs(),
      tags$style(
        HTML(
          "
          .box {
          background: #F5F5F5;
          border-radius: 5px;
          max-width: 1280px;
          position: relative;
          left: 50%;
          transform: translateX(-50%);
          }
          "
        )
      ),
      div(
        style = "display:none;",
        actionButton("hidden_repeat_button", label = NULL),
        textInput("hidden_repeat_text", value = NULL, label = NULL)
      ),
      box(
        width = 12,
        title = "Chat Room",
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        label = boxLabel(
          text = "Parameters",
          status = "warning"
        ),
        sidebar = boxSidebar(
          id = "chat_params",
          startOpen = FALSE,
          width = 25,
          pickerInput(
            inputId = "chat_model",
            label = "Model:",
            choices = c("gpt-3.5-turbo", "gpt-4"),
            selected = "gpt-3.5-turbo"
          ),
          sliderTextInput(
            inputId = "chat_temperature",
            label = "Temperature:",
            selected = 1,
            choices = seq(0, 2, length.out = 21),
            grid = TRUE
          ),
          sliderTextInput(
            inputId = "chat_presence_penalty",
            label = "Presence penalty:",
            selected = 0,
            choices = seq(-2, 2, length.out = 21),
            grid = TRUE
          ),
          sliderTextInput(
            inputId = "chat_frequency_penalty",
            label = "Frequency penalty:",
            selected = 0,
            choices = seq(-2, 2, length.out = 21),
            grid = TRUE
          )
        ),
        fillPage(
          tags$style(type = "text/css", "#chat_output_container {height: calc(100vh - 290px) !important;}"),
          div(
            id = "chat_output_container", style = "overflow-y: auto;",
            uiOutput("chat_output"),
            div(style = "height:30px;")
          ),
          div(
            id = "chat_input_container", style = "bottom: 0;",
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
                  tags$head(
                    tags$style(HTML("
                      .form-control:focus {
                        box-shadow: 0px 0px 5px 1px #69666c;
                      }
                    "))
                  ),
                  tags$head(
                    tags$style(
                      "#chat_input{color:white; background: #41404e; font-size:12px;
                        height:auto; min-height:100px; max-height: 100%;
                        white-space: normal; overflow-wrap: break-word;}"
                    )
                  ),
                  tags$script("
                      var textarea = document.getElementById(\"chat_input\");
                      textarea.addEventListener(\"input\", function() {
                          textarea.style.height = 'auto';
                          textarea.style.overflowY = 'hidden';
                          textarea.style.height = `${textarea.scrollHeight}px`;
                      });"),
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
                "data-proxy-click" = "chat_submit"
              ),
              div(
                materialSwitch("chat_continuous", label = "Continuous", status = "success", value = TRUE, width = "130px"),
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

  if (!is.null(db)) {
    if (!grepl(pattern = "\\.sqlite$", x = db)) {
      stop("'db' must end with .sqlite")
    }
    if (!file.exists(db)) {
      credentials <- data.frame(
        user = "root",
        password = "123456",
        expire = NA,
        admin = TRUE,
        stringsAsFactors = FALSE
      )
      create_db(
        credentials_data = credentials,
        sqlite_path = db
      )
    }
    ui <- secure_app(ui, enable_admin = TRUE, theme = "darkly", language = "en")
  }

  server <- function(input, output, session) {
    disable("chat_model")

    if (!is.null(db)) {
      res_auth <- secure_server(check_credentials(db = db))
      con <- dbConnect(SQLite(), db)
    } else {
      res_auth <- reactiveValues()
    }

    r <- reactiveValues(
      rooms = ChatRooms$new(chat_params = chat_params),
      refresh = FALSE
    )
    menuUI <- reactiveVal(menus_create(names(isolate(r$rooms$rooms)), current = names(isolate(r$rooms$rooms))[1]))
    historyUI <- reactiveVal(div_update(NULL, openai_logo = openai_logo, user_logo = user_logo))
    outputUI <- reactiveVal()
    stopUI <- reactiveVal()

    observe({
      if (!is.null(db)) {
        user <- res_auth[["user"]]
        if (paste0(user, "_data") %in% dbListTables(con)) {
          message("Loading data for user: ", user, "...")
          userdata <- dbReadTable(con, paste0(user, "_data"))
          if (nrow(userdata) > 0) {
            r$rooms$rooms <- NULL
            # cat("loading data", unique(userdata[["room"]]), "\n")
            data_rooms <- split.data.frame(userdata, userdata[["room"]])
            for (room_nm in unique(userdata[["room"]])) {
              room <- data_rooms[[room_nm]]
              stream_file <- room[["stream_file"]][1]
              messages <- apply(room[, c("role", "content", "time"), drop = FALSE], 1, as.list)
              names(messages) <- NULL
              if (length(messages) == 1 && is.na(messages[[1]][["content"]])) {
                messages <- NULL
              }
              r$rooms$room_add(name = room_nm, chat_params = chat_params, messages = messages, stream_file = stream_file)
              if (length(messages) > 1 && messages[[length(messages)]][["role"]] == "assistant") {
                r$rooms$rooms[[room_nm]]$text <- messages[[length(messages)]][["content"]]
              }
              r$rooms$current <- names(r$rooms$rooms)[1]
            }
          }
        }
        menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
      }
      NULL
    })

    observe({
      r$rooms$room_add(chat_params = chat_params)
      menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
      historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$add_chatroom)

    observe({
      if (length(r$rooms$rooms) > 1) {
        r$rooms$room_remove()
        menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$remove_chatroom)

    observe({
      room_id <- input$menu %||% names(r$rooms$rooms)[1]
      r$rooms$current <- names(r$rooms$rooms)[as.numeric(gsub("room", "", room_id))]
      menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
      if (is.null(r$rooms$room_current()$history)) {
        r$rooms$rooms[[r$rooms$current]]$text <- welcome_message
        historyUI(div_update(NULL, openai_logo = openai_logo, user_logo = user_logo))
        outputUI(gsub("\\n$", "", markdown(r$rooms$room_current()$text)))
      } else {
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
        outputUI(gsub("\\n$", "", markdown(r$rooms$room_current()$text)))
      }
      NULL
    }) %>% bindEvent(input$menu)

    observe({
      id <- as.numeric(input$hidden_text)
      room_id <- paste0("room", id)
      if (r$rooms$current == names(r$rooms$rooms)[id]) {
        r$rooms$current <- input[[paste0("newname_", room_id)]]
      }
      names(r$rooms$rooms)[id] <- input[[paste0("newname_", room_id)]]
      menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
      NULL
    }) %>% bindEvent(input$hidden_button)

    observe({
      if (input$chat_input != "" && isFALSE(r$refresh)) {
        disable("chat_submit")
        disable("chat_continuous")
        disable("hidden_repeat_button")
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["temperature"]] <- input$chat_temperature
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["presence_penalty"]] <- input$chat_presence_penalty
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["frequency_penalty"]] <- input$chat_frequency_penalty
        r$rooms$room_current()$chat_submit(prompt = input$chat_input, role = "user", continuous = input$chat_continuous)
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    confirmation <- reactiveVal(FALSE)
    observe({
      if (!is.null(r$rooms$room_current()$history)) {
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
      NULL
    }) %>% bindEvent(input$confirmation)

    observe({
      if (isTRUE(confirmation())) {
        index <- as.numeric(input$hidden_repeat_text)
        if (is.na(index)) {
          index <- NULL
        }
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["temperature"]] <- input$chat_temperature
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["presence_penalty"]] <- input$chat_presence_penalty
        r$rooms$rooms[[r$rooms$current]]$chat$chat_params[["frequency_penalty"]] <- input$chat_frequency_penalty
        r$rooms$room_current()$chat_regenerate(index = index, continuous = input$chat_continuous)
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(confirmation())

    observe({
      enable("chat_submit")
      enable("chat_continuous")
      enable("hidden_repeat_button")
      r$rooms$room_current()$chat_stop()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_continuous")
      enable("hidden_repeat_button")
      r$rooms$room_current()$chat_clear()
      historyUI(div_update(NULL, openai_logo = openai_logo, user_logo = user_logo))
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      r$refresh <- !resolved(r$rooms$room_current()$async)
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
      r$rooms$room_current()$streaming()
      if (is.null(r$rooms$room_current()$history)) {
        r$rooms$rooms[[r$rooms$current]]$text <- welcome_message
      }
      outputUI(gsub("\\n$", "", markdown(r$rooms$room_current()$text)))
      NULL
    })

    output$chatitems <- renderUI({
      menuUI()
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

    session$onSessionEnded(
      function() {
        user <- isolate(res_auth[["user"]])
        rooms <- isolate(r$rooms$rooms)
        if (!is.null(db) & !is.null(user)) {
          data_list <- lapply(names(rooms), function(nm) {
            room <- rooms[[nm]]
            if (is.null(room$history) || length(room$history) == 0) {
              data <- data.frame(
                role = "assistant", content = NA, time = NA,
                stream_file = room$stream_file, room = nm
              )
            } else {
              data <- do.call(rbind.data.frame, room$history)
              data[["stream_file"]] <- room$stream_file
              data[["room"]] <- nm
            }
            return(data)
          })
          data_all <- do.call(rbind.data.frame, data_list)
          if (nrow(data_all) > 0) {
            # cat("saving data", unique(data_all[["room"]]), "\n")
            dbWriteTable(con, name = paste0(user, "_data"), value = data_all, overwrite = TRUE)
          }
          dbDisconnect(con)
        }
      }
    )

    session$allowReconnect(TRUE)
  }

  shinyApp(ui = ui, server = server)
}

# ChatGPT_app()
# ChatGPT_app(db = "chatgpt.sqlite")
