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
        div(
          conditionalPanel(
            condition = paste0("input.rename_chatroom", room_id, " == input.save_chatroom", room_id),
            div(menuSubItem(tabName = room_id, text = room_nm, selected = selected), style = "width:100%;margin:15px;"),
            style = "display: flex; align-items: center;"
          ),
          conditionalPanel(
            condition = paste0("input.rename_chatroom", room_id, " != input.save_chatroom", room_id),
            textInput(inputId = paste0("newname_", room_id), label = NULL, value = room_nm, width = "100%"),
            style = "display: flex; align-items: center;"
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
            style = "display: flex; align-items: center;"
          ),
          conditionalPanel(
            condition = paste0("input.rename_chatroom", room_id, " == input.save_chatroom", room_id),
            actionButton(inputId = paste0("rename_chatroom", room_id), label = NULL, icon = icon("edit"), style = "margin:5px; padding:5px;"),
            style = "display: flex; align-items: center;"
          ),
          style = style
        )
      )
    })
  )
}

#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody sidebarMenu menuItem menuSubItem tabItems tabItem updateTabItems
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom shinyWidgets materialSwitch
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

  if (is.null(args[["api_url"]]) || is.null(args[["api_key"]])) {
    warning("api_url or api_key is not defined, please run the api_setup function to configure them.")
    stopApp()
  }

  colors <- c(dark = "#202123", darkchat = "#353541", lightchat = "#4D4F5C", input = "#41404e")
  openai_path <- system.file("icons", "openai-icon.svg", package = "openapi")
  openai_logo <- readLines(openai_path, warn = FALSE)
  user_path <- system.file("icons", "my-account-icon.svg", package = "openapi")
  user_logo <- readLines(user_path, warn = FALSE)

  ui <- shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
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
      tabItems(
        tabItem(
          tabName = "room1",
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
          box(
            width = 12,
            title = "Chat Room",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
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
    if (!is.null(db)) {
      res_auth <- secure_server(check_credentials(db = db))
      con <- dbConnect(SQLite(), db)
      onStop(function() dbDisconnect(con))
    }

    args <- args[setdiff(names(args), "db")]
    r <- reactiveValues(
      rooms = ChatRooms$new(chat_params = args),
      refresh = FALSE
    )

    menuUI <- reactiveVal(menus_create(names(isolate(r$rooms$rooms))))
    stopUI <- reactiveVal()
    historyUI <- reactiveVal()
    outputUI <- reactiveVal()

    observe({
      if (!is.null(db)) {
        # print("loading data")
        user <- res_auth[["user"]]
        if (paste0(user, "_data") %in% dbListTables(con)) {
          message("Loading data for user: ", user, "...")
          userdata <- dbReadTable(con, paste0(user, "_data"))
          if (nrow(userdata) > 0) {
            data_rooms <- split.data.frame(userdata, userdata[["room"]])
            for (room in data_rooms) {
              room_nm <- room[["room"]][1]
              stream_file <- room[["stream_file"]][1]
              messages <- apply(room[, c("role", "content", "time"), drop = FALSE], 1, as.list)
              names(messages) <- NULL
              r$rooms$room_add(name = room_nm, chat_params = args, messages = messages, stream_file = stream_file)
              if (length(messages) > 1 && messages[[length(messages)]][["role"]] == "assistant") {
                r$rooms$rooms[[room_nm]]$text <- messages[[length(messages)]][["content"]]
              }
            }
          }
        }
        menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
        r$refresh <- TRUE
      }
      NULL
    })

    observe({
      r$rooms$room_add(chat_params = args)
      menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$add_chatroom)

    observe({
      if (length(r$rooms$rooms) > 1) {
        r$rooms$room_remove()
        menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$remove_chatroom)

    observe({
      r$rooms$current <- names(r$rooms$rooms)[as.numeric(gsub("room", "", input$menu))]
      menuUI(menus_create(names(r$rooms$rooms), current = r$rooms$current))
      historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
      outputUI(gsub("\\n$", "", markdown(paste0(r$rooms$room_current()$text, collapse = "\n"))))
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

    output$chatitems <- renderUI({
      menuUI()
    })

    observe({
      if (input$chat_input != "" && isFALSE(r$refresh)) {
        disable("chat_submit")
        disable("chat_regenerate")
        disable("chat_continuous")
        r$rooms$room_current()$chat_submit(prompt = input$chat_input, role = "user", continuous = input$chat_continuous)
        updateTextAreaInput(session, inputId = "chat_input", value = "")
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
        r$refresh <- TRUE
      }
      NULL
    }) %>% bindEvent(input$chat_submit)

    observe({
      disable("chat_submit")
      disable("chat_regenerate")
      disable("chat_continuous")
      r$rooms$room_current()$chat_regenerate(continuous = input$chat_continuous)
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_regenerate)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")
      r$rooms$room_current()$chat_stop()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_stop)

    observe({
      enable("chat_submit")
      enable("chat_regenerate")
      enable("chat_continuous")
      r$rooms$room_current()$chat_clear()
      r$refresh <- TRUE
      NULL
    }) %>% bindEvent(input$chat_clear)

    observe({
      r$refresh <- !resolved(r$rooms$room_current()$async)
      if (is.null(r$rooms$room_current()$history)) {
        r$rooms$rooms[[r$rooms$current]]$text <- "Welcome to the ChatGPT!\n\nThis is an AI chatbot based on the OpenAI API that can engage in intelligent conversations with you.\n\nPlease enter your questions or topics in the input box below and press \"Send\" button on the right to start chatting with the chatbot.\n\nHave a great time!"
        historyUI(div_update(r$rooms$room_current()$history, openai_logo = openai_logo, user_logo = user_logo))
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
          r$rooms$room_current()$streaming()
          outputUI(gsub("\\n$", "", markdown(paste0(r$rooms$room_current()$text, collapse = "\n"))))
          # session$sendCustomMessage(type = "scrollCallback", 1)
        } else {
          enable("chat_submit")
          enable("chat_regenerate")
          enable("chat_continuous")
          stopUI(NULL)
          if (!is.null(db)) {
            data_list <- lapply(names(r$rooms$rooms), function(nm) {
              room <- r$rooms$rooms[[nm]]
              if (is.null(room$history) || length(room$history) == 0) {
                return(NULL)
              }
              data <- do.call(rbind.data.frame, room$history)
              data[["stream_file"]] <- room$stream_file
              data[["room"]] <- nm
              return(data)
            })
            data_all <- do.call(rbind.data.frame, data_list)
            if (nrow(data_all) > 0) {
              # print("saving data")
              dbWriteTable(con, name = paste0(res_auth[["user"]], "_data"), value = data_all, overwrite = TRUE)
            }
          }
        }
      }
      if (isFALSE(r$refresh)) {
        outputUI(gsub("\\n$", "", markdown(paste0(r$rooms$room_current()$text, collapse = "\n"))))
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

    session$allowReconnect(TRUE)
  }

  shinyApp(ui = ui, server = server)
}

ChatGPT_app(db = "chatgpt.sqlite")
