## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ModelsResponse ----------------------------------------------------------------------------------
#' @title ModelsResponse Class
#' @description An R6Class object representing a response returned by the OpenAI Models API
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
ModelsResponse <- R6Class(
  classname = "ModelsResponse",
  public = list(
    response = NULL,
    fields = c("id", "object", "created", "owned_by", "permission"),
    initialize = function(response) {
      self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    },
    extract = function(field = "id", filter = NULL) {
      if ("data" %in% names(self$response)) {
        content <- self$response
      } else {
        content <- list(data = list(self$response))
      }
      if (!is.null(filter) && length(intersect(names(filter), c("id", "object", "owned_by"))) > 0) {
        content$data <- content$data[sapply(content$data, function(x) {
          x[["id"]] %in% filter[["id"]] ||
            x[["object"]] %in% filter[["object"]] ||
            x[["owned_by"]] %in% filter[["owned_by"]]
        })]
      }
      if (length(content$data) > 0) {
        out <- switch(field,
          "id" = sapply(content$data, function(x) x[["id"]]),
          "object" = sapply(content$data, function(x) x[["object"]]),
          "created" = sapply(content$data, function(x) x[["created"]]),
          "owned_by" = sapply(content$data, function(x) x[["owned_by"]]),
          "permission" = sapply(content$data, function(x) x[["permission"]])
        )
        setNames(out, sapply(content$data, function(x) x[["id"]]))
      } else {
        NULL
      }
    },
    print = function() {
      cat(sprintf("ID(n=%s): %s\n", length(unique(self$extract("id"))), paste0(unique(self$extract("id")), collapse = ", ")))
      cat(sprintf("Object: %s\n", paste0(unique(self$extract("object")), collapse = ", ")))
      cat(sprintf("Owned by: %s\n", paste0(unique(self$extract("owned_by")), collapse = ", ")))
      cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
    }
  )
)

# CompletionResponse ----------------------------------------------------------------------------------
#' @title CompletionResponse Class
#' @description An R6Class object representing a response returned by the OpenAI Completion API
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
CompletionResponse <- R6Class(
  classname = "CompletionResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    fields = c("id", "object", "created", "model", "choices", "choices_finish", "prompt_tokens", "completion_tokens", "total_tokens"),
    initialize = function(response) {
      self$parameters <- fromJSON(rawToChar(response$request$options$postfields), simplifyVector = FALSE)
      self$fields <- unique(c(names(self$parameters), self$fields))
      self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    },
    extract = function(field = "id") {
      if (field %in% names(self$parameters)) {
        self$parameters[[field]]
      } else {
        switch(field,
          "id" = self$response$id,
          "object" = self$response$object,
          "created" = self$response$created,
          "model" = self$response$model,
          "choices" = switch(self$response$object,
            "text_completion" = sapply(self$response$choices, function(x) x[["text"]]),
            "chat.completion" = sapply(self$response$choices, function(x) x[["message"]][["content"]]),
            "edit" = sapply(self$response$choices, function(x) x[["text"]])
          ),
          "choices_finish" = sapply(self$response$choices, function(x) identical(x[["finish_reason"]], "stop") || is.null(x[["finish_reason"]])),
          "prompt_tokens" = self$response$usage$prompt_tokens,
          "completion_tokens" = self$response$usage$completion_tokens,
          "total_tokens" = self$response$usage$total_tokens
        )
      }
    },
    print = function() {
      object <- self$extract("object")
      cat(sprintf("ID: %s\n", self$extract("id")))
      cat(sprintf("Object: %s\n", object))
      cat(sprintf("Created: %s\n", self$extract("created")))
      cat(sprintf("Model: %s\n", self$extract("model")))
      if (object == "text_completion") {
        cat(sprintf("Prompt: %s\n", truncate_text(self$extract("prompt"))))
      } else if (object == "chat.completion") {
        messages <- self$extract("messages")
        cat(sprintf("Message: %s\n", truncate_text(messages[length(messages)][[1]][["content"]])))
      } else if (object == "edit") {
        cat(sprintf("Input: %s\n", truncate_text(self$extract("input"))))
        cat(sprintf("Instruction: %s\n", self$extract("instruction")))
      }
      cat(sprintf(
        "Choices[%s]: %s [Finished: %s]\n",
        seq_along(self$extract("choices_finish")), gsub("\\n", "\\\\n", truncate_text(self$extract("choices"))), self$extract("choices_finish")
      ), sep = "")
      cat(sprintf("Prompt tokens: %s\n", self$extract("prompt_tokens")))
      cat(sprintf("Completion tokens: %s\n", self$extract("completion_tokens")))
      cat(sprintf("Total tokens: %s\n", self$extract("total_tokens")))
      cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
    }
  )
)

# ImagesResponse ----------------------------------------------------------------------------------
#' @title ImagesResponse Class
#' @description An R6Class object representing a response returned by the OpenAI Images API
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
ImagesResponse <- R6Class(
  classname = "ImagesResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    fields = c("created", "url"),
    initialize = function(response) {
      self$parameters <- response$request$fields
      self$fields <- unique(c(names(self$parameters), self$fields))
      self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    },
    extract = function(field = NULL) {
      if (field %in% names(self$parameters)) {
        self$parameters[[field]]
      } else {
        switch(field,
          "created" = self$response$created,
          "url" = sapply(self$response$data, function(x) x[["url"]])
        )
      }
    },
    print = function() {
      cat(sprintf("Created: %s\n", self$extract("created")))
      cat(sprintf(
        "URL[%s]: %s\n",
        seq_along(self$extract("url")), self$extract("url")
      ), sep = "")
      cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
    }
  )
)

# EmbeddingsResponse ----------------------------------------------------------------------------------
#' @title EmbeddingsResponse Class
#' @description An R6Class object representing a response returned by the OpenAI Embeddings API
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
EmbeddingsResponse <- R6Class(
  classname = "EmbeddingsResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    fields = c("object", "embedding", "model", "prompt_tokens", "total_tokens"),
    initialize = function(response) {
      self$parameters <- fromJSON(rawToChar(response$request$options$postfields), simplifyVector = FALSE)
      self$fields <- unique(c(names(self$parameters), self$fields))
      self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    },
    extract = function(field = NULL) {
      if (field %in% names(self$parameters)) {
        self$parameters[[field]]
      } else {
        switch(field,
          "object" = self$response$object,
          "embedding" = unlist(self$response$data[[1]]$embedding),
          "model" = self$response$model,
          "prompt_tokens" = self$response$usage$prompt_tokens,
          "total_tokens" = self$response$usage$total_tokens
        )
      }
    },
    print = function() {
      cat(sprintf("Object: %s\n", self$extract("object")))
      cat(sprintf("Embedding length: %s\n", length(self$extract("embedding"))))
      cat(sprintf("Model: %s\n", self$extract("model")))
      cat(sprintf("Prompt tokens: %s\n", self$extract("prompt_tokens")))
      cat(sprintf("Total tokens: %s\n", self$extract("total_tokens")))
      cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
    }
  )
)

# AudioResponse ----------------------------------------------------------------------------------
#' @title AudioResponse Class
#' @description An R6Class object representing a response returned by the OpenAI Audio API
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
AudioResponse <- R6Class(
  classname = "AudioResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    fields = c("text"),
    initialize = function(response) {
      self$parameters <- response$request$fields
      self$fields <- unique(c(names(self$parameters), self$fields))
      self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    },
    extract = function(field = NULL) {
      if (field %in% names(self$parameters)) {
        self$parameters[[field]]
      } else {
        switch(field,
          "text" = self$response$text
        )
      }
    },
    print = function() {
      cat(sprintf("Text: %s\n", self$extract("text")))
      cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
    }
  )
)

# ChatGPT ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @export
ChatGPT <- R6Class(
  classname = "ChatGPT",
  public = list(
    messages = NULL,
    initial = list(),
    latest_response = NULL,
    index = NULL,
    chat_params = NULL,
    initialize = function(act_as = NULL, messages = NULL,
                          chat_params = list()) {
      if (!is.null(act_as)) {
        matched <- agrep(pattern = act_as, x = prompts[["act"]], max.distance = 0.1, ignore.case = TRUE)
        if (length(matched) > 0) {
          message("ChatGPT will act as a ", prompts[["act"]][matched[1]])
          self$messages <- list(
            list(
              "role" = "system",
              "content" = prompts[["prompt"]][matched[1]]
            )
          )
        } else {
          message("Couldn't find a built-in prompt for the role. Generating one instead...")
          prompt <- generate_prompts(paste0("Act as a ", act_as))
          self$messages <- list(
            list(
              "role" = "system",
              "content" = prompt
            )
          )
        }
      } else if (!is.null(messages)) {
        self$messages <- lapply(messages, function(x) list("role" = x[["role"]], "content" = x[["content"]]))
      }
      self$initial <- list(act_as = act_as, messages = messages)
      self$chat_params <- chat_params
      invisible(self)
    },
    chat = function(prompt = NULL, role = "user", continuous = TRUE) {
      if (!is.null(prompt)) {
        messages <- list(
          list(
            "role" = role,
            "content" = paste0(prompt, collapse = " ")
          )
        )
        if (isTRUE(continuous)) {
          messages <- c(lapply(self$messages, function(x) list("role" = x[["role"]], "content" = x[["content"]])), messages)
        }
        chat_params <- self$chat_params
        chat_params[["messages"]] <- messages
        resp <- do.call(openapi::create_chat_completion, chat_params)
        self$latest_response <- resp
        if (inherits(resp, "CompletionResponse")) {
          all_messages <- c(
            lapply(self$messages, function(x) list("role" = x[["role"]], "content" = x[["content"]])),
            list(
              list(
                "role" = role,
                "content" = paste0(prompt, collapse = " ")
              )
            ),
            list(
              list(
                "role" = "assistant",
                "content" = self$latest_response$extract("choices")[1]
              )
            )
          )
          self$messages <- all_messages
          self$index <- length(self$messages)
        } else {
          warning("An error occurred when generating the chat completion. Please check the parameters in the latest_response and try again later.", immediate. = TRUE)
        }
      }
      invisible(self)
    },
    regenerate = function(continuous = TRUE) {
      if (length(self$messages) > 1) {
        if (is.null(self$index)) {
          self$index <- length(self$messages)
        }
        self$messages <- self$messages[1:(self$index - 1)]
        if (!identical(self$messages[[length(self$messages)]][["role"]], "user")) {
          warning("No reply to be regenerated", immediate. = TRUE)
        } else {
          if (isTRUE(continuous)) {
            messages <- self$messages
          } else {
            messages <- self$messages[length(self$messages)]
          }
          messages <- lapply(self$messages, function(x) list("role" = x[["role"]], "content" = x[["content"]]))
          chat_params <- self$chat_params
          chat_params[["messages"]] <- messages
          resp <- do.call(openapi::create_chat_completion, chat_params)
          self$latest_response <- resp
          if (inherits(resp, "CompletionResponse")) {
            all_messages <- c(
              lapply(self$messages, function(x) list("role" = x[["role"]], "content" = x[["content"]])),
              list(
                list(
                  "role" = "assistant",
                  "content" = self$latest_response$extract("choices")[1]
                )
              )
            )
            self$messages <- all_messages
            self$index <- length(self$messages)
          } else {
            warning("An error occurred when generating the chat completion. Please check the parameters in the latest_response and try again later.", immediate. = TRUE)
          }
        }
      } else {
        warning("No previous message.", immediate. = TRUE)
      }
      invisible(self)
    },
    first = function() {
      self$index <- 1
      messages <- self$messages[self$index]
      cat(sprintf("index: %s\n{%s}\n\n", self$index, paste0(messages[[1]][["role"]], ": ", messages[[1]][["content"]])), sep = "")
      return(invisible(messages[[1]][["content"]]))
    },
    last = function() {
      self$index <- length(self$messages)
      messages <- self$messages[self$index]
      cat(sprintf("index: %s\n{%s}\n\n", self$index, paste0(messages[[1]][["role"]], ": ", messages[[1]][["content"]])), sep = "")
      return(invisible(messages[[1]][["content"]]))
    },
    backward = function() {
      self$index <- max(self$index - 1, 1)
      messages <- self$messages[self$index]
      cat(sprintf("index: %s\n{%s}\n\n", self$index, paste0(messages[[1]][["role"]], ": ", messages[[1]][["content"]])), sep = "")
      return(invisible(messages[[1]][["content"]]))
    },
    forward = function() {
      self$index <- min(self$index + 1, length(self$messages))
      messages <- self$messages[self$index]
      cat(sprintf("index: %s\n{%s}\n\n", self$index, paste0(messages[[1]][["role"]], ": ", messages[[1]][["content"]])), sep = "")
      return(invisible(messages[[1]][["content"]]))
    },
    print = function() {
      cat("Conversations:\n\n")
      conversations <- sapply(self$messages, function(x) paste0(x[["role"]], ":\n", "{", x[["content"]], "}"))
      cat(sprintf("%s\n\n", conversations), sep = "")
      invisible(self)
    }
  )
)

# ChatRoom ----------------------------------------------------------------------------------
#' @importFrom future plan future value resolved
#' @importFrom future.callr callr
#' @export
ChatRoom <- R6Class(
  classname = "ChatRoom",
  public = list(
    chat = NULL,
    stream_file = NULL,
    history = NULL,
    text = NULL,
    async = NULL,
    initialize = function(act_as = NULL, messages = NULL,
                          chat_params = list(),
                          stream_file = tempfile(fileext = ".streamfile.txt")) {
      plan(callr)

      chat_params[["api_base"]] <- chat_params[["api_base"]] %||% getOption("openapi_api_base")
      chat_params[["api_key"]] <- chat_params[["api_key"]] %||% getOption("openapi_api_key")
      chat_params[["organization"]] <- chat_params[["organization"]] %||% getOption("openapi_organization")
      chat_params[["api_type"]] <- chat_params[["api_type"]] %||% getOption("openapi_api_type")
      chat_params[["api_version"]] <- chat_params[["api_version"]] %||% getOption("openapi_api_version")
      chat_params[["azure_deployment"]] <- chat_params[["azure_deployment"]] %||% getOption("openapi_azure_deployment")

      if (is.null(chat_params[["api_base"]]) || is.null(chat_params[["api_key"]])) {
        stop("api_base or api_key is not defined, please run the api_setup function to configure them.")
      }

      if (!file.exists(stream_file)) {
        dir.create(dirname(stream_file), recursive = TRUE, showWarnings = FALSE)
        file.create(stream_file, showWarnings = TRUE)
      }
      chat_params[["stream"]] <- TRUE
      chat_params[["stream_file"]] <- stream_file
      chat <- ChatGPT$new(act_as = act_as, messages = messages, chat_params = chat_params)
      self$history <- messages
      self$chat <- chat
      self$stream_file <- stream_file
      invisible(self)
    },
    chat_submit = function(prompt = NULL, role = "user", continuous = TRUE) {
      self$history <- c(self$history, list(list("role" = role, "content" = prompt, "time" = as.character(Sys.time()))))
      writeLines("", self$stream_file)
      chatgpt <- self$chat
      self$async <- future(chatgpt$chat(prompt, continuous = continuous), seed = NULL)
      invisible(self)
    },
    chat_regenerate = function(index = NULL, continuous = TRUE) {
      if (is.null(index) || isTRUE(index == 1)) {
        self$history <- self$history[-length(self$history)]
      } else {
        self$history <- self$history[1:(index - 1)]
      }
      writeLines("", self$stream_file)
      chatgpt <- self$chat
      chatgpt$index <- index
      self$async <- future(chatgpt$regenerate(continuous = continuous), seed = NULL)
      invisible(self)
    },
    chat_stop = function() {
      if (inherits(self$async, "Future")) {
        self$async$process$kill()
        self$text <- paste0(self$text, "[The message was interrupted]")
        writeLines(self$text, self$stream_file)
        self$history <- c(self$history, list(list("role" = "assistant", "content" = self$text, "time" = as.character(Sys.time()))))
        self$chat$messages <- self$history
        self$chat$index <- length(self$chat$messages)
      }
      self$async <- NULL
      invisible(self)
    },
    chat_clear = function() {
      if (inherits(self$async, "Future")) {
        self$async$process$kill()
      }
      self$async <- NULL
      self$chat <- ChatGPT$new(act_as = self$chat$initial$act_as, messages = self$chat$initial$messages, chat_params = self$chat$chat_params)
      self$history <- NULL
    },
    streaming = function() {
      text <- readLines(self$stream_file, warn = FALSE)
      if (identical(text, "") || length(text) == 0) {
        text <- "..."
      }
      if (identical(text[length(text)], "data: [DONE]") || (resolved(self$async) && !is.null(self$async))) {
        if (identical(text[length(text)], "data: [DONE]")) {
          text <- text[-length(text)]
        }
        if (inherits(self$async, "Future")) {
          self$chat <- value(self$async)
          new_messages <- self$chat$messages[length(self$chat$messages)]
          new_messages[[1]][["time"]] <- as.character(Sys.time())
          self$history <- c(self$history, new_messages)
          self$async <- NULL
        }
      }
      self$text <- paste0(text, collapse = "\n")
      invisible(self)
    }
  )
)


# ChatRooms ----------------------------------------------------------------------------------
#' @export
ChatRooms <- R6Class(
  classname = "ChatRooms",
  public = list(
    current = NULL,
    rooms = NULL,
    initialize = function(name = NULL, ...) {
      if (is.null(name)) {
        name <- "room1"
      }
      self$rooms[[name]] <- ChatRoom$new(...)
      self$current <- name
      invisible(self)
    },
    room_add = function(name = NULL, ...) {
      if (is.null(name)) {
        n <- gsub("(^room)(\\d+$)", "\\2", grep("(^room)(\\d+$)", names(self$rooms), value = TRUE))
        if (length(n) > 0) {
          name <- paste0("room", max(max(as.numeric(n), na.rm = TRUE) + 1, 1))
        } else {
          name <- "room1"
        }
      }
      self$rooms[[name]] <- ChatRoom$new(...)
      invisible(self)
    },
    room_remove = function(name = NULL) {
      if (is.null(name)) {
        if (self$current %in% names(self$rooms)) {
          name <- self$current
        } else {
          name <- names(self$rooms)[length(self$rooms)]
        }
      }
      stream_file <- self$rooms[[name]]$stream_file
      message("Remove stream file: ", stream_file)
      unlink(stream_file)
      self$rooms[[name]] <- NULL
      if (identical(self$current, name)) {
        self$current <- names(self$rooms)[length(self$rooms)]
      }
      invisible(self)
    },
    room_current = function(name = NULL) {
      if (is.null(name)) {
        if (self$current %in% names(self$rooms)) {
          name <- self$current
        } else {
          name <- names(self$rooms)[length(self$rooms)]
        }
      }
      self$rooms[[name]]
    }
  )
)

# TextEditing ----------------------------------------------------------------------------------
#' TextEditing
#' An R6Class for text editing
#' @importFrom R6 R6Class
#' @export
TextEditing <- R6Class("TextEditing",
  public = list(
    response = NULL,
    input = NULL,
    output = NULL,
    difference = NULL,
    fields = c("input", "output", "difference"),
    initialize = function(response, input = NULL) {
      self$response <- response
      if (inherits(self$response, "CompletionResponse")) {
        object <- self$response$extract("object")
        if (is.null(input)) {
          input <- switch(object,
            "text_completion" = self$response$extract("prompt"),
            "chat.completion" = self$response$extract("messages")[[length(self$response$extract("messages"))]][["content"]],
            "edit" = self$response$extract("input")
          )
        }
        output <- response$extract("choices")[1]
        comparison <- compare_text(text1 = input, text2 = output)
        self$input <- input
        self$output <- output
        self$difference <- comparison
        self$fields <- unique(c(self$fields, names(self$response$parameters)))
      } else {
        warning("The request has failed. Return the response only...", immediate. = TRUE)
        postfields <- fromJSON(rawToChar(response$request$options$postfields), simplifyVector = FALSE)
        if (is.null(input)) {
          if ("prompt" %in% names(postfields)) {
            input <- postfields[["prompt"]]
          } else if ("messages" %in% names(postfields)) {
            input <- postfields$messages[[length(postfields$messages)]][["content"]]
          } else if ("input" %in% names(postfields)) {
            input <- postfields[["input"]]
          }
        }
        self$input <- input
      }
    },
    extract = function(field = NULL) {
      if (field %in% c("input", "output", "difference")) {
        self[[field]]
      } else {
        if (inherits(self$response, "CompletionResponse")) {
          self$response$extract(field)
        } else {
          message("No field can be extracted.")
        }
      }
    },
    print = function() {
      line <- paste0(rep("=", 30), collapse = "")
      cat(
        line, " Input text ", line, "\n\n", self$input, "\n\n",
        line, " Output text ", line, "\n\n", self$output, "\n\n",
        sep = ""
      )
    }
  )
)
