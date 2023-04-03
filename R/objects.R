# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setClass("TextCompletion",
  slots = list(
    response = "ANY",
    input = "character",
    output = "character",
    difference = "ANY"
  )
)
setMethod(
  "show",
  signature(object = "TextCompletion"),
  function(object) {
    line <- paste0(rep("=", 30), collapse = "")
    cat(
      line, " Input text ", line, "\n\n", slot(object, "input"), "\n\n",
      line, " Output text ", line, "\n\n", slot(object, "output"), "\n\n",
      sep = ""
    )
    invisible(object)
  }
)
setMethod(
  "print",
  signature(x = "TextCompletion"),
  function(x) {
    show(x)
  }
)

# ModelsResponse ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
ModelsResponse <- R6Class(
  classname = "ModelsResponse",
  public = list(
    response = NULL,
    response_format = NULL,
    fields = c("id", "object", "created", "owned_by", "permission"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$response_format <- "json"
      } else {
        self$response <- response
        self$response_format <- "raw"
      }
    },
    extract = function(field = NULL, filter = NULL) {
      if (self$response_format == "json") {
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
      } else {
        self$response
      }
    },
    print = function() {
      if (self$response_format == "json") {
        cat(sprintf("ID(n=%s): %s\n", length(unique(self$extract("id"))), paste0(unique(self$extract("id")), collapse = ", ")))
        cat(sprintf("Object: %s\n", paste0(unique(self$extract("object")), collapse = ", ")))
        cat(sprintf("Owned by: %s\n", paste0(unique(self$extract("owned_by")), collapse = ", ")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$response))
      }
    }
  )
)

# CompletionResponse ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
CompletionResponse <- R6Class(
  classname = "CompletionResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    response_format = NULL,
    fields = c("id", "object", "created", "model", "choices", "choices_finish", "prompt_tokens", "completion_tokens", "total_tokens"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        self$parameters <- fromJSON(rawToChar(response$request$options$postfields), simplifyVector = FALSE)
        self$fields <- unique(c(names(self$parameters), self$fields))
        self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$response_format <- "json"
      } else {
        self$response <- response
        self$response_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$response_format == "json") {
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
      } else {
        self$response
      }
    },
    print = function() {
      if (self$response_format == "json") {
        object <- self$extract("object")
        cat(sprintf("ID: %s\n", self$extract("id")))
        cat(sprintf("Object: %s\n", object))
        cat(sprintf("Created: %s\n", self$extract("created")))
        cat(sprintf("Model: %s\n", self$extract("model")))
        if (object == "text_completion") {
          cat(sprintf("Prompt: %s\n", self$extract("prompt")))
        } else if (object == "chat.completion") {
          messages <- self$extract("messages")
          cat(sprintf("Message: %s\n", messages[length(messages)][[1]][["content"]]))
        } else if (object == "edit") {
          cat(sprintf("Input: %s\n", self$extract("input")))
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
      } else {
        cat(sprintf("Content: %s\n", self$response))
      }
    }
  )
)

# ImagesResponse ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
ImagesResponse <- R6Class(
  classname = "ImagesResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    response_format = NULL,
    fields = c("created", "url"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        self$parameters <- response$request$fields
        self$fields <- unique(c(names(self$parameters), self$fields))
        self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$response_format <- "json"
      } else {
        self$response <- response
        self$response_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$response_format == "json") {
        if (field %in% names(self$parameters)) {
          self$parameters[[field]]
        } else {
          switch(field,
            "created" = self$response$created,
            "url" = sapply(self$response$data, function(x) x[["url"]])
          )
        }
      } else {
        self$response
      }
    },
    print = function() {
      if (self$response_format == "json") {
        cat(sprintf("Created: %s\n", self$extract("created")))
        cat(sprintf(
          "URL[%s]: %s\n",
          seq_along(self$extract("url")), self$extract("url")
        ), sep = "")
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$response))
      }
    }
  )
)

# EmbeddingsResponse ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
EmbeddingsResponse <- R6Class(
  classname = "EmbeddingsResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    response_format = NULL,
    fields = c("object", "embedding", "model", "prompt_tokens", "total_tokens"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        self$parameters <- fromJSON(rawToChar(response$request$options$postfields), simplifyVector = FALSE)
        self$fields <- unique(c(names(self$parameters), self$fields))
        self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$response_format <- "json"
      } else {
        self$response <- response
        self$response_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$response_format == "json") {
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
      } else {
        self$response
      }
    },
    print = function() {
      if (self$response_format == "json") {
        cat(sprintf("Object: %s\n", self$extract("object")))
        cat(sprintf("Embedding length: %s\n", length(self$extract("embedding"))))
        cat(sprintf("Model: %s\n", self$extract("model")))
        cat(sprintf("Prompt tokens: %s\n", self$extract("prompt_tokens")))
        cat(sprintf("Total tokens: %s\n", self$extract("total_tokens")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$response))
      }
    }
  )
)

# AudioResponse ----------------------------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
AudioResponse <- R6Class(
  classname = "AudioResponse",
  public = list(
    parameters = NULL,
    response = NULL,
    response_format = NULL,
    fields = c("text"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        self$parameters <- response$request$fields
        self$fields <- unique(c(names(self$parameters), self$fields))
        self$response <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$response_format <- "json"
      } else {
        self$response <- response
        self$response_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$response_format == "json") {
        if (field %in% names(self$parameters)) {
          self$parameters[[field]]
        } else {
          switch(field,
            "text" = self$response$text
          )
        }
      } else {
        self$response
      }
    },
    print = function() {
      if (self$response_format == "json") {
        cat(sprintf("Text: %s\n", self$extract("text")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$response))
      }
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
    act_as = NULL,
    latest_response = NULL,
    index = NULL,
    initialize = function(act_as = NULL, messages = NULL) {
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
        self$messages <- messages
      }
      invisible(self)
    },
    chat = function(prompt = NULL, role = "user", stream = TRUE, continuous = TRUE, ...) {
      if (!is.null(prompt)) {
        messages <- list(
          list(
            "role" = role,
            "content" = paste0(prompt, collapse = " ")
          )
        )
        if (isTRUE(continuous)) {
          messages <- c(self$messages, messages)
        }
        resp <- create_chat_completion(messages = messages, stream = stream, ...)
        if (inherits(resp, "CompletionResponse")) {
          self$latest_response <- resp
          self$messages <- c(messages, list(
            list(
              "role" = "assistant",
              "content" = self$latest_response$extract("choices")[1]
            )
          ))
          self$index <- length(self$messages)
        } else {
          warning("An error occurred when generating the chat completion. Please check the parameters and try again later.", immediate. = TRUE)
        }
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
      conversations <- sapply(self$messages, function(x) paste0(x[["role"]], ": ", x[["content"]]))
      cat("Conversations:\n\n")
      cat(sprintf("{%s}\n\n", conversations), sep = "")
      invisible(self)
    }
  )
)
