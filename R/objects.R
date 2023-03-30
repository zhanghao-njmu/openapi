# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            "embedding" = unlist(content$data[[1]]$embedding),
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
    chat = function(content = NULL, role = "user", stream = TRUE, continue = TRUE, ...) {
      if (!is.null(content)) {
        messages <- list(
          list(
            "role" = role,
            "content" = content
          )
        )
        if (isTRUE(continue)) {
          messages <- c(self$messages, messages)
        }
        content <- create_chat_completion(messages = messages, stream = stream, ...)
        self$messages <- c(messages, list(
          list(
            "role" = "assistant",
            "content" = content$extract("choices")[1]
          )
        ))
      }
    },
    print = function() {
      conversations <- sapply(self$messages, function(x) paste0(x[["role"]], ": ", x[["content"]]))
      cat(sprintf("%s\n", conversations), sep = "")
    }
  )
)
