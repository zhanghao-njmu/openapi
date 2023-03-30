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
    content = NULL,
    content_format = NULL,
    fields = c("id", "object", "created", "owned_by", "permission"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        response_json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$content <- response_json
        self$content_format <- "json"
      } else {
        self$content <- response
        self$content_format <- "raw"
      }
    },
    extract = function(field = NULL, filter = NULL) {
      if (self$content_format == "json") {
        if ("data" %in% names(self$content)) {
          content <- self$content
        } else {
          content <- list(data = list(self$content))
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
        self$content
      }
    },
    print = function() {
      if (self$content_format == "json") {
        cat(sprintf("ID(n=%s): %s\n", length(unique(self$extract("id"))), paste0(unique(self$extract("id")), collapse = ", ")))
        cat(sprintf("Object: %s\n", paste0(unique(self$extract("object")), collapse = ", ")))
        cat(sprintf("Owned by: %s\n", paste0(unique(self$extract("owned_by")), collapse = ", ")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$content))
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
    content = NULL,
    content_format = NULL,
    fields = c("id", "object", "created", "model", "choices", "choices_finish", "prompt_tokens", "completion_tokens", "total_tokens"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        response_json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$content <- response_json
        self$content_format <- "json"
      } else {
        self$content <- response
        self$content_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$content_format == "json") {
        switch(field,
          "id" = self$content$id,
          "object" = self$content$object,
          "created" = self$content$created,
          "model" = self$content$model,
          "choices" = switch(self$content$object,
            "text_completion" = sapply(self$content$choices, function(x) x[["text"]]),
            "chat.completion" = sapply(self$content$choices, function(x) x[["message"]][["content"]]),
            sapply(self$content$choices, function(x) x[["text"]])
          ),
          "choices_finish" = sapply(self$content$choices, function(x) identical(x[["finish_reason"]], "stop") || is.null(x[["finish_reason"]])),
          "prompt_tokens" = self$content$usage$prompt_tokens,
          "completion_tokens" = self$content$usage$completion_tokens,
          "total_tokens" = self$content$usage$total_tokens
        )
      } else {
        self$content
      }
    },
    print = function() {
      if (self$content_format == "json") {
        cat(sprintf("ID: %s\n", self$extract("id")))
        cat(sprintf("Object: %s\n", self$extract("object")))
        cat(sprintf("Created: %s\n", self$extract("created")))
        cat(sprintf("Model: %s\n", self$extract("model")))
        cat(sprintf(
          "Choices[%s]: %s [Finished: %s]\n",
          seq_along(self$extract("choices_finish")), self$extract("choices"), self$extract("choices_finish")
        ), sep = "")
        cat(sprintf("Prompt tokens: %s\n", self$extract("prompt_tokens")))
        cat(sprintf("Completion tokens: %s\n", self$extract("completion_tokens")))
        cat(sprintf("Total tokens: %s\n", self$extract("total_tokens")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$content))
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
    content = NULL,
    content_format = NULL,
    fields = c("created", "url"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        response_json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$content <- response_json
        self$content_format <- "json"
      } else {
        self$content <- response
        self$content_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$content_format == "json") {
        switch(field,
          "created" = self$content$created,
          "url" = sapply(self$content$data, function(x) x[["url"]])
        )
      } else {
        self$content
      }
    },
    print = function() {
      if (self$content_format == "json") {
        cat(sprintf("Created: %s\n", self$extract("created")))
        cat(sprintf(
          "URL[%s]: %s\n",
          seq_along(self$extract("url")), self$extract("url")
        ), sep = "")
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$content))
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
    content = NULL,
    content_format = NULL,
    fields = c("object", "embedding", "model", "prompt_tokens", "total_tokens"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        response_json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$content <- response_json
        self$content_format <- "json"
      } else {
        self$content <- response
        self$content_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$content_format == "json") {
        switch(field,
          "object" = self$content$object,
          "embedding" = unlist(content$data[[1]]$embedding),
          "model" = self$content$model,
          "prompt_tokens" = self$content$usage$prompt_tokens,
          "total_tokens" = self$content$usage$total_tokens
        )
      } else {
        self$content
      }
    },
    print = function() {
      if (self$content_format == "json") {
        cat(sprintf("Object: %s\n", self$extract("object")))
        cat(sprintf("Embedding length: %s\n", length(self$extract("embedding"))))
        cat(sprintf("Model: %s\n", self$extract("model")))
        cat(sprintf("Prompt tokens: %s\n", self$extract("prompt_tokens")))
        cat(sprintf("Total tokens: %s\n", self$extract("total_tokens")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$content))
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
    content = NULL,
    content_format = NULL,
    fields = c("text"),
    initialize = function(response) {
      if (inherits(response, "response")) {
        response_json <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
        self$content <- response_json
        self$content_format <- "json"
      } else {
        self$content <- response
        self$content_format <- "raw"
      }
    },
    extract = function(field = NULL) {
      if (self$content_format == "json") {
        switch(field,
          "text" = self$content$text
        )
      } else {
        self$content
      }
    },
    print = function() {
      if (self$content_format == "json") {
        cat(sprintf("Text: %s\n", self$extract("text")))
        cat(sprintf("Fields: %s\n", paste0(self$fields, collapse = ", ")))
      } else {
        cat(sprintf("Content: %s\n", self$content))
      }
    }
  )
)
