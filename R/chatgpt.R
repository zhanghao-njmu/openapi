#' @export
chatgpt <- function(messages, stream = TRUE, ...) {
  content <- create_chat_completion(messages = messages, stream = stream, ...)
}
