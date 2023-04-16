#' @export
create_fine_tune <- function(endpoint = "v1/fine-tunes",
                             training_file,
                             validation_file = NULL,
                             model = "curie",
                             n_epochs = 4,
                             batch_size = NULL,
                             learning_rate_multiplier = NULL,
                             prompt_loss_weight = 0.01,
                             compute_classification_metrics = FALSE,
                             classification_n_classes = NULL,
                             classification_positive_class = NULL,
                             classification_betas = NULL,
                             suffix = NULL,
                             max_tries = 1,
                             timeout = 300,
                             ...) {
  data <- list()
  data[["training_file"]] <- training_file
  data[["validation_file"]] <- validation_file
  data[["model"]] <- model
  data[["n_epochs"]] <- n_epochs
  data[["batch_size"]] <- batch_size
  data[["learning_rate_multiplier"]] <- learning_rate_multiplier
  data[["prompt_loss_weight"]] <- prompt_loss_weight
  data[["compute_classification_metrics"]] <- compute_classification_metrics
  data[["classification_n_classes"]] <- classification_n_classes
  data[["classification_positive_class"]] <- classification_positive_class
  data[["classification_betas"]] <- classification_betas
  data[["suffix"]] <- suffix

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}

#' @export
list_fine_tunes <- function(endpoint = "v1/fine-tunes",
                            max_tries = 1,
                            timeout = 300,
                            ...) {
  response <- making_requests(
    method = "GET",
    endpoint = endpoint,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}

#' @export
retrieve_fine_tunes <- function(endpoint = "v1/fine-tunes",
                                fine_tune_id,
                                max_tries = 1,
                                timeout = 300,
                                ...) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, fine_tune_id, sep = "/"),
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}

#' @export
cancel_fine_tunes <- function(endpoint = "v1/fine-tunes",
                              fine_tune_id,
                              max_tries = 1,
                              timeout = 300,
                              ...) {
  response <- making_requests(
    method = "GET",
    endpoint = paste(endpoint, fine_tune_id, "cancel", sep = "/"),
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}

#' @export
list_fine_tune_events <- function(endpoint = "v1/fine-tunes",
                                  fine_tune_id,
                                  stream = FALSE,
                                  max_tries = 1,
                                  timeout = 300,
                                  ...) {
  response <- making_requests(
    method = "GET",
    endpoint = paste0(paste(endpoint, fine_tune_id, "events", sep = "/"), "?stream=", stream),
    stream = stream,
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}

#' @export
delete_fine_tune_model <- function(endpoint = "v1/models",
                                   model,
                                   max_tries = 1,
                                   timeout = 300,
                                   ...) {
  response <- making_requests(
    method = "DELETE",
    endpoint = paste(endpoint, model, sep = "/"),
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response))
}
