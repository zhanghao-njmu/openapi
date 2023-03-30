#' @export
create_transcription <- function(endpoint = "v1/audio/transcriptions",
                                 file,
                                 model = "whisper-1",
                                 prompt = NULL,
                                 response_format = c("json", "text", "srt", "verbose_json", "vtt"),
                                 temperature = 0,
                                 language = NULL,
                                 api_url = NULL,
                                 api_key = NULL,
                                 organization = NULL,
                                 max_tries = 1,
                                 timeout = 300) {
  response_format <- match.arg(response_format)

  data <- list()
  data[["file"]] <- httr::upload_file(file)
  data[["model"]] <- model
  data[["prompt"]] <- prompt
  data[["response_format"]] <- response_format
  data[["temperature"]] <- temperature
  data[["language"]] <- language

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    encode = "multipart",
    post_type = "multipart/form-data",
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}

#' @export
create_translation <- function(endpoint = "v1/audio/translations",
                               file,
                               model = "whisper-1",
                               prompt = NULL,
                               response_format = c("json", "text", "srt", "verbose_json", "vtt"),
                               temperature = 0,
                               api_url = NULL,
                               api_key = NULL,
                               organization = NULL,
                               max_tries = 1,
                               timeout = 300) {
  response_format <- match.arg(response_format)

  data <- list()
  data[["file"]] <- httr::upload_file(file)
  data[["model"]] <- model
  data[["prompt"]] <- prompt
  data[["response_format"]] <- response_format
  data[["temperature"]] <- temperature

  response <- making_requests(
    method = "POST",
    endpoint = endpoint,
    data = data,
    encode = "multipart",
    post_type = "multipart/form-data",
    api_url = api_url,
    api_key = api_key,
    organization = organization,
    max_tries = max_tries,
    timeout = timeout
  )
  return(parse_response(response))
}
