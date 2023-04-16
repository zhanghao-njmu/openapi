#' Create Transcription
#'
#' This function creates transcription of an audio file using OpenAI language models
#'
#' @param endpoint an endpoint url (default "v1/audio/transcriptions").
#' @param file name and path of the audio file.
#' @param model name of the language model to use (default "whisper-1").
#' @param prompt a string prompt which will be prepended to the file transcription.
#' @param response_format a character string indicating the type of response format (default "json").
#' @param temperature a parameter controlling the creativity of the model. Set between 0 and 1 (default 0).
#' @param language the language code of the audio if known. Currently this parameter
#'        only supports English ("en") or Mandarin ("zh") (default NULL)
#' @param api_url base URL for the OpenAI API (default NULL).
#' @param api_key An API Key is required to use this tool.
#' @param organization Optional organization slug required for use with OpenAI API.
#' @param max_tries the maximum number of times to try the request if there are connection errors (default 1).
#' @param timeout maximum time to wait for a reponse (default 300).
#'
#' @return An object of class "AudioResponse".
#'
#' @export
create_transcription <- function(endpoint = "v1/audio/transcriptions",
                                 file,
                                 model = "whisper-1",
                                 prompt = NULL,
                                 response_format = c("json", "text", "srt", "verbose_json", "vtt"),
                                 temperature = 0,
                                 language = NULL,
                                 max_tries = 1,
                                 timeout = 300,
                                 ...) {
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
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, AudioResponse))
}

#' Create Translation
#'
#' This function translates an audio file to text using OpenAI API.
#'
#' @param endpoint A string indicating the API endpoint for making requests (default is "v1/audio/translations").
#' @param file A character string indicating the file path of the audio file to be translated.
#' @param model A character string indicating the model name (default is "whisper-1").
#' @param prompt A string to use as input prompt.
#' @param response_format A character string indicating the response format (accepts "json", "text", "srt", "verbose_json", or "vtt").
#' @param temperature A numeric value indicating the degree of randomness in the model's output (default is 0).
#' @param api_url A string indicating the base URL of the OpenAI API (default is NULL).
#' @param api_key A string indicating the authentication key to use for requests (default is NULL).
#' @param organization A string indicating the organization ID (default is NULL).
#' @param max_tries An integer value indicating the maximum number of attempts to make the API call (default is 1).
#' @param timeout A numeric value indicating the timeout limit in seconds for the API call (default is 300).
#'
#' @return An object of class "AudioResponse".
#' @export
create_translation <- function(endpoint = "v1/audio/translations",
                               file,
                               model = "whisper-1",
                               prompt = NULL,
                               response_format = c("json", "text", "srt", "verbose_json", "vtt"),
                               temperature = 0,
                               max_tries = 1,
                               timeout = 300,
                               ...) {
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
    max_tries = max_tries,
    timeout = timeout,
    ...
  )
  return(parse_response(response, AudioResponse))
}
