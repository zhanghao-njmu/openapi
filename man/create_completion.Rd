% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/completions.R
\name{create_completion}
\alias{create_completion}
\title{Create a completion using the OpenAI API.}
\usage{
create_completion(
  endpoint = "completions",
  model = "text-davinci-003",
  prompt = "<|endoftext|>",
  suffix = NULL,
  max_tokens = 2048,
  temperature = 1,
  top_p = 1,
  n = 1,
  stream = FALSE,
  logprobs = NULL,
  echo = FALSE,
  stop = NULL,
  presence_penalty = 0,
  frequency_penalty = 0,
  best_of = 1,
  logit_bias = NULL,
  user = NULL,
  max_tries = 1,
  timeout = 300,
  ...
)
}
\arguments{
\item{endpoint}{The endpoint of the API (default is "completions").}

\item{model}{The ID of the model to use.}

\item{prompt}{The prompt(s) to use.}

\item{suffix}{The suffix(es) to use.}

\item{max_tokens}{The maximum number of tokens to generate for each prompt.}

\item{temperature}{Controls the randomness of the generated tokens. Higher values means more randomness.}

\item{top_p}{Controls the diversity of the generated tokens.}

\item{n}{The number of completions to generate for each prompt.}

\item{stream}{Whether to use streaming to receive the response.}

\item{logprobs}{Whether to include the log probabilities for each token.}

\item{echo}{Whether to include the prompt and the completion together in the returned object.}

\item{stop}{The stopping sequence to use.}

\item{presence_penalty}{Control the degree to which model should avoid generating words that were already in the prompt.}

\item{frequency_penalty}{Control the degree to which model should avoid generating words which appeared frequently in the past texts.}

\item{best_of}{Return the N best completions}

\item{logit_bias}{A dictionary of logit bias values to add to the logits of the token.}

\item{user}{A unique identifier for the user requesting the completion.}

\item{max_tries}{The maximum number of times to try the request again in case of failure.}

\item{timeout}{The maximum amount of time to wait for the request to complete.}

\item{...}{Additional arguments to pass to making_requests().}

\item{api_base}{The API URL to use (override the default URL).}

\item{api_key}{The API key to use.}

\item{organization}{The organization ID to use.}
}
\value{
Response object of class 'CompletionResponse'.
}
\description{
Create a completion using the OpenAI API.
}
