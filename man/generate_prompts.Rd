% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{generate_prompts}
\alias{generate_prompts}
\title{Generate Prompts}
\usage{
generate_prompts(prompt = "Act as an R Package Development Assistant", ...)
}
\arguments{
\item{prompt}{A string indicating the prompt for the chat.}

\item{...}{Additional arguments.}
}
\value{
A prompt for the user to engage in a chat with a chatbot.
}
\description{
This function generates prompts for the user to engage in a chat with a chatbot.
}
\examples{
\dontrun{
q <- ChatGPT$new(act_as = "Prompt generater")$chat("Act as an R Package Development Assistant")
prompt1 <- q$last()
q <- ChatGPT$new()$chat(prompt1, role = "system")

prompt2 <- generate_prompts("Act as an R Package Development Assistant")
q <- ChatGPT$new()$chat(prompt2, role = "system")
}
}
