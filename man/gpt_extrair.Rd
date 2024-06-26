% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gpt.R
\name{gpt_extrair}
\alias{gpt_extrair}
\title{Submete decisões judiciais à  API da OPENAI e obtêm os resultados em json.}
\usage{
gpt_extrair(
  x = NULL,
  destaques = NULL,
  perguntas = NULL,
  chaves = NULL,
  modelo = "gpt-3.5-turbo-16k",
  temperatura = 0
)
}
\arguments{
\item{x}{Decisisões judiciais}

\item{destaques}{Vetor de palavras ou expressões a destacar.}

\item{perguntas}{Vetor de perguntas.}

\item{chaves}{Vetor de chaves do json.}

\item{modelo}{Informar o modelo.}

\item{temperatura}{Informar a temperatura.}
}
\value{
Json
}
\description{
Submete decisões judiciais à  API da OPENAI e obtêm os resultados em json.
}
