---
output:
  md_document:
    variant: markdown_github
---

[![Build Status](https://travis-ci.org/courtsbr/JurisMiner.svg?branch=master)](https://travis-ci.org/courtsbr/JurisMiner)


# JurisMiner

The goal of JurisMiner is to manipulate judicial decisions texts from Brazilian courts. This package must be used after you downloaded the data using [esaj](https://github.com/courtsbr/esaj) package.

## Installation

You can install JurisMiner from github with:


```r
# install.packages("devtools")
devtools::install_github("courtsbr/JurisMiner")
```

## Example

After downloading an opinion (acórdão) from high court and read the text to R, you can inspect words in context and mine relevant information. 


```r
## Inspect the word related to the decision of an appeal:
provimento<-pt_kwic(opinions,docnames, keyword="provimento")
```

Besides inspecting, you can automate the classification of the decisions according to whether the court reversed (provimento) or affirmed the lower court decision (desproveu):


```r
decision<-sg_decision(opinions,docnames,keyword="provimento")
```



