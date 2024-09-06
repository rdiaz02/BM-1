## This is a temporary hack.

library(rmarkdown)
library(BiocStyle)
library(bookdown)
render("Lesson-1.Rmd", output_format = bookdown::pdf_document2(toc = TRUE, toc_depth = 4, keep_tex = TRUE))
