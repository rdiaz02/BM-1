## Common R/knitr setup for the Rnw files of this course.
##
## Source it from the setup chunk of each Rnw file (knitr runs with the
## Rnw file's directory as the working directory):
##
##     <<setup, include=FALSE, cache=FALSE>>=
##     require(knitr)
##     source("knitr-setup.R")
##     @
##
## Its purpose is to keep the output of R inside the text block, instead
## of running past the right margin. Three things are needed:
##
##  1. options(width = ...): the number of columns R uses when it wraps
##     printed output. This takes care of most of the output.
##
##  2. A set of knitr hooks that hard-wrap the lines that R itself does
##     *not* wrap. Several print methods build their output with cat()
##     and therefore ignore options(width); the usual offender is
##         alternative hypothesis: true difference in means between group
##         Cancer and group NC is not equal to 0
##     from t.test() and friends. Warnings and messages are not wrapped
##     by R either. The hooks below wrap all of them.
##
##  3. A guard, because attaching a package (or some function called from
##     a chunk) can silently reset options("width") back to 80 in the
##     middle of the document. The width set here is therefore treated as
##     a maximum: it is re-imposed before every chunk, and the wrapping
##     hooks never use anything wider. A narrower options(width = ...)
##     set by hand in a chunk is respected.
##
## Choosing the width. The number of characters that fit in a line is
## (text width) / (width of one character of the typewriter font), and a
## character of the typewriter font is about 0.6 x (font size). knitr
## prefixes each line of output with "## ", so the printed lines are
## options("width") + 3 characters long. For a document with 30 mm
## margins (text width 150 mm = 427 pt) and Courier (the typewriter font
## of the times package):
##
##     code at \normalsize (11 pt): 6.6 pt/char -> 64 characters fit
##     code at \small      (10 pt): 6.0 pt/char -> 71 characters fit
##
## so with a width of 65 the code chunks must be typeset at \small, with
## opts_chunk$set(size = "small"). Text widths and typewriter fonts
## differ between documents (BiocStyle, for instance, uses a narrower
## one, where about 75 characters fit at \normalsize), so if in doubt
## check the .log of the document for "Overfull \hbox".
##
## This does *not* wrap the R code you write in a chunk: knitr never
## breaks source lines. Those have to be kept short by hand.


## The width, and the maximum width, of the printed output. Call this
## after source("knitr-setup.R") to use a different value in a given
## file; it can also be called inside a chunk.
.max_output_width <- local({
    w <- 65
    function(new) {
        if (!missing(new)) w <<- new
        w
    }
})

set_r_output_width <- function(width) {
    .max_output_width(width)
    options(width = width)
    invisible(width)
}

set_r_output_width(65)


## Re-impose the maximum before every chunk, in case something has
## widened options("width") behind our back. Note that the argument of
## the hook is called `options`, so base::options() must be spelled out.
knitr::opts_hooks$set(keep.width = function(options) {
    if (isTRUE(options$keep.width) &&
        getOption("width") > .max_output_width())
        base::options(width = .max_output_width())
    options
})
knitr::opts_chunk$set(keep.width = TRUE)


## Figures produced by a chunk are typeset as the first line of a new
## paragraph, so they start \parindent (17 pt in an 11pt article) to the
## right, and a full-width figure sticks out on the right. Centring them
## removes the indentation. (This affects only the figures produced by
## the chunks, not the ones included by hand with \includegraphics.)
knitr::opts_chunk$set(fig.align = "center")


## Hard-wrap the output lines that R did not wrap itself. Applied to
## regular output and to messages, warnings and errors.
local({
    ## x is what knitr is about to write to the .tex file: the lines
    ## already carry the comment prefix ("## " by default).
    wrap_lines <- function(x, options) {
        n <- if (!is.null(options$linewidth)) options$linewidth
             else getOption("width")
        n <- min(n, .max_output_width())
        if (!is.finite(n) || n < 20) return(x)
        cmt <- options$comment
        prefix <- if (is.null(cmt) || is.na(cmt) || !nzchar(cmt)) ""
                  else paste0(cmt, " ")
        ends_nl <- grepl("\n$", x)
        lines <- unlist(strsplit(x, "\n", fixed = TRUE))
        lines <- unlist(lapply(lines, function(l) {
            has_prefix <- nzchar(prefix) && startsWith(l, prefix)
            body <- if (has_prefix) substring(l, nchar(prefix) + 1L) else l
            ## only touch the lines that are too long, so that the
            ## alignment of tables and of printed data frames is kept
            if (nchar(body) <= n) return(l)
            paste0(if (has_prefix) prefix else "",
                   strwrap(body, width = n, exdent = 2))
        }), use.names = FALSE)
        x <- paste(lines, collapse = "\n")
        if (ends_nl) x <- paste0(x, "\n")
        x
    }
    for (hook_name in c("output", "message", "warning", "error")) {
        local({
            old_hook <- knitr::knit_hooks$get(hook_name)
            new_hook <- function(x, options) old_hook(wrap_lines(x, options),
                                                      options)
            knitr::knit_hooks$set(structure(list(new_hook),
                                            names = hook_name))
        })
    }
})
