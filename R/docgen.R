#' Automatically generate package documentation from a gentest() result
#'
#' Generates automatically the documentation for a function tested with
#' \code{\link[gentest]{gentest}}. Takes into consideration only those
#' results which did not return errors during tests.
#' of the current working directory
#' @param gentest_restult Value returned by \code{\link[gentest]{gentest}}.
#' @param title String, the title of the .
#' Default: a function name stored in \code{gentest_result}.
#'
#' The documentation file is saved as \code{XXX.Rd} -- where
#' \code{XXX} is determined by the function name stored in
#' the \code{gentest_result} -- in the sub-directory \code{man}
#' of the current working directory. This sub-directory needs to be
#' created first, if it does not yet exist.
#' @export
docgen <- function(gentest_result,
                   title=attr(gentest_result,'function')) {
    stopifnot(gentest_result %>%
                  inherits('gentest_result'),
              title %>% is.character,
              length(title)==1)
    stopif(nrow(gentest_result)==0)
    without_errors <-
        gentest_result %>%
        extract(with(., ..error %>% is.na),)
    if (nrow(without_errors)==0)
        stop('There are no test results without errors.')
    fname <-
        gentest_result %>%
        attr('function')
    HeaderSection <-
        paste('% Generated by function `doctest` from package `gentest`',
              paste0('\\name{',fname,'}'),
              paste0('\\alias{',fname,'}'),
              paste0('\\title{',title,'}'),
              paste('\\usage{',
                    tryCatch(get(fname),
                             error = function(e)
                                 geterrmessage() %>%
                                 {`if`(grepl("object 'function(.*).* not found",.),
                                       stop('Function `docgen` does not support anonymous functions.',
                                            call.=FALSE),
                                       e)}) %>%
                        args %>%
                        deparse %>%
                        head(-1) %>%
                        paste(collapse='\n') %>%
                        sub('^function ', fname %>% ifNotSyntacBtick, .),
                    '}', sep='\n'),
              sep='\n')
    ArgumentsSection <-
        gentest_result %>%
        attr('arguments') %>%
        paste0('..type') %>%
        sapply(function(name)
            without_errors[[name]] %>%
                unique      %>%
                paste(collapse=' \\strong{or} \\cr\n') %>%
                paste0('\\item{',
                       name %>% sub('\\.\\.type$',"",.),
                       '}{',.,'}')) %>%
        paste(collapse='\n') %>%
        paste('\\arguments{',.,'}', sep='\n')
    uniqVTypes <-
        without_errors$..value..type %>%
        unique
    # Multiple \tab below for better rendering in RStudio's help viewer
    # (thin column separation spaces)
    ValueSection <-
        `if`(length(uniqVTypes)==1,
             uniqVTypes,
             # A table:
             without_errors %>%
                 extract(, colnames(.) %>%
                             txtfilter('\\.\\.type$') %>%
                             lastToFirst) %>%
                 extract(do.call(order,.),) %>%
                 within(..value..type <- ..value..type %>%
                            paste0('\\strong{',.,'} \\tab \\tab')) %>%
                 do.call(function(...)
                     paste(..., sep=' \\tab \\tab \\tab '),.) %>%
                 paste(collapse='\\cr \n') %>%
                 paste(paste0('\\tabular{',rep.int('l',120) %>% paste(collapse=""),'}{'),
                       paste('\\strong{\\emph{\\code{Returned value}}} \\tab \\tab \\tab \\tab',
                             gentest_result %>%
                                 attr('arguments') %>%
                                 paste0('\\strong{\\emph{\\code{',.,'}}}') %>%
                                 paste(collapse=' \\tab \\tab \\tab ') %>%
                                 paste('\\cr'),
                             sep=' \\tab '),
                       .,'}', sep='\n')) %>%
        paste('\\value{',
              .,'}',
              sep='\n')
    paste(HeaderSection,
          ArgumentsSection,
          ValueSection,
          sep='\n') %>%
        gsub('\\%','\\\\%',.) %>%
        gsub('^\\\\%','%',.) %>%
        paste0('\n') %>%
        cat(file =
                paste('man',
                      paste0(fname,'.Rd'),
                      sep='/') %T>%
                      {message('Saved the doc file in the current working directory as:\n',
                               paste0('"',.,'"'))})
}

# Helpers

txtfilter <- function(strvec, patt)
    strvec[grep(patt,strvec)]

lastToFirst <- function(vec)
    c(tail(vec,1), head(vec,-1))

isSyntactic <- function(str)
    make.names(str)==str

ifNotSyntacBtick <- function(str)
    if (str %>% isSyntactic)
        str else paste0('`',str,'`')


