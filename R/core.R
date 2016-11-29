#' @import magrittr
NULL

#' Test a function with different combinations of arguments, possibly generated randomly
#' @param ..f  A non-anonymous function to be tested
#' (and possibly to be later documented with \code{\link[gentest]{docgen}}).
#' @param ..p  An optional function with one argument (i.e. arity 1)
#' to be applied to the value retunred by function \code{f}. By
#' default \code{p = \link{identity}}. Useful for applying additional
#' tests/checks that do not have to or should not be included in function \code{f}.
#' @param ...  Different values to be applied as arguments
#' under different calls of function \code{f}. If vectors are to be used
#' as single objects (rather than single value by value) they should be wrapped in
#' a \code{\link{list}}, or \code{\link[gentest]{AsOne}}
#' (an alias for \code{list} for clarity), or \code{\link[gentest]{Replicate}}.
#' @return A \code{data.frame} (S3 class \code{gentest_result}) for all
#' the combinations of argument values specified
#' in \code{...}, including:
#' \itemize{
#' \item the columns with the argument values
#' \item return values (column \code{..value}),
#' \item warnings (columns \code{..warning}),
#' \item errors (columns \code{..error}),
#' \item the character columns with the suffix \code{..type}
#' which describe the class
#' }
#' @export
gentest <- function(..f, ..p=identity, ...) {
    arguments <- substitute(list(...)) %>%
        names %>%
        tail(-1)
    fname <- substitute(..f) %>%
        deparse
    mlapplyCbind(function(...) {
        Res <- tryCatch_(list(..p(..f(...))))
        data.frame(..value = I(Res$value),
                   ..warning = I(Res$warning %>% toText),
                   ..error = I(Res$error %>% toText),
                   stringsAsFactors = FALSE)
    },
    ...) %>%
        addAttr('arguments',
                arguments) %>%
        addAttr('function',
                fname) %>%
        addClass('gentest_result')
}

#' Stop if gentest() results include errors
#' @param gentest_restult Value returned by \code{\link[gentest]{gentest}}.
#' @return If there are any errors, it stops with an error and
#' stores the part of the \code{gentest_restult} in global environment
#' as object \code{ERROR}. If there are no errors, it returns the
#' \code{gentest_restult} invisibly.
#' @export
stopIfErr <- function(gentest_result) {
    stopifnot(gentest_result %>%
                  inherits('gentest_result'))
    with_errors <-
        gentest_result %>%
        extract(.$..error %>%
                    is.na %>% not,)
    if (nrow(with_errors)!=0) {
        .GlobalEnv$ERROR <- with_errors
        stop('in the tested function `',
             gentest_result %>% attr('function'),'`!\n',
             'See the data frame stored in\n',
             'global environment in object `ERROR`.',
             call.=FALSE)
    } else {
        message('No error in ',
                gentest_result %>% attr('function'),'.')
        invisible(gentest_result)
    }
}

#' Stop if gentest() results include warnings
#' @param gentest_restult Value returned by \code{\link[gentest]{gentest}}.
#' @return If there are any warnings, it stops with an error and
#' stores the part of the \code{gentest_restult} in global environment
#' as object \code{WARNING}. If there are no errors, it returns the
#' \code{gentest_restult} invisibly.
#' @export
stopIfWarn <- function(gentest_result) {
    stopifnot(gentest_result %>%
                  inherits('gentest_result'))
    with_warnings <-
        gentest_result %>%
        extract(.$..warning %>%
                    is.na %>% not,)
    if (nrow(with_warnings)!=0) {
        .GlobalEnv$WARNING <- with_warnings
        stop('there are warnings in the tested function `',
             gentest_result %>% attr('function'),'`!\n',
             'See the data frame stored in\n',
             'global environment in object `WARNING`.',
             call.=FALSE)
    } else {
        message('No warnings in ',
                gentest_result %>% attr('function'),'.')
        invisible(gentest_result)
    }
}

#' Convenience wrapper around stopIfErr() and stopIfWarn()
#'
#' Simply \preformatted{
#' library(magrittr)
#' x \%>\%
#'     stopIfErr \%>\%
#'     stopIfWarn
#' }
#' @export
stopIfErrOrWarn <- function(x)
    x %>%
    stopIfErr %>%
    stopIfWarn

#### Helpers

duplicated.data.frame <- # to avoid collisions if one of the tested fun's args is `sep`
function (x, incomparables = FALSE, fromLast = FALSE, ...)
{
    if (!identical(incomparables, FALSE))
        .NotYetUsed("incomparables != FALSE")
    if (length(x) != 1L)
        duplicated(do.call(paste, x %>% set_names(NULL)), # to avoid collisions if one of the tested fun's args is `sep`
                   fromLast = fromLast)
    else duplicated(x[[1L]], fromLast = fromLast, ...)
}

# From http://stackoverflow.com/a/24569739
tryCatch_ <- function(expr) {
    warn <- err <- NA
    value <- withCallingHandlers(
        tryCatch(expr,
                 error=function(e) {
                     err <<- e
                     e
                 }), warning=function(w) {
                     warn <<- w
                     invokeRestart("muffleWarning")
                 })
    list(value = if (err[1] %>% is.na) value else list(value),
         warning=warn,
         error=err)
}

isError <- function(x)
    x %>% is.character &&
    length(x)==1 &&
    (x=='...error...') %>%
    `if`(is.na(.),FALSE, .)


ifIsError <- function(x, f1, f2)
    ifelse(x %>% isError,
           f1(x), f2(x))

constant <- function(v)
    function(ignore_me) v

list_of_dfs_to_df_of_lists <- function(ldf)
    data.frame(..value=I(lapply(ldf, . %>% `$`(..value))),
               ..warning=I(lapply(ldf, . %>% `$`(..warning))),
               ..error=I(lapply(ldf, . %>% `$`(..error))),
               stringsAsFactors = FALSE)

toText <- function(x)
    if (x[1] %>% is.na %>% not)
        x %>%
    as.character %>%
    paste(collapse=' ') %>%
    # to save space in the diplayed output below:
    sub('^simpleWarning.*\\(.*\\): ',"",.) %>%
    gsub('\n',"",.) %>%
    # capitalize first letter for readability:
    gsub("(^[[:alpha:]])", "\\U\\1", ., perl=TRUE) else x

mlapplyCbind <- function(FUN, ...) {
    L <- list(...)
    df <-
        L %>%
        expand.grid(stringsAsFactors=FALSE,
                    KEEP.OUT.ATTRS=FALSE) %>%
        extract(!duplicated(.), , drop=FALSE) %>%
        set_rownames(NULL) %>% # reset rownames
        importNameAttrs(L) # restore attributes
    df_names <-
        df %>%
        colnames %>%
        lapply(function(name)
            df[[name]] %>%
                ifAnonListSapply(function(x) x %>%
                                     attr('gentest_type') %>%
                                     ifNull(extractClass(x)))
        ) %>%
        as.data.frame(stringsAsFactors=FALSE) %>%
        set_colnames(paste0(colnames(df),'..type'))
    df %>%
        split(nrow(.) %>% seq_len) %>%
        lapply(. %>%
                   lapply(function(x)
                       if (x %>% is.list) x[[1]] else x) %>%
                   do.call(FUN, .)) %>%
        list_of_dfs_to_df_of_lists %>%
        cbind(df, df_names, .) %>%
        within(..value <- ..value %>%
                   lapply(extract2, 1)) %>% # unpacking from the nested list
        within(..value..type <- ..value %>%
                   sapply(. %>%
                              ifIsError(constant('Error'),
                                        . %>% extractClass)))
}

ifNull <- function(x,y)
    if (x %>% is.null)
        y else x

ifAnonListSapply <- function(x, f)
    if (x %>% is.list &&
        x %>% attr('gentest_type') %>% is.null)
        x %>% sapply(f) else f(x)

extractClass <- function(x)
    `if`(x %>%
    {inherits(.,'data.frame') | inherits(.,'list')},
    x %>%
        extractClassFromElements %>%
        paste(x %>%
                  classCollapsed,
              '(',.,')',
              sep="",
              collapse=""),
    x %>%
        classCollapsed)

extractClassFromElements <- function(y)
    y %>%
    sapply(extractClass) %>%
    paste(names(y), .,
          sep=' = ',
          collapse=', ')

classCollapsed <- function(x)
    x %>% class %>%
    paste(collapse=" ")

addClass <- function(obj, name) {
    # Cannot use the terser notation as for `addAttr` below
    # because it appears to mutate `obj` in place
    class(obj) <- c(class(obj),
                    name)
    obj
}

addAttr <- function(obj, attr_name, val)
    `attr<-`(obj, attr_name, val)

importNameAttrs <- function(df, L) {
    for (x in df %>% colnames)
        attr(df[[x]], 'gentest_type') <-
            attr(L[[x]], 'gentest_type') # %>%
            # ifNull(extractClass(L[[x]]))
    df
}

