###### Generators

#' Generator for Boolean values (logical: TRUE and FALSE)
#' @export
Bool <- function(vector_length=100) {
    stopifnot(is.numeric(vector_length),
              vector_length>=1)
    c(TRUE, FALSE) %>%
        maybeSample(vector_length) %>%
        extendToVectorLength(vector_length) %>%
        Name('Bool')
}

#' Generator for strings
#' @param string_length A numeric vector of string lengths
#' (in terms of the numbers of characters) in a vector
#' to be generated. Each element must be >= 0.
#' @param vector_length A length of the vector to be generated.
#' It must be >= 1.
#'
#' length of \code{string_length} must be equal \code{vector_length}
#' or \code{string_length} must be a scalar (\code{length(string_length) == 1}).
#' The generated strings include only ASCII characters in the code range 32--126.
#' @export
String <- function(string_length = replicate(vector_length,
                                             sample.int(101,1) - 1),
                   vector_length = 100) {
    stopifnot(is.numeric(vector_length),
              is.numeric(string_length),
              vector_length>=1,
              all(string_length>=0),
              length(string_length) == vector_length |
                  length(string_length) == 1)
    `if`(length(string_length)==1,
         rep.int(string_length,
                 vector_length),
         string_length) %>%
        sapply(function(x)
            if (x==0) "" else
                SingleCharacter(x) %>%
                paste0(collapse="")) %>%
        Name(paste0('String(',
                    if (sd(string_length) %>%
                        zeroIfNA %>%
                        equals(0))
                        paste('string_length =',string_length) else
                            'diverse string_length',
                    ')'))
}

#' Generator for single characters
#' @param vector_length A length of the vector to be generated.
#' It must be >= 1.
#' The generated characters are only from the ASCII code range 32--126.
#' @export
SingleCharacter <- function(vector_length=100) {
    stopifnot(is.numeric(vector_length),
              vector_length>=1)
    seq.int(32,126) %>% # printable chars' ASCII codes, https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters
        as.raw %>%
        rawToChar %>%
        strsplit("") %>%
        extract2(1) %>%
        maybeSample(vector_length) %>%
        extendToVectorLength(vector_length) %>%
        Name('SingleCharacter')
}

#' Generator for numbers (doubles) based on runif()
#' @param bottom minimum values that may be generated
#' @param top maximum values that may be generated
#' @param vector_length A length of the vector to be generated.
#' It must be >= 1.
#' @export
NumericInRange <- function(bottom, top, vector_length=100) {
    stopifnot(is.numeric(vector_length),
              vector_length>=1,
              is.numeric(bottom),
              is.numeric(top),
              length(bottom)==1,
              length(top)==1,
              bottom < top)
    runif(vector_length, bottom, top) %>%
        Name(paste0('NumericInRange(',bottom,',',top,')'))
}

#' Generator for integer numbers based on sample()
#' @param bottom minimum values that may be generated
#' @param top maximum values that may be generated
#' @param vector_length A length of the vector to be generated.
#' It must be >= 1.
#' @export
IntegerInRange <- function(bottom, top, vector_length=100) {
    stopifnot(is.numeric(vector_length),
              vector_length>=1,
              is.numeric(bottom),
              is.numeric(top),
              length(bottom)==1,
              length(top)==1,
              round(bottom)==bottom,
              round(top)==top,
              bottom < top)
    seq.int(bottom, top) %>%
        maybeSample(vector_length) %>%
        extendToVectorLength(vector_length) %>%
        Name(paste0('IntegerInRange(',bottom,',',top,')'))
}

packInto <- function(..., ..container,
                     ..container_name=deparse(substitute(..container)))
    `if`(identical(..container, data.frame),
         ..container(..., stringsAsFactors=FALSE), # otherwise the implicit conversion wipes out the `gentest_test` attribute
         ..container(...)) %>%
    Name(paste(names(.),
               sapply(., attr, 'gentest_type'),
               sep=' = ') %>%
             paste(collapse=', ') %>%
             paste0(..container_name,
                    '(',.,')'))
#' Generator for lists
#'
#' It may contain other generators.
#' @export
List <- function(...)
    packInto(..., ..container=list)

#' Generator for data frames
#'
#' It may contain other generators.
#' The elements of the data frame must
#' have the equal number of elements
#' (equal the number of rows of the data frame).
#' @export
DataFrame <- function(...) {
    ..a <- substitute(list(...)) %>%
        tail(-1) %>%
        paste0(names(.) %>%
        {ifelse(.=="", "", paste0(.,' = '))},
        .) %>%
        paste(collapse=', ')
    # In order to handle a common error: "arguments imply differing number of rows":
    tryCatch(packInto(..., ..container=data.frame),
             error = function(e)
                 stop('in DataFrame(',..a,')\n',
                      geterrmessage(),
                      call.=FALSE))
}

###### Modifiers

#' Modifier reducing vector to scalar (a single element)
#'
#' Cuts the length of the generated vector \code{vec} to 1
#' by selecting a random element of the vector.
#' @export
Scalar <- function(vec) {
    stopif(length(vec)<1)
    vec %>%
        extract(sample(seq_along(.),1)) %>%
        # Cannot use `addToName` below because the extraction wipes out the original `gentest_test` attribute
        Name(paste(attr(vec,
                        'gentest_type'),
                   'Scalar'))
}

#' Modifier injecting missing observations (NAs)
#'
#' Injects randomly missing observations (NA values)
#' replacing about 100*\code{share} per cent of
#' elements.
#' @export
withNA <- function(vec, share=.1) {
    stopifnot(share %>% is.numeric,
              length(share)==1,
              0<=share & share<=1)
    vec %>%
        insert(sample(seq_along(.),
                      length(.)*share),
               NA) %>%
        addToName(paste0('withNA(in ',
                         (100*share) %>% round(2),
                         '% of observations)'))
}

#' Modifier injecting observations with 0 (zeros)
#'
#' Injects randomly 0 (zeros)
#' replacing about 100*\code{share} per cent of
#' elements.
#' @export
withZero <- function(vec, share=.1) {
    stopifnot(vec %>% is.numeric,
              share %>% is.numeric,
              length(share)==1,
              0<=share & share<=1)
    vec %>%
        insert(sample(seq_along(.),
                      length(.)*share),
               0) %>%
        addToName(paste0('withZero(in ',
                         (100*share) %>% round(2),
                         '% of observations)'))
}

###### Wrappers

#' Wrapper for two alternative generators
#'
#' To be used to generate values/vectors of two different
#' types/classes.
#' @export
`%|%` <- function(x,y)
    `if`(x %>% is.atomic && y %>% is.atomic,
         list(namedIfConstant(x), namedIfConstant(y)),
         `if`(x %>% is.atomic && y %>% is.atomic %>% not,
              c(AsOne(namedIfConstant(x)), y),
              `if`(y %>% is.atomic && x %>% is.atomic %>% not,
                   c(x, AsOne(namedIfConstant(y))),
                   c(x,y))))

#' Convenience wrapper around many different generators
#'
#' A wrapper around
#' \preformatted{
#' (Bool() \%>\% withNA) \%|\%
#' (String() \%>\% withNA) \%|\%
#' (SingleCharacter() \%>\% withNA) \%|\%
#' (NumericInRange(-1e6,1e6) \%>\% withZero \%>\% withNA) \%|\%
#' (IntegerInRange(-1e6,1e6) \%>\% withZero \%>\% withNA)
#' }
#' @export
Anything <- function()
    (Bool() %>% withNA) %|%
    (String() %>% withNA) %|%
    (SingleCharacter() %>% withNA) %|%
    (NumericInRange(-1e6,1e6) %>% withZero %>% withNA) %|%
    (IntegerInRange(-1e6,1e6) %>% withZero %>% withNA)

#' Wrapper for vector generators to be used 'as one' insted of element-wise
#'
#' To be used if the generated vector should be used as whole vector rather
#' than its elements one-by-one.
#' @export
AsOne <- function(x)
    list(x)

#' Wrapper for vector generators replicating randomly whole vectors
#'
#' To be used to generate different vectors of the same type/class.
#' @export
Replicate <- function(expr, times=10) {
    stopifnot(times %>% is.numeric,
              length(times)==1)
    stopif(times<1)
    eval(bquote(replicate(.(times),
                          .(substitute(expr)),
                          simplify = FALSE))) %>%
        addAttr('gentest_type',
                extract2(.,1) %>%
                    attr('gentest_type'))
}

###### Helpers

#' Add a name, to be used ulitmately by docgen(), to the value(s) used in the gentest()
#' @param obj A return value of a generator function
#' @param name String, a human readable name for the generator
#' @export
Name <- function(obj, name) {
    stopifnot(name %>% is.character,
              length(name)==1)
    obj %>%
        addAttr('gentest_type', name)
}
# Name <- function(obj, name) {
#     stopifnot(name %>% is.character,
#               length(name)==1)
#     `if`(obj %>% is.atomic,
#          obj %>%
#              lapply(addAttr,
#                     'gentest_type', name),
#          obj %>%
#              addAttr('gentest_type', name))
# }

namedIfConstant <- function(x)
    `if`(x %>% is.atomic &&
             length(x)==1 &&
             x %>% attr('gentest_type') %>% is.null,
         x %>% Name(x %>%
                        as.character %>%
                        dQuoteIfStr(x)),
         x)

dQuoteIfStr <- function(x,y)
    if (y %>% is.character)
        paste0('"',x,'"') else x

zeroIfNA <- function(x)
    if (x %>% is.na) 0 else x

addToName <- function(obj, add) {
    stopifnot(add %>% is.character,
              length(add)==1)
    stopif(obj %>%
               attr('gentest_type') %>%
               is.null)
    obj %>%
        addAttr('gentest_type',
                paste(attr(., 'gentest_type'),
                      add))
}

insert <- `[<-`

stopifnot <- function(...)
    .stop(..., ..pred=`!`)

stopif <- function(...)
    .stop(..., ..pred=identity)

.stop <- function(..., ..pred) {
    isNegation <- identical(..pred, `!`)
    line1 <- if (isNegation)
        'expected' else 'not allowed'
    line2 <- if (isNegation)
        'not true' else 'encountered'
    mapply(function(val,expr)
        if (..pred(val))
            stop(call.=FALSE, '\nIn `',
                 deparse(sys.call(-4), width.cutoff=500),
                 paste0('`\n',line1,'\n`'),
                 deparse(expr, width.cutoff=500),
                 paste0('`\nis ',line2,'.')),
        val = list(...),
        expr = substitute(list(...)) %>%
            as.list %>%
            tail(-1))
}

maybeSample <- function(vec, vector_length)
    `if`(vector_length < length(vec),
         sample(vec, vector_length),
         vec)

extendToVectorLength <- function(vec, vector_length)
    vec %>%
    rep.int(ceiling(vector_length/length(vec))) %>%
    extract(seq_len(vector_length))
