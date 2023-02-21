##' A placeholder function using roxygen
##'
##' This function shows a standard text on the console. In a time-honoured
##' tradition, it defaults to displaying \emph{hello, world}.
##' @param txt An optional character variable, defaults to \sQuote{world}
##' @return Nothing is returned but as a side effect output is printed
##' @examples
##' hello2()
##' hello2("and goodbye")
##' @export
hello2 <- function(txt = "world") {
    cat("Hello, ", txt, "\n")
}
