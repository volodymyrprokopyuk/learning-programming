# source("main.R")

f1 <- function(x) {
    function() {
        x + 20
    }
}
# f1(1)()

# Name masking
f2 <- function() {
    x <- 1
    y <- 2
    c(x, y)
}
# f2()
rm(f2)

x <- 10
f3 <- function() {
    y <- 20
    c(x, y)
}
# f3()
rm(x, f3)

# Closures
f4 <- function(x) {
    y <- 2
    function() {
        c(x, y)
    }
}
# f4(1)()

# 1 + (2 * 3) is equivalent to `+`(1, `*`(2, 3))

# if (i == 1) { print("ok") } else { print("oh") }
# is equvalent to
# `if`(i == 1, print("ok"), print("oh"))

# for (i in 1:4) { print(i) } is equivalent to `for`(i, 1:4, print(i))

# x[3] is equivalent to `[`(x, 3)

# { print("a"); print("b") } is equivalent to `{`(print("a"), print("b"))

add <- function(x, y) { x + y }
# sapply(1:10, add, 10)
# sapply(1:10, `+`, 10)

# mean(1:10, na.rm = T) is equivalent to do.call(mean, list(1:10, na.rm = T))

# Default agruments
f5 <- function(a = 1, b = a * 10) {
    c(a, b)
}
# f5()

# Lazy arguments
f6 <- function(x) {
    # force(x) # force agrument evaluation
    1
}
# f6(stop("Lazy agrument evaluation"))

# Match otherwise not matched ... special argument
f7 <- function(...) {
    list(...)
}
# f7(a = 10, b = 20, 1, 1.2, T, "Vlad")

# Infix functions
`%+%` <- function(a, b) {
    paste(a, b, sep = " ")
}
# "Vlad" %+% "Lana"

# Replacement functions
`modify_second<-` <- function(x, value) {
    x[2] <- value
    x
}
mx <- c(1, 2, 3, 4)
modify_second(mx) <- 20
# mx

`modify_position<-` <- function(x, position, value) {
    x[position] <- value
    x
}
modify_position(mx, 1) <- 10
# mx

# Copy-on-modify
comx <- c(1, 2, 3)
f8 <- function(x) {
    x[1] <- 10
    x
}
# f8(comx)
# comx

# Invisible return
f9 <- function() {
    invisible(1)
}
f9()
(f9()) # force output
