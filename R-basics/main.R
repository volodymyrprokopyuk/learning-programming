# source("main.R")

f1 <- function(x) {
    function() {
        x + 20
    }
}

# f1(1)()

# 1 + (2 * 3) equivalent `+`(1, `*`(2, 3))

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
f4(1)()
