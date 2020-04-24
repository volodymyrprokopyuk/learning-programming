# source("main.R")

f1 <- function(x) {
    function() {
        x + 20
    }
}

f1(1)()

# 1 + (2 * 3) equivalent `+`(1, `*`(2, 3))
