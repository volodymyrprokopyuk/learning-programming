function format {
    local target=${1?ERROR: mandatory target is not provided}
    local line_length=88
    local tab_width=4

    prettier --print-width $line_length --tab-width $tab_width \
        --no-bracket-spacing --arrow-parens always --trailing-comma es5 \
        --write $target
}

function validate {
    local target=${1?ERROR: mandatory target is not provided}

    tslint --format verbose $target
}

function compile {
    tsc
}
