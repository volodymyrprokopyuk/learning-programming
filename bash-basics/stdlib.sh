function ostype {
    local osname=$(uname -s)
    case $osname in
        "Linux") OSTYPE=Linux
        ;;
        *) OSTYPE=UNKNOWN
        ;;
    esac
    return 0
}


function is_even {
    local last_digit=$(echo $1 | sed 's/\(.*\)\(.\)$/\2/')
    case $last_digit in
        1|3|5|7|9)
            return 1
            ;;
        *)
            return 0
    esac
}


function is_connected {
    ping -c 1 'www.google.com' >/dev/null
}
