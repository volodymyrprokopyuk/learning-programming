#!/usr/bin/env bash

set -eu

source config/config.sh

function plot {
    local plot_data=$1
    local plot_name=$2

    gnuplot \
        -e "input_data='$DATA_DIR/$plot_data'" \
        -e "output_plot='$PLOT_DIR/$plot_name.png'" \
        $PLOT_DIR/$plot_name.gplot
}

# plot "ch01_marathon.txt" "marathon"
# plot "ch01_dlacluster.txt" "cluster"
plot "" "function"
