unset border
unset tics
unset key
set view 75,35
splot "ch10_pyramid.txt" i 0 w p pt 7 ps 3, "" i 1 u 1:2:3:($4-$1):($5-$2):($6-$3) w vectors nohead
