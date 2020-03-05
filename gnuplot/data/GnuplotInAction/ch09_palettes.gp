
# Gray scale
set palette model RGB defined ( 0 'black', 1 'white' )


# Gray scale (in HSV space)
set palette model HSV functions 1,0,gray


# Heat scale 1: starting at red
set palette model RGB defined ( 0 'red', 1 'yellow', 2 'white' )


# Heat scale 2: including black
set palette model RGB defined ( -1 'black', 0 'red', 1 'yellow', 2 'white' )


# Rainbow
set palette model HSV functions gray,1,1


# Improved rainbow
f(x) = x < 5./6 ? 2./3 - 4./5*x : 1 - (x-5./6)
set palette model HSV functions f(gray),1,1


# Geo scale (hue palette)
set palette model RGB defined ( 0 '#3366ff', 1 '#99ffcc', 2 '#339900', 3 '#66ff33', 4 '#996633', 5 '#ff9900', 6 '#ffff33' )


# Blue-white-red
set palette model RGB defined ( -1 'blue', 0 'white', 1 'red' )


# White-blue-red-white with sharp transition
set palette model RGB defined ( -1 'white', 0 'blue', 0 'red', 1 'white' )


# Blue-green (luminance palette)
green = 1.0/3.0
blue = 2.0/3.0
set palette model HSV defined ( 0 blue 0.6 0.6, 1 blue 0.5 1, 2 green 0.5 1, 3 green 0.7 0.5 )


# Traffic light 
set palette model RGB defined ( 0 'green', 1 'green', 1 'yellow', 2 'yellow', 2 'red', 3 'red' )


# Tree rings
hue(x) = x < 1./8. || x > 7./8. ? 0 : (8*x-1)/6.0
sat(x) = x < 3.0/16.0 || x > 15.0/16.0 ? 0 : (1+cos(8*2*pi*x))/2
lum(x) = x < 1.0/16.0 ? 0 : (1+cos(8*2*pi*x))/2
stp(x,y) = x < y ? 0 : 1
w = 0.99
set palette model HSV functions hue(gray), stp( sat(gray), w ), gray + (1-gray)*stp(lum(gray), w)
