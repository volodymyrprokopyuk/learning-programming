
The downloads are organized into three directories:


datasets
	contains the datasets
	publicly available data sets are not copied,
	instead, the URL where the dataset can be found is given 
	in a file with the extension .readme

gnuplot
	contains the gnuplot command files

misc
	contains files in other formats (Perl, Python, Latex, HTML)



To run the examples, proceed as follows:
1) change into the datasets directory (NOT into the gnuplot directory!)
2) start gnuplot
3) issue gnuplot commands from the book, like so:
   plot "marathon" using 1:2 with boxes

OR

3) load a command file using the "load" command and the relative path, like so:
   load "../gnuplot/orders.gp"
