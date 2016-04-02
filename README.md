### Example usage

	# Get the script

	curl https://raw.githubusercontent.com/geotheory/r-plot/master/rplot.R > rplot.R

	# Help

	Rscript rplot.R -h

	# Get some data

	Rscript -e "write.csv(mtcars, 'mtcars.csv')"; head -6 mtcars.csv

	# Scatterplot - car weight vs fuel efficiency

	Rscript rplot.R mtcars.csv wt mpg

	# Hashplot - car fuel efficiencies

	Rscript rplot.R mtcars.csv 1 mpg

	# Ordering plot data

	Rscript rplot.R mtcars.csv 1 mpg -o

	# bash function (e.g. for .bash_profile)

	rplot() { Rscript rplot.R "$1" ${*:2}; }

	# Iris dataset

	Rscript -e "write.csv(iris, 'iris.csv')"; head -6 iris.csv

	# Aggregating data for categorical averages

	rplot iris.csv Species Sepal.Length -am

	# Plotting a single numeric variable

	rplot iris.csv Sepal.Width

	# Reordered and without summary

	rplot iris.csv Sepal.Width -ox

	# Change scatterplot size

	rplot iris.csv Sepal.Width -ox -r 40 -c 80

	# Single categorical variable frequency (ie. aggregate by length)

	rplot iris.csv Species -al

	# Other data formats (eg. semicolon-seperated)

	Rscript -e "print(names(airquality)); write.table(airquality, 'airquality.csv', sep=';', row.names=T, col.names=F)"; head -3 airquality.csv

	# Specifying seperating-character, no-header row, and fields by column index (eg. Ozone 'V2' and Temp 'V5')

	rplot airquality.csv 2 5 -ns ";"

	# Passing r-plot a bash text object instead of file

	rplot "$(cat mtcars.csv)" mpg disp -x

### Output from above

	$
	$ # Get the script
	$
	$ curl https://raw.githubusercontent.com/geotheory/r-plot/master/rplot.R > rplot.R
	  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
	                                 Dload  Upload   Total   Spent    Left  Speed
	100 12015  100 12015    0     0   6264      0  0:00:01  0:00:01 --:--:--  6264
	$
	$ # Help
	$
	$ Rscript rplot.R -h

	**********************
	******* R-PLOT *******
	**********************

	This library plots a scatterplot or hashbar plot (bars made of hashes!) of a csv or a similarly formatted
	file or string in your console. If 2 numeric id_fields are provided a scatterplot will default, else hashbars.
	Required arguments: csv file/string, then column name(s)/index(ices) (values-column last for hashbars)

	NB read.table check.names=T so e.g. numeric colnames prepent 'X' and those with spaces have spaces replaced by '.'.
	Use '-Pz | head' to suppress the plot and see the colnames that are read in..

	USAGE
	Example csv call - scatterplot (by column name):
	    "Rscript rplot.R file.csv num_field1 num_field2"
	Example csv call - scatterplot (by column index):
	    "Rscript rplot.R file.csv col#1 col#3"
	Example csv call - hashbar plot:
	    "Rscript rplot.R file.csv id_field1 id_field2 value_field"
	Example csv text string call:
	    "Rscript rplot.R "$(cat file.csv)" id_field value_field"
	For convenience you can set up an alias in .bash_profile or equivalent, e.g.
	    "rplot() { Rscript /pathto/rplot.R $*; }"
	and call with:
	    "rplot file.csv field1 field2 etc.."

	OPTIONS:
	  Data handling:
	    -n   Specify no header row for input data. Use col indices instead
	    -s   sep character for input data (default `,`). Requires value e.g. ";"  "\t"  "" (inc. quotes)
	    -a   Aggregate (default `sum`) hashbar plot data by its categorical variables
	    -m   Aggregate by `mean` if -a selected
	    -l   Aggregate by `length` (count instances) if -a selected
	  Plotting:
	    -o   Reorder hashbar chart by value (also reorders data.frames)
	    -H   Override a default scatter plot with hashbar plot
	    -S   Override a default hashbar plot with scatter plot (NA values are removed)
	    -r   Scatterplot rows/height (default 20). Requires following value.
	    -c   Scatterplot cols/width (default 50). Requires following value.
	    -p   pch char/str (default `*`). requires value e.g. -p "." (inc. quotes)
	    -x   Suppress summary in case of scatter plot
	    -z   Suppress plot (eg. use with -P or -Q)
	  Other:
	    -h   Call this help (also --help)
	    -P   Output raw data.frame to console (truncated 1000 rows)
	    -Q   Output processed data.frame to console (truncated)

	$
	$ # Get some data
	$
	$ Rscript -e "write.csv(mtcars, 'mtcars.csv')"; head -6 mtcars.csv
	"","mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"
	"Mazda RX4",21,6,160,110,3.9,2.62,16.46,0,1,4,4
	"Mazda RX4 Wag",21,6,160,110,3.9,2.875,17.02,0,1,4,4
	"Datsun 710",22.8,4,108,93,3.85,2.32,18.61,1,1,4,1
	"Hornet 4 Drive",21.4,6,258,110,3.08,3.215,19.44,1,0,3,1
	"Hornet Sportabout",18.7,8,360,175,3.15,3.44,17.02,0,0,3,2
	$
	$ # Scatterplot - car weight vs fuel efficiency
	$
	$ Rscript rplot.R mtcars.csv wt mpg
	 ______________________________________________________
	|                                                      |
	|      *                                               |
	|                                                      |
	|          *                                           |
	|  **                                                  |
	|                                                      |
	|                                                      |
	|       *                                              |
	|         *                                            |
	|                       *                              |
	|            *         *                               | mpg
	|                                                      |
	|             * * * *   *                              |
	|                 *        *    *                      |
	|                          *                           |
	|                          *  *                        |
	|                      *    *      *                   |
	|                          **  *                   *   |
	|                               *                      |
	|                                                      |
	|                                                *  *  |
	|______________________________________________________|
	                         wt
	32 data rows plotted
	       wt             mpg
	 Min.   :1.513   Min.   :10.40
	 1st Qu.:2.581   1st Qu.:15.43
	 Median :3.325   Median :19.20
	 Mean   :3.217   Mean   :20.09
	 3rd Qu.:3.610   3rd Qu.:22.80
	 Max.   :5.424   Max.   :33.90
	$
	$ # Hashplot - car fuel efficiencies
	$
	$ Rscript rplot.R mtcars.csv 1 mpg
	32 data rows plotted
	Index                 mpg
	Mazda RX4             21     ##########################################
	Mazda RX4 Wag         21     ##########################################
	Datsun 710            22.8   ##############################################
	Hornet 4 Drive        21.4   ###########################################
	Hornet Sportabout     18.7   ######################################
	Valiant               18.1   ####################################
	Duster 360            14.3   #############################
	Merc 240D             24.4   #################################################
	Merc 230              22.8   ##############################################
	Merc 280              19.2   #######################################
	Merc 280C             17.8   ####################################
	Merc 450SE            16.4   #################################
	Merc 450SL            17.3   ###################################
	Merc 450SLC           15.2   ##############################
	Cadillac Fleetwood    10.4   #####################
	Lincoln Continental   10.4   #####################
	Chrysler Imperial     14.7   #############################
	Fiat 128              32.4   #################################################################
	Honda Civic           30.4   #############################################################
	Toyota Corolla        33.9   #####################################################################
	Toyota Corona         21.5   ###########################################
	Dodge Challenger      15.5   ###############################
	AMC Javelin           15.2   ##############################
	Camaro Z28            13.3   ###########################
	Pontiac Firebird      19.2   #######################################
	Fiat X1-9             27.3   #######################################################
	Porsche 914-2         26     ####################################################
	Lotus Europa          30.4   #############################################################
	Ford Pantera L        15.8   ################################
	Ferrari Dino          19.7   ########################################
	Maserati Bora         15     ##############################
	Volvo 142E            21.4   ###########################################
	$
	$ # Ordering plot data
	$
	$ Rscript rplot.R mtcars.csv 1 mpg -o
	32 data rows plotted
	Index                 mpg
	Toyota Corolla        33.9   #####################################################################
	Fiat 128              32.4   #################################################################
	Honda Civic           30.4   #############################################################
	Lotus Europa          30.4   #############################################################
	Fiat X1-9             27.3   #######################################################
	Porsche 914-2         26     ####################################################
	Merc 240D             24.4   #################################################
	Datsun 710            22.8   ##############################################
	Merc 230              22.8   ##############################################
	Toyota Corona         21.5   ###########################################
	Hornet 4 Drive        21.4   ###########################################
	Volvo 142E            21.4   ###########################################
	Mazda RX4             21     ##########################################
	Mazda RX4 Wag         21     ##########################################
	Ferrari Dino          19.7   ########################################
	Merc 280              19.2   #######################################
	Pontiac Firebird      19.2   #######################################
	Hornet Sportabout     18.7   ######################################
	Valiant               18.1   ####################################
	Merc 280C             17.8   ####################################
	Merc 450SL            17.3   ###################################
	Merc 450SE            16.4   #################################
	Ford Pantera L        15.8   ################################
	Dodge Challenger      15.5   ###############################
	Merc 450SLC           15.2   ##############################
	AMC Javelin           15.2   ##############################
	Maserati Bora         15     ##############################
	Chrysler Imperial     14.7   #############################
	Duster 360            14.3   #############################
	Camaro Z28            13.3   ###########################
	Cadillac Fleetwood    10.4   #####################
	Lincoln Continental   10.4   #####################
	$
	$ # bash function (e.g. for .bash_profile)
	$
	$ rplot() { Rscript rplot.R "$1" ${*:2}; }
	$
	$ # Iris dataset
	$
	$ Rscript -e "write.csv(iris, 'iris.csv')"; head -6 iris.csv
	"","Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"
	"1",5.1,3.5,1.4,0.2,"setosa"
	"2",4.9,3,1.4,0.2,"setosa"
	"3",4.7,3.2,1.3,0.2,"setosa"
	"4",4.6,3.1,1.5,0.2,"setosa"
	"5",5,3.6,1.4,0.2,"setosa"
	$
	$ # Aggregating data for categorical averages
	$
	$ rplot iris.csv Species Sepal.Length -am
	Aggregate function is mean
	150 data rows plotted
	Species      Sepal.Length
	setosa       5.01           #####################################################
	versicolor   5.94           ###############################################################
	virginica    6.59           ######################################################################
	$
	$ # Plotting a single numeric variable
	$
	$ rplot iris.csv Sepal.Width
	 ______________________________________________________
	|                                                      |
	|      *                                               |
	|                                                      |
	|            *                                         |
	|            *                                         |
	|   *  **                                              |
	|       **       **                      *    *        |
	|     *  *        *                                    |
	|   *     *    *                      *                |
	|  **** **********            *                *   *   |
	|         *        * *             *       *      *    | Sepal.Width
	|  *        * * * **      *            ** * *     *    |
	|  * *      * *     *   *      *                **     |
	|  * ***   *   * *    ***  ** *** * **** *  ** ** ***  |
	|                   **    **       *    * *****        |
	|                     *  *   **  *  *  *   *     *     |
	|                           *   **       *     *       |
	|                    *   **  *  *  * ** *          *   |
	|               *   *          * *                     |
	|                      * *                *            |
	|                     *                                |
	|______________________________________________________|
	                       Index
	150 data rows plotted
	     Index         Sepal.Width
	 Min.   :  1.00   Min.   :2.000
	 1st Qu.: 38.25   1st Qu.:2.800
	 Median : 75.50   Median :3.000
	 Mean   : 75.50   Mean   :3.057
	 3rd Qu.:112.75   3rd Qu.:3.300
	 Max.   :150.00   Max.   :4.400
	$
	$ # Reordered and without summary
	$
	$ rplot iris.csv Sepal.Width -ox
	 ______________________________________________________
	|                                                      |
	|                                                   *  |
	|                                                      |
	|                                                  *   |
	|                                                  *   |
	|                                                 **   |
	|                                               ***    |
	|                                              **      |
	|                                             **       |
	|                                       ******         |
	|                                     **               | Sepal.Width
	|                                *****                 |
	|                             ****                     |
	|                 ************                         |
	|            ******                                    |
	|         ****                                         |
	|        **                                            |
	|    ****                                              |
	|   **                                                 |
	|  *                                                   |
	|  *                                                   |
	|______________________________________________________|
	                       Index
	$
	$ # Change scatterplot size
	$
	$ rplot iris.csv Sepal.Width -ox -r 40 -c 80
	 ____________________________________________________________________________________
	|                                                                                    |
	|                                                                                 *  |
	|                                                                                    |
	|                                                                                    |
	|                                                                                    |
	|                                                                                *   |
	|                                                                               *    |
	|                                                                                    |
	|                                                                               *    |
	|                                                                                    |
	|                                                                              *     |
	|                                                                           ***      |
	|                                                                                    |
	|                                                                         **         |
	|                                                                       ***          |
	|                                                                                    |
	|                                                                    ***             |
	|                                                                                    |
	|                                                             *******                |
	|                                                          ****                      |
	|                                                                                    | Sepal.Width
	|                                                   ********                         |
	|                                                                                    |
	|                                              ******                                |
	|                                **************                                      |
	|                                                                                    |
	|                          ******                                                    |
	|                                                                                    |
	|                   ********                                                         |
	|              *****                                                                 |
	|                                                                                    |
	|            ***                                                                     |
	|       *****                                                                        |
	|                                                                                    |
	|      **                                                                            |
	|                                                                                    |
	|    **                                                                              |
	|  **                                                                                |
	|                                                                                    |
	|                                                                                    |
	|  *                                                                                 |
	|____________________________________________________________________________________|
	                                      Index
	$
	$ # Single categorical variable frequency (ie. aggregate by length)
	$
	$ rplot iris.csv Species -al
	Aggregate function is length
	150 data rows plotted
	Species      length
	setosa       50       ############################################################################
	versicolor   50       ############################################################################
	virginica    50       ############################################################################
	$
	$ # Other data formats (eg. semicolon-seperated)
	$
	$ Rscript -e "print(names(airquality)); write.table(airquality, 'airquality.csv', sep=';', row.names=T, col.names=F)"; head -3 airquality.csv
	[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"
	"1";41;190;7.4;67;5;1
	"2";36;118;8;72;5;2
	"3";12;149;12.6;74;5;3
	$
	$ # Specifying seperating-character, no-header row, and fields by column index (eg. Ozone 'V2' and Temp 'V5')
	$
	$ rplot airquality.csv 2 5 -ns ";"
	 ______________________________________________________
	|                                                      |
	|                        *                             |
	|                          *                           |
	|                       *  * *       *                 |
	|                        *    **                       |
	|                      *   **     *   *                |
	|             * *     *  **                            |
	|           *  * *   *  ***       *                    |
	|           * *     **                    *            |
	|    * **********   *                               *  |
	|   *   *      *    **              *                  | V5
	|     ****** * **                                      |
	|   * ***                                              |
	|    **  *   *                                         |
	|    **  * *                                           |
	|   * **** *                                           |
	|    * * ** * *                                        |
	|     * **                                             |
	|    ***                                               |
	|  * *      *                                          |
	|  **  **                                              |
	|______________________________________________________|
	                         V2
	116 data rows plotted. 37 rows with NA values omitted
	       V2               V5
	 Min.   :  1.00   Min.   :57.00
	 1st Qu.: 18.00   1st Qu.:71.00
	 Median : 31.50   Median :79.00
	 Mean   : 42.13   Mean   :77.87
	 3rd Qu.: 63.25   3rd Qu.:85.00
	 Max.   :168.00   Max.   :97.00
	$
	$ # Passing r-plot a bash text object instead of file
	$
	$ rplot "$(cat mtcars.csv)" mpg disp -x
	 ______________________________________________________
	|                                                      |
	|  *                                                   |
	|  *                                                   |
	|          *                                           |
	|                                                      |
	|                    *                                 |
	|                                                      |
	|        * *  *     *                                  |
	|                                                      |
	|            *                                         |
	|           *                                          | disp
	|            * * *                                     |
	|                        *                             |
	|                  *                                   |
	|                                                      |
	|                                                      |
	|                 *  *   *                             |
	|                     *     *   *                      |
	|                        **        *                   |
	|                           *               *          |
	|                                     *     *   *   *  |
	|______________________________________________________|
	                        mpg
	$
