### Example usage

	curl https://raw.githubusercontent.com/geotheory/r-plot/master/rplot.R > rplot.R

	Rscript -e "Rscript rplot.R -h"

	Rscript -e "write.csv(mtcars, 'cars.csv')"
	head -3 cars.csv

	Rscript rplot.R cars.csv disp hp

	rplot() { Rscript rplot.R $*; }

	rplot cars.csv hp drat -x

	rplot cars.csv 1 11

	rplot cars.csv 1 11 -o

	rplot cars.csv cyl mpg

	rplot cars.csv cyl mpg -Ho

	rplot cars.csv 1 cyl mpg -o | head -15

	rplot cars.csv cyl mpg -Ham

### Output of above

	$ curl https://raw.githubusercontent.com/geotheory/r-plot/master/rplot.R > rplot.R
	  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
	                                 Dload  Upload   Total   Spent    Left  Speed
	100 10558  100 10558    0     0   5538      0  0: 0:--:--:--  5536
	$
	$ Rscript rplot.R -h

	####### RPLOT #######
	This script plots a scatter or hashbar plot of a csv file or string in your console.
	If 2 numeric id_fields are provided a scatterplot will default, else hashbars.
	Required arguments: csv file/string, then column names/indices (values-column last for hashbars)
	NB read.table check.names=T so e.g. numeric colnames prepent 'X' and those with spaces
	have spaces replaced by '.'. Use '-P' to see the colnames that are read in..

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
	-o   Reorder hashbar chart by value
	-H   Override a default scatter plot with hashbar plot
	-S   Override a default hashbar plot with scatter plot (NA values are removed)
	-x   Suppress summary in case of scatter plot
	-r   Scatterplot rows/height (default 20). Requires following value.
	-c   Scatterplot cols/width (default 50). Requires following value.
	-a   Aggregate (default `sum`) hashbar plot data by its categorical variables
	-m   Aggregate by `mean` if -a selected
	-l   Aggregate by `length` (count instances) if -a selected
	-s   sep character (default `,`). requires value e.g. -S ";" / "	" (inc. quotes)
	-p   pch char/str (default `*`). requires value e.g. -p "." (inc. quotes)
	-P   Output raw data.frame to console (for debugging)
	-Q   Output processed data.frame to console (for debugging)

	$
	$ Rscript -e "write.csv(mtcars, 'cars.csv')"
	$ head -3 cars.csv
	"","mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"
	"Mazda RX4",21,6,160,110,3.9,2.62,16.46,0,1,4,4
	"Mazda RX4 Wag",21,6,160,110,3.9,2.875,17.02,0,1,4,4
	$
	$ Rscript rplot.R cars.csv disp hp
	 ______________________________________________________
	|                                                      |
	|                              *                       |
	|                                                      |
	|                                                      |
	|                                                      |
	|                                                      |
	|                                    *                 |
	|                                                      |
	|                                    **                |
	|                                               *      |
	|                                                 * *  | hp
	|                                                      |
	|           *               *         *    *           |
	|                                                      |
	|                              * *                     |
	|                                                      |
	|    *        *                                        |
	|       **   *       *   *                             |
	|      * * *                                           |
	|                                                      |
	|  *        *                                          |
	|______________________________________________________|
	                        disp
	32 data rows plotted
	      disp             hp
	 Min.   : 71.1   Min.   : 52.0
	 1st Qu.:120.8   1st Qu.: 96.5
	 Median :196.3   Median :123.0
	 Mean   :230.7   Mean   :146.7
	 3rd Qu.:326.0   3rd Qu.:180.0
	 Max.   :472.0   Max.   :335.0
	$
	$ rplot() { Rscript rplot.R $*; }
	$
	$ rplot cars.csv hp drat -x
	 ______________________________________________________
	|                                                      |
	|  *                                                   |
	|                                                      |
	|                                                      |
	|                                                      |
	|                                                      |
	|        *                                             |
	|                                                      |
	|    *                                 *               |
	|    *      *                                          |
	|         *    *                                       | drat
	|         *  *                                         |
	|   *     *  *                      *                  |
	|                       *                              |
	|                                                   *  |
	|                                                      |
	|                                *                     |
	|                  *    *           *                  |
	|            *          **     *                       |
	|                            *                         |
	|           *      *                                   |
	|______________________________________________________|
	                         hp
	$
	$ rplot cars.csv 1 11
	32 data rows plotted
	X                     gear
	Mazda RX4             4      #######################################################
	Mazda RX4 Wag         4      #######################################################
	Datsun 710            4      #######################################################
	Hornet 4 Drive        3      #########################################
	Hornet Sportabout     3      #########################################
	Valiant               3      #########################################
	Duster 360            3      #########################################
	Merc 240D             4      #######################################################
	Merc 230              4      #######################################################
	Merc 280              4      #######################################################
	Merc 280C             4      #######################################################
	Merc 450SE            3      #########################################
	Merc 450SL            3      #########################################
	Merc 450SLC           3      #########################################
	Cadillac Fleetwood    3      #########################################
	Lincoln Continental   3      #########################################
	Chrysler Imperial     3      #########################################
	Fiat 128              4      #######################################################
	Honda Civic           4      #######################################################
	Toyota Corolla        4      #######################################################
	Toyota Corona         3      #########################################
	Dodge Challenger      3      #########################################
	AMC Javelin           3      #########################################
	Camaro Z28            3      #########################################
	Pontiac Firebird      3      #########################################
	Fiat X1-9             4      #######################################################
	Porsche 914-2         5      #####################################################################
	Lotus Europa          5      #####################################################################
	Ford Pantera L        5      #####################################################################
	Ferrari Dino          5      #####################################################################
	Maserati Bora         5      #####################################################################
	Volvo 142E            4      #######################################################
	$
	$ rplot cars.csv 1 11 -o
	32 data rows plotted
	X                     gear
	Porsche 914-2         5      #####################################################################
	Lotus Europa          5      #####################################################################
	Ford Pantera L        5      #####################################################################
	Ferrari Dino          5      #####################################################################
	Maserati Bora         5      #####################################################################
	Mazda RX4             4      #######################################################
	Mazda RX4 Wag         4      #######################################################
	Datsun 710            4      #######################################################
	Merc 240D             4      #######################################################
	Merc 230              4      #######################################################
	Merc 280              4      #######################################################
	Merc 280C             4      #######################################################
	Fiat 128              4      #######################################################
	Honda Civic           4      #######################################################
	Toyota Corolla        4      #######################################################
	Fiat X1-9             4      #######################################################
	Volvo 142E            4      #######################################################
	Hornet 4 Drive        3      #########################################
	Hornet Sportabout     3      #########################################
	Valiant               3      #########################################
	Duster 360            3      #########################################
	Merc 450SE            3      #########################################
	Merc 450SL            3      #########################################
	Merc 450SLC           3      #########################################
	Cadillac Fleetwood    3      #########################################
	Lincoln Continental   3      #########################################
	Chrysler Imperial     3      #########################################
	Toyota Corona         3      #########################################
	Dodge Challenger      3      #########################################
	AMC Javelin           3      #########################################
	Camaro Z28            3      #########################################
	Pontiac Firebird      3      #########################################
	$
	$ rplot cars.csv cyl mpg
	 ______________________________________________________
	|                                                      |
	|  *                                                   |
	|                                                      |
	|  *                                                   |
	|  *                                                   |
	|                                                      |
	|                                                      |
	|  *                                                   |
	|  *                                                   |
	|  *                                                   |
	|  *                                                   | mpg
	|                                                      |
	|  *                       *                           |
	|                          *                        *  |
	|                          *                        *  |
	|                          *                        *  |
	|                                                   *  |
	|                                                   *  |
	|                                                   *  |
	|                                                      |
	|                                                   *  |
	|______________________________________________________|
	                        cyl
	32 data rows plotted
	      cyl             mpg
	 Min.   :4.000   Min.   :10.40
	 1st Qu.:4.000   1st Qu.:15.43
	 Median :6.000   Median :19.20
	 Mean   :6.188   Mean   :20.09
	 3rd Qu.:8.000   3rd Qu.:22.80
	 Max.   :8.000   Max.   :33.90
	$
	$ rplot cars.csv cyl mpg -Ho
	32 data rows plotted
	cyl   mpg
	4     33.9   #####################################################################################
	4     32.4   #################################################################################
	4     30.4   ############################################################################
	4     30.4   ############################################################################
	4     27.3   ####################################################################
	4     26     #################################################################
	4     24.4   #############################################################
	4     22.8   #########################################################
	4     22.8   #########################################################
	4     21.5   #####################################################
	6     21.4   #####################################################
	4     21.4   #####################################################
	6     21     ####################################################
	6     21     ####################################################
	6     19.7   #################################################
	6     19.2   ################################################
	8     19.2   ################################################
	8     18.7   ##############################################
	6     18.1   #############################################
	6     17.8   ############################################
	8     17.3   ###########################################
	8     16.4   #########################################
	8     15.8   #######################################
	8     15.5   ######################################
	8     15.2   ######################################
	8     15.2   ######################################
	8     15     #####################################
	8     14.7   ####################################
	8     14.3   ###################################
	8     13.3   #################################
	8     10.4   ##########################
	8     10.4   ##########################
	$
	$ rplot cars.csv 1 cyl mpg -o | head -15
	32 data rows plotted
	X                     cyl   mpg
	Toyota Corolla        4     33.9   ###############################################################
	Fiat 128              4     32.4   ############################################################
	Honda Civic           4     30.4   ########################################################
	Lotus Europa          4     30.4   ########################################################
	Fiat X1-9             4     27.3   ##################################################
	Porsche 914-2         4     26     ################################################
	Merc 240D             4     24.4   #############################################
	Datsun 710            4     22.8   ##########################################
	Merc 230              4     22.8   ##########################################
	Toyota Corona         4     21.5   #######################################
	Hornet 4 Drive        6     21.4   #######################################
	Volvo 142E            4     21.4   #######################################
	Mazda RX4             6     21     #######################################
	Error: ignoring SIGPIPE signal
	Execution halted
	$
	$ rplot cars.csv cyl mpg -Ham
	Aggregate function is mean
	32 data rows plotted
	cyl   mpg
	4     26.66              #########################################################################
	6     19.74              ######################################################
	8     15.1               #########################################
	$
