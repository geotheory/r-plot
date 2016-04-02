# A small library for rendering bar or scatter plots
# of csv data to console using ascii characters.
# (call `Rscript rplot.R -h` for instructions..)
# by @geotheory | geotheory.co.uk

# manage input arguments
args_in = commandArgs(trailingOnly=T)

# arguments that require a following value (e.g. "-p '+'")
pars = list(sep=c('-s',','), pch=c('-p','*'), x=c('-c',50), y=c('-r',20))

# split up combined arguments (e.g. '-am' for aggregate by mean)
args = unlist(sapply(args_in, function(a){
    if(substr(a,1,1)=='-') return(paste0('-',strsplit(substr(a,2,100),'')[[1]])) else a
  }), use.names = F)

# update pars argument update received
for(i in 1:length(pars)){
  p = pars[[i]]
  if(p[1] %in% args){
    n = match(p[1], args) + 1
    pars[[i]][2] = args[n]
    if(p[1]=='-s' & args[n]=="\\t") pars[[i]][2]="\t" # string to tab char
    args = args[-n]
  }
}

plot_args = args[substr(args, 1, 1) == '-']
field_args = args[!substr(args, 1, 1) == '-']

if(any(c('-h','--help') %in% plot_args)){
  cat('\n####### RPLOT #######\n')
  cat('This script plots a scatter or hashbar plot of a csv file or string in your console.\n')
  cat('If 2 numeric id_fields are provided a scatterplot will default, else hashbars.\n')
  cat('Required arguments: csv file/string, then column names/indices (values-column last for hashbars)\n')
  cat("NB read.table check.names=T so e.g. numeric colnames prepent 'X' and those with spaces\n")
  cat("have spaces replaced by '.'. Use '-p' to see the colnames that are read in..\n\n")
  cat('USAGE\n')
  cat('Example csv call - scatterplot (by column name):\n')
  cat('    "Rscript rplot.R file.csv num_field1 num_field2"\n')
  cat('Example csv call - scatterplot (by column index):\n')
  cat('    "Rscript rplot.R file.csv col#1 col#3"\n')
  cat('Example csv call - hashbar plot:\n')
  cat('    "Rscript rplot.R file.csv id_field1 id_field2 value_field"\n')
  cat('Example csv text string call:\n')
  cat('    "Rscript rplot.R "$(cat file.csv)" id_field value_field"\n')
  cat('For convenience you can set up an alias in .bash_profile or equivalent, e.g.\n')
  cat('    "rplot() { Rscript /pathto/rplot.R $*; }"\n')
  cat('and call with:\n')
  cat('    "rplot file.csv field1 field2 etc.."\n\n')
  cat('OPTIONS:\n')
  cat('-o   Reorder hashbar chart by value\n')
  cat('-H   Override a default scatter plot with hashbar plot\n')
  cat('-S   Override a default hashbar plot with scatter plot (NA values are removed)\n')
  cat('-x   Suppress summary in case of scatter plot\n')
  cat('-r   Scatterplot rows/height (default 20). Requires following value.\n')
  cat('-c   Scatterplot cols/width (default 50). Requires following value.\n')
  cat('-a   Aggregate (default `sum`) hashbar plot data by its categorical variables\n')
  cat('-m   Aggregate by `mean` if -a selected\n')
  cat('-l   Aggregate by `length` (count instances) if -a selected\n')
  cat('-s   sep character (default `,`). requires value e.g. -S ";" / "\t" (inc. quotes)\n')
  cat('-p   pch char/str (default `*`). requires value e.g. -p "." (inc. quotes)\n')
  cat('-P   Output raw data.frame to console (for debugging)\n')
  cat('-Q   Output processed data.frame to console (for debugging)\n\n')
  quit()
}

# adapted from http://biostatmatt.com/R/scat.R
scat <- function(x, y, cols=50, rows=20, pch="*", xlab="x", ylab="Y") {
  dat = data.frame(x, y); names(dat) = c(xlab, ylab)
  # output processed data.frame to console
  if('-Q' %in% plot_args) print(dat)
  #make an ASCII scatterplot on a rows X cols grid
  #pch is the ASCII character plotted
  #check arguments
  y <- as.numeric(y)
  if(missing(x)) x <- 1:length(y)
  else x <- as.numeric(x)
  if(length(y) != length(x))
    stop("lengths of y and x differ")
  rows <- as.numeric(rows)
  cols <- as.numeric(cols)
  if(rows < 1 || cols < 1)
    stop("rows and cols must be > 1")
  if(nchar(pch)!=1)
    stop("pch must be exactly one character")

  #map the y and x values to rows and cols
  #FIXME values in y or x could be NA or NaN
  #FIXME division by zero when max(y)-min(y) == 0
  #FIXME any better way to do this?
  ymap <- floor((y-min(y))/(max(y)-min(y))*(rows-1))
  xmap <- floor((x-min(x))/(max(x)-min(x))*(cols-1))

  #sort the mapped values so that the are drawn in
  #left-to-right top-to-bottom order, because thats
  #how they will be printed, unique because we can
  #only print one character in a cell
  bitmap <- unique(cbind(ymap,xmap)[order(-ymap, xmap),])

  #initialize row and col positions
  #last plotted character row and column
  row <- rows - 1
  col <- 0
  k = 0
  cat(" ", rep("_", cols+4), "\n|  ", sep="")
  cat(rep(" ", cols), "  |\n|  ", sep="")
  for(bit in 1:nrow(bitmap)) {
    while(bitmap[bit,1] != row) {
      if(cols-col > 0)
        cat(rep(" ", cols-col), sep="")
      cat("  |")
      k = k + 1
      if(k == floor(rows/2)) cat('', ylab)
      cat('\n|  ')
      row <- row - 1
      col <- 0
    }
    if(bitmap[bit,2]-col > 0)
      cat(rep(" ", bitmap[bit,2]-col), sep="")
    cat(pch)
    col <- bitmap[bit, 2] + nchar(pch)
  }
  if(cols-col > 0)
    cat(rep(" ", cols-col), sep="")
  cat("  |\n|", rep("_", cols+4), "|\n", sep="")
  invisible(bitmap)
  xlab_mar = max(0, 1 + cols/2 - (nchar(xlab)/2))
  cat(rep(' ', xlab_mar), xlab, '\n', sep="")
  # add summary unless overriden
  if(!'-x' %in% plot_args) {
    cat(nrows, 'data rows plotted')
    if(nrow(d_orig) > nrows) cat('.', nrow(d_orig) - nrows, 'rows with NA values omitted')
    cat('\n')
    print(summary(dat))
  }
}

# read in data
cons_width = min(100, as.integer(system('tput cols', intern=T)))
txt = field_args[1]
rows = length(strsplit(txt, split='\n')[[1]])

# data from text blob argument or csv file
if(rows == 1) d = read.table(txt, sep=pars$sep[2], stringsAsFactors=F, header=T, row.names=NULL)
if(rows > 1) d = read.table(text=txt, header=T, sep=pars$sep[2], stringsAsFactors=F)
d_orig = d

# output data.frame to console
if('-P' %in% plot_args) print(d)

field_names = field_args[2:(length(field_args))]

# test coercible to numeric
num = function(n) !is.na(suppressWarnings(as.numeric(n)))

# update field names if actually column index numbers
for(i in length(field_names):1){
  f = field_names[i]
  badfield = F
  if(!f %in% names(d)){                          # not a valid col name
    if(num(f)){                                  # is possible number
      if(as.numeric(f) <= ncol(d)){              # is possible col index range
        field_names[i] = names(d)[as.numeric(f)] # change to col name
      } else badfield = T
    } else badfield = T
  }
  if(badfield){
    cat('fieldname not valid name or column index:', f, '\n')
    field_names = field_names[-i]
  }
}

# omit rows with NA in plotting columns
d = na.omit(d[,field_names])
nrows = nrow(d) # to calc NA removals

id_fields = field_names[1:(length(field_names)-1)]
values_field = field_names[length(field_names)]

# scatter plot if 2 fully numeric/NA variables or manually specified
if(length(id_fields) == 1){
  v = d[[id_fields]]
  numvals = suppressWarnings(as.numeric(v[!is.na(v)]))
  all_numeric = all(!is.na(numvals))
  plot_scatter = F
  if(all_numeric & !'-H' %in% plot_args) plot_scatter = T
  if(!all_numeric & '-S' %in% plot_args) plot_scatter = T
  if(plot_scatter){
    dat = na.omit(data.frame(x = suppressWarnings(as.numeric(v)), y = d[[values_field]]))
    scat(dat$x, dat$y, cols=pars$x[2], rows=pars$y[2], pch=pars$pch[2], xlab=id_fields, ylab=values_field)
    quit()
  }
}

# aggregate
if('-a' %in% plot_args){
  fun = 'sum'
  if('-m' %in% plot_args) fun = 'mean'
  if('-l' %in% plot_args) fun = 'length'
  cat('Aggregate function is', fun, '\n')
  if(length(id_fields) > 1) agg_list = as.list(d[,id_fields]) else agg_list = list(d[,id_fields])
  if(fun == 'length'){
    d = aggregate(rep(1,nrow(d)), by=agg_list, FUN=sum, na.rm=T)
  } else{
    d = aggregate(d[[values_field]], by=agg_list, FUN=fun, na.rm=T)
  }
  names(d) = c(id_fields, values_field)
}

# output processed data.frame to console
if('-Q' %in% plot_args) print(d)

# reorder data hashbars
if('-o' %in% plot_args) d = d[order(d[[values_field]], decreasing=T),]

# calculate column widths
field_data = list()
pos_x = 1
for(f in c(id_fields, values_field)){
  n = length(field_data) + 1
  maxlen = min(max(nchar(f), nchar(d[[f]])), 30)
  vals = substr(d[[f]], 1, maxlen)
  padstr = paste0("%-",maxlen,"s")
  if(f == values_field){
    numerics = !is.na(suppressWarnings(as.numeric(vals)))
    vals[numerics] = round(as.numeric(vals[numerics]), 2)
  }
  field_data[n] = list(list(name = sprintf(padstr, substr(f, 1, maxlen)),
                            values = sprintf(padstr, vals),
                            pos_start = pos_x,
                            pos_end = pos_x + maxlen + 2))
  pos_x = pos_x + maxlen + 3
}

plot_width = cons_width - field_data[[length(field_data)]]$pos_end
values = d[[values_field]]

# functions from scales: included in case R library path isn't accessible from a console call
zero_range = function (x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) return(TRUE)
  if (length(x) != 2) stop("x must be length 1 or 2")
  if (any(is.na(x))) return(NA)
  if (x[1] == x[2]) return(TRUE)
  if (all(is.infinite(x))) return(FALSE)
  m <- min(abs(x))
  if (m == 0) return(FALSE)
  abs((x[1] - x[2])/m) < tol
}

rescale = function (x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) {
  if (zero_range(from) || zero_range(to)) return(ifelse(is.na(x), NA, mean(to)))
  (x - from[1])/diff(from) * diff(to) + to[1]
}

# scale and spacing
plot_ind = field_data[[length(field_data)]]$pos_end + 2

# whether scale to zero or negative extreme
if(min(values) < 0 & max(values) > 0){
  ran = range(values)         # scale positive to negative
} else if(min(values) >= 0){
  ran = c(0, max(values))     # all positive, scale to zero
} else{
  values = -values            # all negative, scale to zero
  ran = c(0, max(values))     # still plot hashbars from left axis
}

dif = diff(ran)
fact = (cons_width - plot_ind)/dif
plot_values = as.integer(rescale(values, to = fact * ran))
minvalue = min(values)
spaces = pmax(0, pmin(plot_values - fact * minvalue, fact * -minvalue))
hashes = fact * abs(values)

# print hashbar plot
cat(nrows, 'data rows plotted')
if(nrow(d_orig) > nrows) cat('.', nrow(d_orig) - nrows, 'rows with NA values omitted')
cat('\n'); for(f in field_data) cat(f$name, '  '); cat('\n')

for(i in 1:length(values)){
  # id_fields
  for(f in field_data) cat(f$values[i], '  ')
  # hashes
  cat(rep(' ', spaces[i]), sep='')
  cat(rep('#', hashes[i]), '\n', sep='')
}
