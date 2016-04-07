# A small library for rendering bar or scatterplots of csv data to console using ascii characters.
# Call `Rscript rplot.R -h` for instructions..
# by @geotheory | geotheory.co.uk 2016

# manage input arguments
args_in = commandArgs(trailingOnly=T)

# arguments that require a following value (e.g. "-p '+'")
pars = list(sep=c('-s',','), pch=c('-p','*'), x=c('-c',50), y=c('-r',20))

# split up combined arguments (e.g. '-am' for aggregate by mean)
args = unlist(sapply(args_in, function(a) {
    if(substr(a,1,1)=='-') return(paste0('-',strsplit(substr(a,2,100),'')[[1]])) else a
  }), use.names = F)

# update pars argument update received
for(i in 1:length(pars)) {
  p = pars[[i]]
  if(p[1] %in% args) {
    n = match(p[1], args) + 1
    if(p[1] == '-s') {
      if(is.na(args[n])) args[n] = ""       # whitespace seperator
      if(args[n] == "\\t") args[n] = "\t"   # string to tab char
    }
    pars[[i]][2] = args[n]
    args = args[-n]
  }
}

plot_args = args[substr(args, 1, 1) == '-']
field_args = args[!substr(args, 1, 1) == '-']

if(any(c('-h','--help') %in% args_in | '-h' %in% plot_args)) {
  cat('\n**********************\n')
  cat('******* R-PLOT *******\n')
  cat('**********************\n\n')
  cat('This library plots a scatterplot or hashbar plot (bars made of hashes!) of a csv or a similarly formatted\n')
  cat('file or string in your console. If 2 numeric id_fields are provided a scatterplot will default, else hashbars.\n')
  cat('Required arguments: csv file/string, then column name(s)/index(ices) (values-column last for hashbars)\n\n')
  cat("NB read.table check.names=T so e.g. numeric colnames prepend 'X' and those with spaces have spaces replaced by '.'.\n")
  cat("Use '-Pz | head' to suppress the plot and see the colnames that are read in..\n\n")
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
  cat('    "rplot() { Rscript /pathto/rplot.R "$1" ${*:2}; }"\n')
  cat('and call with:\n')
  cat('    "rplot file.csv field1 field2 etc.."\n\n')
  cat('OPTIONS:\n')
  cat('  Data handling:\n')
  cat('    -n   Specify no header row for input data. Use col indices instead\n')
  cat('    -s   sep character for input data (default `,`). Requires value e.g. ";"  "\\t"  "" (inc. quotes)\n')
  cat('    -a   Aggregate (default `sum`) a hashbar plot data by its categorical variables\n')
  cat('    -m   Aggregate by `mean` if `-a` selected\n')
  cat('    -l   Aggregate by `length` (count instances) if `-a` selected\n')
  cat('  Plotting:\n')
  cat('    -o   Reorder hashbar chart by value (also reorders data.frames)\n')
  cat('    -H   Override a default scatterplot with hashbar plot\n')
  cat('    -S   Override a default hashbar plot with scatterplot (NA values are removed)\n')
  cat('    -r   Scatterplot rows/height (default 20). Requires following value.\n')
  cat('    -c   Scatterplot cols/width (default 50). Requires following value.\n')
  cat('    -p   pch char (defaults: `#` hashbars, `*` scatterplots without overplotting,\n')
  cat('         `. : ■ █` scatterplots with o/p). Requires 1 char eg. `-p "."` (eg. with -y)\n')
  cat('         or a 4 char string eg. ".°*@" to change overplot symbols (inc. quotes)\n\n')
  cat('    -R   Add r2 correlation (bivariate only))\n')
  cat('    -x   Suppress summary in case of scatterplot\n')
  cat('    -y   Suppress scatterplot point symbols (that show overplotting)\n')
  cat('    -z   Suppress plot (eg. use with -P or -Q)\n')
  cat('  Other:\n')
  cat('    -h   Call this help (also --help)\n')
  cat('    -P   Output raw data.frame to console (truncated 1000 rows)\n')
  cat('    -Q   Output processed data.frame to console (truncated 1000 rows)\n\n')
  quit()
}

# 2 functions from {scales}: included as R's library path sometimes isn't accessible from a console call
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

rescale = function (x, to=c(0,1), from, finite=T) {
  if(missing(from)) from = range(x, na.rm=T)
  if(zero_range(from) || zero_range(to)) return(ifelse(is.na(x), NA, mean(to)))
  (x - from[1])/diff(from) * diff(to) + to[1]
}

map = function(x, n) floor(rescale(x, to=c(1,n)))

scatter_plot = function(x, y, cols=50, rows=20, pch="*", xlab="x", ylab="Y") {
  y0 = y
  if('-o' %in% plot_args) y = sort(as.numeric(y))
  if(xlab == ylab) xlab = "Index"
  if(missing(x)) x <- 1:length(y)
  else x <- as.numeric(x)
  symbs = c('.', ':', '■', '█')
  if(nchar(pch)==4) symbs = strsplit(pch, '')[[1]]
  if(nchar(pch)!=1 & nchar(pch)!=4) stop("pch must be 1 or 4 characters long")

  # output processed data.frame to console
  orig_dat = data.frame(x, y, stringsAsFactors=F)
  names(orig_dat) = c(xlab, ylab)
  if('-Q' %in% plot_args) print(head(orig_dat,1000))
  if('-z' %in% plot_args) quit()
  
  # rescale to grid and count point overplotting
  summary = as.data.frame(table(paste(map(x,cols), map(-y,rows))), stringsAsFactors=F)  # summarise
  summary = data.frame(apply(cbind(do.call('rbind', strsplit(summary[[1]], split=' ')), summary$Freq),2,as.numeric)) # parse
  names(summary) = c('x','y','freq')
  op = max(summary$freq)
  pr_labs = F; labs = NULL
  
  if(op > 1 & !'-y' %in% plot_args) {  # overplotting and not manually over-riden   
    if(nchar(pch)==1 & '-p' %in% plot_args) warning('Single character argument for -p is ignored when point overplotting is present except when -y selected.')
    # cluster overplots to map to symbols
    summary$grp = summary$freq
    if(length(unique(summary$freq)) > 4) {
      f = summary$grp != 1   # ignore 1s (over-plotting)
      centres = unique(as.numeric(quantile(summary$grp[f], 0:2/2)))
      summary$grp[f] = kmeans(summary$grp[f], centres)$cluster + 1
    } else{
      summary$grp = as.numeric(factor(summary$freq))
    }
    
    # symbol labels
    if(length(unique(summary$freq))) {
      pr_labs = T
      # data break points
      op_data = unique(summary[ order(summary$freq), 3:4 ])
      op_data_rev = op_data[order(op_data$freq, decreasing=T), ]
      n = length(unique(op_data$grp))
      labs = data.frame(p0 = 1:n, p1 = 1:n, lab = '', x=0, stringsAsFactors=F)
      for(g in unique(op_data$grp)) {
        labs$p0[g] = op_data$freq[match(g, op_data$grp)]          # first match in group
        labs$p1[g] = op_data_rev$freq[match(g, op_data_rev$grp)]  # last match in group
        labs$x[g] = mean(summary$freq[summary$grp == g])          # mean of grp frequencies
      }
      for(i in 1:nrow(labs)) {
        labs$lab[i] = ifelse(labs$p0[i] == labs$p1[i], labs$p0[i], paste0(labs$p0[i], '-', labs$p1[i]))
        labs$lab[i] = paste0(symbs[i], '  ', labs$lab[i], '  (x̄ ', format(round(labs$x[i],1),nsmall=1), ')')
      }
      labs = c('Points', labs$lab)
    }
  } else { # only 2 types of 
    symbs = pch   
    summary$grp = 1
  }

  # output scatterplot
  l = rep(' ', cols)
  cat(' ', rep('_',cols), ' \n', sep='')
  k = 0
  for(i in 1:rows) {
    dat = subset(summary, y == i)
    ln = l
    ln[dat$x] = symbs[dat$grp]
    cat('|', ln, '| ',  sep='')
    if(pr_labs & i <= length(labs)) cat(' ', labs[i], sep='')  # point symbol key
    k = k + 1
    if(k == ceiling(rows/2)) cat('', ylab)                     # y label
    if(k == rows) if('-R' %in% plot_args & xlab!='Index') cat(' R²=', round(cor(x,y0),3), sep='')
    cat('\n')
  }
  cat('|', rep('_',cols), '|\n', sep='')
  xlab_mar = max(0, 1 + cols/2 - (nchar(xlab)/2))
  cat(rep(' ', xlab_mar), xlab, '\n', sep="")                  # x label

  # add summary unless overriden
  if(!'-x' %in% plot_args) {
    cat(nrows, 'data rows plotted')
    if(nrow(d_orig) > nrows) cat('.', nrow(d_orig) - nrows, 'rows with NA values omitted')
    cat('\n')
    print(summary(orig_dat))
  }
}

# read in data
cons_width = min(100, as.integer(system('tput cols', intern=T)))
txt = field_args[1]
rows = length(strsplit(txt, split='\n')[[1]])

# data from text blob argument or csv file
if('-n' %in% plot_args) header = F else header = T
if(rows == 1) d = read.table(txt, sep=pars$sep[2], stringsAsFactors=F, header=header, row.names=NULL)
if(rows > 1) d = read.table(text=txt, header=header, sep=pars$sep[2], stringsAsFactors=F)
d_orig = d

# output data.frame to console
if('-P' %in% plot_args) print(head(d, 1000))

field_names = field_args[2:(length(field_args))]

# test coercible to numeric
num = function(n) !is.na(suppressWarnings(as.numeric(n)))

# interpret field names - check if valid as name or column index
for(i in length(field_names):1) {
  f = field_names[i]
  badfield = F
  if(!f %in% names(d)) {                          # not a valid col name
    if(num(f)) {                                  # is possible number
      if(as.numeric(f) <= ncol(d)) {              # is possible col index range
        field_names[i] = names(d)[as.numeric(f)] # change to col name
      } else badfield = T
    } else badfield = T
  }
  if(badfield) {
    cat('fieldname not valid name or column index:', f, '\n')
    field_names = field_names[-i]
  }
}

id_fields = field_names[1:(length(field_names)-1)]
values_field = field_names[length(field_names)]

# omit rows with NA in plotting columns
d = na.omit(d[,c(id_fields, values_field), drop=F])
nrows = nrow(d) # to calc NA removals

# scatterplot if 2 fully numeric/NA variables or manually specified
if(length(id_fields) == 1) {
  v = d[[id_fields]]
  numvals = suppressWarnings(as.numeric(v[!is.na(v)]))
  all_numeric = all(!is.na(numvals))
  plot_scatter = F
  if(all_numeric & !'-H' %in% plot_args) plot_scatter = T
  if(!all_numeric & '-S' %in% plot_args) plot_scatter = T
  if(plot_scatter) {
    if(values_field == id_fields) {    # ie. only a single field supplied
      dat = na.omit(data.frame(x = 1:length(v), y = d[[values_field]], stringsAsFactors=F))
    } else dat = na.omit(data.frame(x = suppressWarnings(as.numeric(v)), y = d[[values_field]], stringsAsFactors=F))
    scatter_plot(dat$x, dat$y, cols=as.numeric(pars$x[2]), rows=as.numeric(pars$y[2]), pch=pars$pch[2], xlab=id_fields, ylab=values_field)
    quit()
  }
}
# aggregate
if('-a' %in% plot_args) {
  fun = 'sum'
  if('-m' %in% plot_args) fun = 'mean'
  if('-l' %in% plot_args) fun = 'length'
  cat('Aggregate function is', fun, '\n')
  if(length(id_fields) > 1) agg_list = as.list(d[,id_fields]) else agg_list = list(d[,id_fields])
  if(fun == 'length') {
    d = aggregate(rep(1,nrow(d)), by=agg_list, FUN=sum, na.rm=T, simplify=T)
  } else d = aggregate(d[[values_field]], by=agg_list, FUN=fun, na.rm=T, simplify=T)
  if(id_fields == values_field) values_field = fun # ie. 'length'
} else{
  if(length(unique(c(id_fields, values_field))) == 1) {
    d[[id_fields]] = 1:nrow(d)
    id_fields = 'Index'
  }
}

# rename fields if they've changed
names(d) = c(id_fields, values_field)

# reorder data hashbars
if('-o' %in% plot_args) d = d[order(d[[values_field]], decreasing=T),]

# output processed data.frame to console
if('-Q' %in% plot_args) print(head(d,1000))

# calculate column widths
field_data = list()
pos_x = 1
for(f in c(id_fields, values_field)) {
  n = length(field_data) + 1
  maxlen = min(max(nchar(f), nchar(d[[f]])), 30)
  vals = substr(d[[f]], 1, maxlen)
  padstr = paste0("%-", maxlen, "s")
  if(f == values_field) {
    numerics = !is.na(suppressWarnings(as.numeric(vals)))
    vals[numerics] = round(as.numeric(vals[numerics]), 2)
  }
  field_data[n] = list(list(name = sprintf(padstr, substr(f, 1, maxlen)), values = sprintf(padstr, vals),
                            pos_start = pos_x, pos_end = pos_x + maxlen + 2))
  pos_x = pos_x + maxlen + 3
}

plot_width = cons_width - field_data[[length(field_data)]]$pos_end
values = d[[values_field]]

# scale and spacing
plot_ind = field_data[[ length(field_data) ]]$pos_end + 2

# whether scale to zero or positive/negative extreme
if(min(values) < 0 & max(values) > 0) {
  ran = range(values)         # scale positive to negative
} else if(min(values) >= 0) {
  ran = c(0, max(values))     # all positive, scale to zero
} else{
  #values = -values           # all negative, scale to zero
  ran = c(min(values), 0)     # still plot hashbars from left axis
}

fact = (cons_width - plot_ind) / diff(ran)
#plot_values = as.integer(rescale(values, to = fact * ran)) # old
plot_values = as.integer(values * fact)
minvalue = min(values)
spaces = pmax(0, pmin(plot_values - fact * minvalue, fact * -minvalue))
hashes = fact * abs(values)

# print hashbar plot
cat(nrows, 'data rows plotted')
if('-z' %in% plot_args) quit()
if('-p' %in% plot_args) pch = pars$pch[2] else pch = '#'
if(nrow(d_orig) > nrows) cat('.', nrow(d_orig) - nrows, 'rows with NA values omitted')
cat('\n'); for(f in field_data) cat(f$name, '  '); cat('\n')

for(i in 1:length(values)) {
  # id_fields
  for(f in field_data) cat(f$values[i], '  ')
  # hashes
  cat(rep(' ', spaces[i]), sep='')
  cat(rep(pch, hashes[i]), '\n', sep='')
}

