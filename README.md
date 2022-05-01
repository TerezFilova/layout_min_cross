# layout_min_cross
# principal idea: in qgraph you can modify the repulse.rad ==> lower values than default return more circular layouts
# these might seem nicielier to someone, but if a too low value is chosed, layouts tend to be unclear
# this function returns fruchterman-reingold layout with such a minimal repulse.rad, that has the lowest no. of edge-crossings
#
# function might be usefull for small or poor grapghs, for graphs with too many edges is slow
