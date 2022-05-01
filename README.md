# layout_min_cross
Principal idea: in qgraph you can modify the repulse.rad. Lower values than default return more circular layouts. These might seem nicielier to someone, but if a too low value is chosed, layouts tend to be unclear. 
This function returns Fruchterman-Reingold layout with such a minimal repulse.rad, that has the lowest no. of edge-crossings. Function might be usefull for small or poor grapghs. For graphs with too many edges is slow.
