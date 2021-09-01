# Minimum Convex Polygon

MoveApps

Github repository: *github.com/andreakoelzsch/Minimum-Convex-Polygon*


## Description
Calculate the individual MCPs of the your Animals' locations and have them plotted on a colourful map. An additional small output file provides the MCP sizes.

## Documentation
After downsampling your data to maximum 5 minute resolution, this App calculates simple Minimum Convex Polygons (MCPs) for each individual of your data set. For calculating the planar shapes, the data set is first transformed to an equal area projection (+proj= +aeqd ...). The percentage of points that the MCP shall overlap can be defined by the user.

The MCPs for each individual are then plotted on a OpenStreetMap of the area with transparent colours. On top, the downsampled locations are added in the same, individual specific colours. Finally, a csv-file with the area values of each MCP is added to the output for download.

### Input data
moveStack in Movebank format

### Output data
Shiny user interface (UI)

### Artefacts
`MCP_areas.csv`: csv-file with Table of all individuals and the sizes of their calculated MCPs. Note that this is done only once for the initial setting of `perc`. Unit of the area values: km^2.
`shapefile_output/`: the four shapefile files of the MCP polygons for input to a GIS software. The files are called `mcp.dbf`, `mcp.prj`, `mcp.shp` and `mcp.shx`. The files are only generated for the initial parameter settings.


### Parameters 
`perc`: Defined percentage of locations that the MCP algorithm shall use for calculating the MCP. We use the mcp() implementation of the adehabitat package, where (100 minus `perc` percent of the) locations furthest away from the centroid (arithmetric mean of the coordinates for each animal) are removed. Unit: `%` (range 0-100).

### Null or error handling:
**Parameter `perc`:** A default of 95 percent is provided and is retained if the input values is changed to NULL. If numbers above `100` are provided then the mcp function uses 100 percent of all locations. Negative values are not tolerated and will lead to an error.

**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned.