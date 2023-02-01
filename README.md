# Minimum Convex Polygon

MoveApps

Github repository: *github.com/andreakoelzsch/Minimum-Convex-Polygon*


## Description
Calculate the individual MCPs of the your Animals' locations and have them plotted on a colourful map. An additional small output file provides the MCP sizes.

## Documentation
After downsampling your data to maximum 5 minute resolution, this App calculates simple Minimum Convex Polygons (MCPs) for each individual of your data set. Note that calcualtion of MCP is only possible for tracks with at least 5 locations. Individual tracks with less locations are removed for this analysis, but are includedin the output data set for use in further Apps.

For calculating the planar MCP shapes, the data set is first transformed to an equal area projection (+proj= +aeqd ...). The percentage of points that the MCP shall overlap can be defined by the user.

The MCPs for each individual are plotted on a OpenStreetMap of the area with transparent colours. Underneath, the downsampled locations (of the individuals with sufficiently long tracks) are added in the same, individual specific colours. A csv-file with the area values of each MCP is added to the output for download. When pressing the button "Save map" the presently calculated map with MCP can be downloaded as .png plot; when pressing hte button "Save MCP as shapefile" a zip file with the shapefile of the MCP will be downloaded and provided in the output overview.

### Input data
moveStack in Movebank format

### Output data
Shiny user interface (UI)

### Artefacts
`MCP_areas.csv`: csv-file with Table of all individuals and the sizes of their calculated MCPs. Note that this is done only once for the initial setting of `perc`. Unit of the area values: km^2.

`MPC_map.png`: png-file of the UI map view. Not in output overview, but direct download via button in UI.

`MCP_shapefile.zip`: zipped shapefile of the MCP layers for upload to a GIS. Not in output overview, but direct download via button in UI.

### Parameters 
`Percentage of points the MCP should overlap`: Defined percentage of locations that the MCP algorithm shall use for calculating the MCP. We use the mcp() implementation of the adehabitat package, where (100 minus `perc` percent of the) locations furthest away from the centroid (arithmetric mean of the coordinates for each animal) are removed. Unit: `%` (range 0-100).

`Choose a margin size`: Edge area of map around bounding box of the locations for better visibility. Unit: degrees. Default is 0.001. Note that this parameter cannot be interactively changed in the UI, but only set in the Settings in the WFI.

### Null or error handling:
**Parameter `Percentage of points the MCP should overlap`:** A default of 95 percent is provided and is retained if the input values is changed to NULL. If numbers above `100` are provided then the mcp function uses 100 percent of all locations. Negative values are not tolerated and will lead to an error.

**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned.
