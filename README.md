# Minimum Convex Polygon

MoveApps

Github repository: *github.com/andreakoelzsch/Minimum-Convex-Polygon*


## Description
Calculate the individual MCPs of your Animals' locations and have them plotted on three types of maps. Different additional output files for downloading Map as HTML and PNG, table of the MCPs sizes and saving MCPs shapes as three types; GeoJSON, GeoPackage, kmz.

## Documentation
After down sampling your data to a maximum 5-minute resolution, this App calculates simple Minimum Convex Polygons (MCPs) for each individual of your data set. Note that calculation of MCP is only possible for tracks with at least 5 locations. Individual tracks with less locations are removed for this analysis but are included in the output data set for use in further Apps.
In addition, the user can select the individual for whom the MCP will be calculated and visualized on the map.

To calculate the planar MCP shapes, the dataset is reprojected to an Azimuthal Equidistant (AEQD) coordinate system centered on the spatial extent of the data, using meters as units.

The MCPs for each individual are plotted on an OpenStreetMap as the default basemap, with transparent, individual-specific colors. Below the polygons, the downsampled tracks of individuals (with sufficiently long tracks) are displayed in the same matching colors. Users can switch to Esri (Environmental Systems Research Institute) basemaps if desired.

A csv-file summarizing the area of each MCP is available through the "Download MCP Table (CSV)" button. Users also can save the currently displayed map as an HTML file by pressing the "Save map as HTML" button, or as a PNG image by pressing the "Save map as PNG" button.

The calculated MCP shapes can also be downloaded in various geospatial formats; As a GeoJSON file by clicking "Download MCP as GeoJSON", As a KMZ (compressed KML) file via "Download MCP as KMZ" and As a GeoPackage (GPKG) file using "Download MCP as GPKG".

### Input data
move2 location

### Output data
move2 location

### Artefacts
MCP Table (CSV): downloadable csv-file with Table of all individuals and the sizes of their calculated MCPs. Note that this is done only once for the initial setting of perc. Unit of the area values: km^2.

Map as HTML: downloadable as a .html file, and. Not in output overview, but direct download via button in UI. It is generated using Leaflet and shows MCP area on the map.

Map as PNG: captures the map as an image in .png format. Not in output overview, but direct download via button in UI.

MCP Shapes: Geospatial shape created from the MCP analysis, offered in multiple formats. All of them Not in output overview, but direct download via button in UI.

•	.geojson (GeoJSON): Ideal for web applications and open-source tools.

•	.gpkg (GeoPackage): storing vector and raster geodata in a single SQLite database. supports multiple layers and large datasets

•	.kmz :A zipped version of a KML file (Keyhole Markup Language), displays geographic data in Google Earth and Google Maps.


### Settings
`Percentage of points the MCP should overlap`: Defined percentage of locations that the MCP algorithm shall use for calculating the MCP. We use the mcp() implementation of the adehabitat package, where (100 minus perc percent of the) locations furthest away from the centroid (arithmetric mean of the coordinates for each animal) are removed. Unit: % (range 0-100).

`Select Animal`: The user can select the individual(s) for whom the MCP will be calculated and visualized on the map. By default, all individuals are selected.

After the map is displayed, the user can zoom in and out using the buttons on the map. They can also select the type of basemap to view (OpenStreetMap, Topomap or Aerial).
Additionally, the user can choose whether to display the MCPs, the tracks, or both. By default, both MCPs and tracks are shown.


### Null or error handling:
**Setting `Percentage of points the MCP should overlap`:** The MCP percentage is selected using a slider with a default value of 90%. The slider only allows values from 0 to 100, invalid values like NULL or negative numbers cannot be selected by the user. If the user doesn’t choose anything, then the app will just use 90 as the value.

**Setting `Select Animal`:** By default, all individuals are selected. If no individuals are selected, no data and no map will be displayed.
**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned.
