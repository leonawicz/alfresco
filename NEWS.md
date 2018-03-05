# alfresco 0.2.6

* Added ecoregion and fire management zone masking argument to `fmo_ba`. This adds a new dependency on `snappoly`.
* Added accessor function, `fire_perimiters`, to obtain historical Alaska fire perimiter polygon data via AWS.

# alfresco 0.2.5

* Added plot function for zooming in on radial buffer around a point in a raster layer.

# alfresco 0.2.4

* Added additional handling of treatment runs, multiple FMO base map layers.
* Added point fire probability function.
* Updated documentation.

# alfresco 0.2.3

* Updated scripts and functions to allow for treatment subdirectories for JFSP project Alfresco run raw outputs, data extractions and curated data sets.

# alfresco 0.2.2 (Release date: 2018-01-31)

* Updated FMO map generation functions based on new map versions in `snapgrid` package.
* Updated R function for generating slurm scripts for Atlas jobs. Added a `partition` argument.
* Added `ba_fmo` for computing burn area by FMO zone for each year, averaged across replicates.
* Package now depends on raster.
* Updated documentation.

# alfresco 0.2.1 (Release date: 2017-10-02)

* Added `fire_weights` function for fire probability point estimates based on inverse distance weighting.
* Added `fire_probs` function for fire probability point estimates using `fire_weights` output.
* Added `point_probs` for parallelized application of `fire_weights` and `fire_probs` to multiple point locations.
* Added initial vignette for point probability estimation.

# alfresco 0.2.0 (Release date: 2017-09-29)

* Added function for generating ALFRESCO outputs data extraction SLURM scripts leveraging Rmpi on SNAP's Atlas cluster.
* Added function for generating slurm and R scripts for performing distribution estimation on extractions.
* Added functions for extraction and curation (discrete distribution estimation using `rvtable` package).
* Refactored `fmo_cb_reduction` to handle projected data by RCP and GCM.

# alfresco 0.1.1 (Release date: 2017-09-13)

* Added functions for saving fire management options geotiff and png maps.

# alfresco 0.1.0 (Release date: 2017-09-13)

* Added functions for fire events processing, general data extraction, fire management options, and bridging consecutive ALFRESCO model runs.
* Added function for generating bash scripts for data organization.
* Added documentation and web pages.
* Bug fixes.

# alfresco 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
