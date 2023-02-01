# inquisitive 0.1.0

* Plot_missingness_by_var now aggregates data if more than 5,000 rows are present and will show percentage of missing data in the plot.
* Updated previous fix and returned to using geom_raster for plot_missingness_by_var

# inquisitive 0.0.5

* Fixed bug where geom_raster is unable to handle more than 32,667 rows on some systems.

# inquisitive 0.0.4

* Moved the reason label from the body of the columns removed graph to a secondary x-asis to improve visibility of missing and present in the stacked chart.

# inquisitive 0.0.3

* Updateds to adjudicate_suspect_data and plot_suspect_data documentation

# inquisitive 0.0.2

* Bug fixes and appeasements to R CMD check

# inquisitive 0.0.1

* Added plot_suspect_data functions included adjudication helper function.

# inquisitive 0.0.0.9001

* Beginning consolidation of EDA tools from multiple sources
