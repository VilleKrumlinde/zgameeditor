# BitmapCells {#BitmapCells}

Creates a bitmap with "cell"-style pattern. This bitmap generator is implemented as Voronoi diagram with variable number of points, their placement, visual style, metrics for distance, and width of border between segments. 

![Examples of bitmap cells](comp-cell-examples.png)

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?f=7&t=396).

## Properties

@dl

@dt CellStyle
@dd Visual style of cell. 

* Standard
* Nice1
* TG1
* TG2
* TG3
* Werk

@dt PointsPlacement
@dd Placement of points.

* Random
* Honeycomb
* Squares
* Cobblestone

@dt UsedMetrics
@dd Metrics for computing distance in Voronoi diagram.

* Euclidean
* Manhattan
* MaxMin
* Product
* Stripes

@dt RandomSeed
@dd Random seed of point placement for Random and Cobblestone point placement. Changes also used colors for Standard cell style.

@dt BorderPixels
@dd Width of border between segments expressed in pixels.

@dt PointCount
@dd Number of points in Voronoi diagram.

@dlx
