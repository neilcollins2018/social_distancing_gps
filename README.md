# Social Distancing with GPS
R script and sample data for investigating distancing between individuals

Note this sample data is concerning taxis travelling in a city however an identical process can be (and has been) used with athlete gps data. The use of taxi data is to overcome any data sharing issues (it is avaialble here https://www.octranspo.com/developers)

Script performs three main functions.

Reading in data.

Aligning data so lengths and time variables match.

Produce final distance matrix
Lines 117 & 118 set the threshold for the distance. It is currently set at 1000m due to the use of the taxi data, alter to desired value. 
Distance matrix formula sourced here: https://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r/
