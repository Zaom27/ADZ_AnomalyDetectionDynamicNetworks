===================================================

All experimental data were collected by Danielle Mersch, contact danielle.mersch@gmail.com

Files: 

* behavior.csv 
contains for each ant a row with behavioural information such as the social group (nurse = N, cleaner = C, forager = F), number of visits to the brood, rubbish pile, nest entrance, foraging area, number of interactions with workers from other groups
for info: body size is in pixels with 1 pixel = 0.06mm 

* information_spread.txt
contains the ID of the ant used as seed for the information spread, the time and number of ants informed in each social group

* observed_expected_frequencies.csv
This file contains for each ant the observed and expected number of interactions with individuals of the three social groups. Expected frequencies are calculated with two different methods: random and space as described in the supplementary information of the paper.

* 246 files with matrices of the social network (network_col*_day*.txt). There is one file per colony per day.
In each file the network is represented as a square matrix, but only column names are given (row names are symmetrical).

* 18 files with the spatial distribution of workers (heatmap_col*_groupname.txt).There is one file per social group per colony. The x_coor and y_coor is the corresponding index in the heatmap grid. The maximum number of visits is already converted to values between 0 and 255.