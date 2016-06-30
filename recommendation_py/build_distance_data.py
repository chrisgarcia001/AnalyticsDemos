#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About: This app builds the distance data for the lastfm-based music recommender app. It uses
#        the file ./data/lastfm.csv and then creates a new data file containing the item distances 
#        and related metrics and stores it at the following path: ./data/distdata.p. This app should
#        be run before running the recommender app. It takes 5-10 minutes to run. 
# 
# Example Usage: > python ./build_distance_data.py  
# 
# The lastfm.csv file was obtained here: 
#        http://www.biz.uiowa.edu/faculty/jledolter/datamining/datatext.html
#-----------------------------------------------------------------------------------------------------

from recommender_functions import *

build_and_store_distance_data()
