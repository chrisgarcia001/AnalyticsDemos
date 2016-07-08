#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About: This app is demo of item-based recommendation. In this model items are liked by users (as
#        done on Pandora, rather being given ratings from 1-5 as done on Amazon). The input data
#        consists of rows of individual users liking individual items. See demo data for an example.
#   
#        This assumes the item distance data has already been calculated and has been stored in 
#        the file "./data/distdata.p". If not, use the build_distance_data.py program first.
#
#        To run, do the following at the command line (make sure to put artists in double quotes ""):
#          > python recommender_app.py <num. recommendations> <"artist 1"> ["artist 2" "artist 3" ...]'
#
# Example Usage: > python recommender_app.py 25 "adele" "ludwig van beethoven"
#        This will recommend the 25 items liked most by people who like Adele and Beethoven.
#
# The lastfm.csv file was obtained here: 
#        http://www.biz.uiowa.edu/faculty/jledolter/datamining/datatext.html
#----------------------------------------------------------------------------------------------------

import sys
from recommender_functions import *

def print_usage():
	msg = 'Usage: > python recommender_app.py <num. recommendations> <"artist 1"> ["artist 2" "artist 3" ...]'
	print('\n  ' + msg + '\n')

try:
	if sys.argv[1].lower() in ['-u', '-usage', '--u', '--usage']:
		raise 'print usage'
	dist_data = read_distance_data()
	k = int(sys.argv[1])
	items = sys.argv[2:]
	recommendations = get_recommendations(dist_data, items, k)
	print('\n----------- YOUR RECOMMENDATIONS -----------------------')
	for i in range(1, len(recommendations) + 1):
		print(str(i) + '. ' + recommendations[i - 1])
	print('--------------------------------------------------------\n')
	
except:
	print_usage()
