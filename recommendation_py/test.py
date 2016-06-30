#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About: This is a quick-and-dirty set of (visual inspection) tests for the core recommender 
#        functions. It is intended to be run line-by-line to inspect the results one line at a time.
#        This was just for the core code development and not intended outside this context.
#----------------------------------------------------------------------------------------------------

from recommender_functions import *
import pickle
import sys

raw = get_raw_data()
raw.head()
print(list(raw['user'][:10]))

sim_data = compute_similarity_data(raw)
pickle.dump(sim_data, open("./data/simdata.p", "wb" ) )
build_and_store_distance_data()
build_and_store_distance_data('./data/lastfm_sample.csv', './data/smallsimdata.p')


dist_data = read_distance_data()
k = 15
items = ['nirvana', 'pearl jam', 'lenny kravitz']
items = ['metallica', 'aerosmith', 'megadeth', 'van halen']
items = ['eminem', 'wu-tang clan']
items = ['u2', 'journey']
items = ['gwen stefani', 'taylor swift', 'adele', 'cher']
items = ['pink', 'mariah carey', 'whitney houston']
items = ['ludwig van beethoven']
print('Top ' + str(k) + ' Recommendations:\n' + str(get_recommendations(dist_data, items, k)))
print_items(dist_data)

print(sys.argv)