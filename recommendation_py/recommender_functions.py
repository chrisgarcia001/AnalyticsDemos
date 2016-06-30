#----------------------------------------------------------------------------------------------------
# Author: cgarcia
# About: This is a demo of item-based recommendation. In this model items are liked by users (as
#        done on Pandora, rather being given ratings from 1-5 as done on Amazon). The input data
#        consists of rows of individual users liking individual items. See demo data for an example.
#   
#        This file contains the core functions - the recommender app uses these to make 
#        recommendations.
#----------------------------------------------------------------------------------------------------

import pandas as pd
import numpy as np
from scipy.spatial.distance import jaccard
import pickle

# Reads the raw data.
def get_raw_data(path='./data/lastfm.csv'):
	return pd.read_csv(path)
	
# This computes the distance data for a given dataframe. The dataframe should be formatted to
# contain rows that consist of an individual user liking an indivdual item (i.e. there is some 
# user and item column). The output is a dict containing the following keys: 	
#   1) item_indices: maps an individual item to a position number
#   2) inverse_item_indices: maps a position number to an item
#   3) item_dist_matrix: the item-item distance matrix
# In addition, the specific user and item columns can be specified, as can the distance function.
def compute_distance_data(df, usercol='user', itemcol='artist', distancef=jaccard):
	users = list(set(list(df[usercol])))
	items = list(set(list(df[itemcol])))
	pairs = set(zip(list(df[usercol]), list(df[itemcol])))
	print('Number of Users: ' + str(len(users)))
	print('Number of Items: ' + str(len(items)))
	user_inds = dict(zip(users, range(len(users))))
	item_inds = dict(zip(items, range(len(items))))
	inv_item_inds = dict(zip(range(len(items)), items))
	user_item_mat = np.zeros((len(user_inds), len(item_inds)))
	print('Building User-Item Matrix...')
	for i in user_inds.keys():
		for j in item_inds.keys():
			if (i, j) in pairs:
				user_item_mat[user_inds[i], item_inds[j]] = 1
	print('Done!')
	print('Building Item distance Matrix...')
	item_dist_mat = np.zeros((len(items), len(items)))
	for i in range(len(items) - 1):
		for j in range(i, len(items)):
			dist = distancef(user_item_mat[:,i], user_item_mat[:,j])
			item_dist_mat[i,j] = dist
			item_dist_mat[j,i] = dist
	print('Done!')
	return {'item_indices':item_inds, 'inverse_item_indices':inv_item_inds, 
			'user_indices':user_inds, 'item_dist_matrix':item_dist_mat}

# Build the distance data and store it for later use.
def build_and_store_distance_data(input_path='./data/lastfm.csv', output_path='./data/distdata.p'):
	dist_data = compute_distance_data(get_raw_data(input_path))
	pickle.dump(dist_data, open(output_path, "wb" ))

# Read in a stored set of distance data (as returned by compute_distance_data) and return it.	
def read_distance_data(path='./data/distdata.p'):
	return pickle.load(open(path, "rb" ))

# A convenience function to see the set of items contained in the dataset.
def print_items(dist_data):
	items = sorted(dist_data['item_indices'].keys())
	print(items)
	
# For a given set of items, make new recommendations of similarly-liked items.
# Recommendations are given in highest-similarity-first order.
def get_recommendations(dist_data, items, k=3):
	item_inds = dist_data['item_indices']
	inv_item_inds = dist_data['inverse_item_indices']
	item_dist_mat = dist_data['item_dist_matrix']
	user_inds = dist_data['user_indices']
	items = filter(lambda y: y in item_inds.keys(), items)
	print('The following items are contained in the data and will be used for recommendations: ' + str(items))
	inds = map(lambda x: item_inds[x], items)
	scores = []
	for ind in inds:
		scores += zip(range(item_dist_mat.shape[1]), list(item_dist_mat[ind,:])) 
	scores = filter(lambda (x,y): not(x in inds), scores)
	scores = sorted(scores, key=lambda x: x[1])
	recommendations = []
	added, i = 0, 0
	while added < k and i < len(scores):  # This avoids duplicate recommendations.
		(item, score) = scores[i]
		if not(item in recommendations):
			recommendations.append(item)
			added += 1
		i += 1
	return map(lambda x: inv_item_inds[x], recommendations)
		