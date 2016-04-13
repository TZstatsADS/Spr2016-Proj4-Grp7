import os
from pyspark.mllib.recommendation import ALS

datasets_path = os.path.join('..', 'Scripts')
ratings_file = os.path.join(datasets_path, 'project4', 'final_raw.csv')
rate_rdd = sc.textFile(ratings_file)

rate_rdd_header = rate_rdd.take(1)[0]

rate_data_rdd = rate_rdd.filter(lambda line: line!=rate_rdd_header)\
	.map(lambda line: line.split(",")).map(lambda tokens: (tokens[4],tokens[5],tokens[3])).cache()

rate_data_rdd.first()
rate_data_rdd.take(5)


#####################################cross-validation to find best rank value###########################################
training_RDD, validation_RDD, test_RDD = rate_data_rdd.randomSplit([6, 2, 2], seed=0L)
validation_for_predict_RDD = validation_RDD.map(lambda x: (x[0], x[1]))
test_for_predict_RDD = test_RDD.map(lambda x: (x[0], x[1]))

import math

seed = 5L
iterations = 10
regularization_parameter = 0.1
ranks = [10, 20, 30, 50, 60]
errors = [0, 0, 0, 0, 0]
err = 0
tolerance = 0.02

min_error = float('inf')
best_rank = -1
best_iteration = -1

for rank in ranks:
    model = ALS.train(training_RDD, rank, seed=seed, iterations=iterations,
                      lambda_=regularization_parameter)
    predictions = model.predictAll(validation_for_predict_RDD).map(lambda r: ((r[0], r[1]), r[2]))
    rates_and_preds = validation_RDD.map(lambda r: ((int(r[0]), int(r[1])), float(r[2]))).join(predictions)
    error = math.sqrt(rates_and_preds.map(lambda r: (r[1][0] - r[1][1])**2).mean())
    errors[err] = error
    err += 1
    print 'For rank %s the RMSE is %s' % (rank, error)
    if error < min_error:
        min_error = error
        best_rank = rank
print 'The best model was trained with rank %s' % best_rank

####################################best rank is between 50 and 60#########################################################


################################### Train the final model #################################################################


complete_rate_data_rdd = rate_rdd.filter(lambda line: line!=rate_rdd_header).map(lambda line: line.split(",")).map(lambda tokens: (int(tokens[5]),int(tokens[6]),float(tokens[4]))).cache()
print "There are %s recommendations in the complete dataset" % (complete_rate_data_rdd.count())


movie_file = os.path.join(datasets_path, 'project4', 'final_movie.csv')
complete_movies_raw_data = sc.textFile(movie_file)
complete_movies_raw_data_header = complete_movies_raw_data.take(1)[0]

#### int(tokens[0]),tokens[1],tokens[2] stands for index, product_id, movie_id change it when all names are available
complete_movies_data = complete_movies_raw_data.filter(lambda line: line!=complete_movies_raw_data_header)\
    .map(lambda line: line.split(",")).map(lambda tokens: (int(tokens[0]),tokens[1],tokens[2])).cache()  

complete_movies_titles = complete_movies_data.map(lambda x: (int(x[1]),x[2]))
print "There are %s movies in the complete dataset" % (complete_movies_titles.count())

def get_counts_and_averages(ID_and_ratings_tuple):
    nratings = len(ID_and_ratings_tuple[1])
    return ID_and_ratings_tuple[0], (nratings, float(sum(x for x in ID_and_ratings_tuple[1]))/nratings)

movie_ID_with_ratings_RDD = (complete_rate_data_rdd.map(lambda x: (x[1], x[2])).groupByKey())
movie_ID_with_avg_ratings_RDD = movie_ID_with_ratings_RDD.map(get_counts_and_averages)
movie_rating_counts_RDD = movie_ID_with_avg_ratings_RDD.map(lambda x: (x[0], x[1][0]))

##################################### new user #########################################################################
new_user_ID = 0

# The format of each line is (userID, movieID, rating)
new_user_ratings = [
     (0,2,4),  # B0016OLXN2 The Last Samurai
     (0,10,3), # B004EPYZQ2 Super 8 
     (0,26,3), # B000I9WVZU Josie and the Pussycats
     (0,45,4), # B006RXQ6FM Doubt
     (0,61,4), # B000IMM3XW X-Men
     (0,83,1), # B003Y5PF80 Fort Apache
     (0,99,1), # B000083EDB Session 9
     (0,123,3), # B000053VQE Hellsing
     (0,156,5), # B00005EB0B What Women Want
     (0,189,4) # B001A7X0XG Ultraviolet
    ]
new_user_ratings_RDD = sc.parallelize(new_user_ratings)
print 'New user ratings: %s' % new_user_ratings_RDD.take(10)

##################################### end of new user #########################################################################

################################train model with new user#########################################################################

complete_data_with_new_ratings_RDD = complete_rate_data_rdd.union(new_user_ratings_RDD)

from time import time

t0 = time()
new_ratings_model = ALS.train(complete_data_with_new_ratings_RDD, best_rank, seed=seed, 
                              iterations=iterations, lambda_=regularization_parameter)
tt = time() - t0

print "New model trained in %s seconds" % round(tt,3)
# New model trained in 91.468 seconds

################################ end train model with new user#########################################################################

################################ prediction #######################################################################################

new_user_ratings_ids = map(lambda x: x[1], new_user_ratings) # get just movie IDs

# keep just those not on the ID list 
new_user_unrated_movies_RDD = (complete_movies_data.filter(lambda x: x[0] not in new_user_ratings_ids).map(lambda x: (new_user_ID, x[0])))

# Use the input RDD, new_user_unrated_movies_RDD, with new_ratings_model.predictAll() to predict new ratings for the movies
new_user_recommendations_RDD = new_ratings_model.predictAll(new_user_unrated_movies_RDD)

new_user_recommendations_rating_RDD = new_user_recommendations_RDD.map(lambda x: (x.product, x.rating))
new_user_recommendations_rating_title_and_count_RDD = \
    new_user_recommendations_rating_RDD.join(complete_movies_titles).join(movie_rating_counts_RDD)

new_user_recommendations_rating_title_and_count_RDD.take(5)

new_user_recommendations_rating_title_and_count_RDD = \
    new_user_recommendations_rating_title_and_count_RDD.map(lambda r: (r[1][0][1], r[1][0][0], r[1][1]))

top_movies = new_user_recommendations_rating_title_and_count_RDD.filter(lambda r: r[2]>=25).takeOrdered(25, key=lambda x: -x[1])

print ('TOP recommended movies (with more than 25 reviews):\n%s' %
        '\n'.join(map(str, top_movies)))






ssh-keygen
ssh-copy-id ruixiongshi@dyn-160-39-192-87.dyn.columbia.edu


./sbin/start-master.sh

./sbin/stop-master.sh
./sbin/start-slaves.sh 4 //dyn-160-39-192-204.dyn.columbia.edu:7077


~/spark-1.6.1/bin/spark-submit --master spark://dyn-160-39-192-87.dyn.columbia.edu:7077 --total-executor-cores 20 --executor-memory 10g server.py

curl --data-binary @user_ratings.file http://0.0.0.0:5419/0/ratings

http://0.0.0.0:5424/10/ratings/top/25




