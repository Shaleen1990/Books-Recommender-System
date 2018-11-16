# Books-Recommender-System

## 1. Introduction
## 1.1 Overview (General Area of Research)
With the increase in digital entertainment and content available to consumers at any given point, the need for a robust filtering solution that analyses the users’ profiles and needs, and makes suggestions to them accordingly, becomes paramount. Recommender systems have become increasingly popular in recent years and are utilized in a variety of areas including movies, music, news, books, research articles, search queries, social tags, and products in general.
Recommendation systems are the means to provide the most appropriate suggestions so that the users online can make better decisions from many alternatives available over the Web based on their preferences.
E-commerce seems to be the up and coming market and Recommendation system is one of the stronger tools to increase profit and retaining buyer. This paper presents book recommendation system based on combined features of collaborative filtering and association rule mining.

## 1.2 Research Objective
The objective of our project is to build a Recommender system for Open Books, an online book store operating from various countries across the globe which sell books of various categories, authors and publishers. 
It recommends books to the users based on their preferences, past purchases, ratings or frequently bought together items and thus improve the quality of decisions the consumers make while choosing a book online.

## 1.3 Business Problem
Have you ever wondered which book to read next?
I often have and to me, choosing my next book is a fascinating issue. With rapid increase in the number of books in a library, it takes a lot of time and difficulty in finding an appropriate book for a user.
In general, when a user enters into an online retail store, the system knows nothing about the user preferences. Consequently, the system is unable to present any personalized recommendations to the user. This problem is referred to as the cold-start problem.
Moreover, when we like to shop for a book online, we often get confused with some of the following questions:
•	What should I read next? 
•	Which genre should I prefer?
•	Which author should I preferably go with?

So, it’s understood that user finds it difficult in choosing a book from large volumes of books available online. As a result, retailers end up losing the customers as the users find it difficult to get what they wish. Online Sales depends on how quickly a customer can get to the books of relevance. 

## 2. Recommender Systems
Whenever a customer goes online to look for any information they are bombarded with humongous data on the internet, finding relevant among which becomes a grueling task. 
Recommendation systems is one such tool that help user by providing them with relevant information based on their interests. A user profile is generated on the basis of user rating and navigation history and choices similarity with other users. Recommender System gives a list of recommendations to the user which is an attempt of predicting user’s preferences. A website using a recommendation system can more effectively provide a user with useful and relevant suggestion that could fulfil his current information requirement. As such, these websites have an edge over others in gaining customer loyalty as well as long term partnership as they provide much more personalized options.
Resnik and Varion were the first one to coin the term Recommender system to generalize the concept of collaborative filtering. They implemented the first recommender system using collaborative filtering technique. Since then the term is being used by different researchers and is explained in various ways. The most common technique used for building recommendation system is collaborative filtering, be it user based or item based. There are few other techniques also which are used for making recommendations such as content-based filtering, demographic and knowledge-based technique but they are not so widely applied. In this project we would be using Collaborative filtering and Association Rules mining for the recommender system.

## 2.1 Types of Recommender systems

## Association based Recommendation 

Association Rule Mining, as the name suggests, are simple rules of “If/Then” statements that help in identifying relationships between seemingly independent relational databases or other data +repositories.
Association rule mining is suitable for non-numeric, categorical data and requires just a little bit more than simple counting.
Association rule mining is a procedure which aims to observe frequently occurring patterns, correlations, or associations from datasets found in various kinds of databases such as relational databases, transactional databases, and other forms of repositories.
An association rule has two parts:
•	an antecedent (if) and
•	a consequent (then).
An antecedent is something that’s found in data, and a consequent is an item that is found in combination with the antecedent.

## There are three common ways to measure association:
## Support
This says how popular an itemset is, as measured by the proportion of transactions in which an itemset appears.  The support is simply the number of transactions that include all items in the antecedent and consequent parts of the rule.
Support (Rule) = P(E1 and E2) [Probability of Buying both the Products A and B]
## Confidence
Confidence is the ratio of the number of transactions that include all items in the consequent, as well as the antecedent (the support) to the number of transactions that include all items in the antecedent. 
Confidence = P(E2|E1) [Probability of Buying product B given that product A has already been bought]
## Lift
Lift is one more parameter of interest in the association analysis. Lift is nothing but the ratio of Confidence to Expected Confidence.

## Collaborative
Collaborative filtering basically filters information by using the recommendations of other users. It is based on the idea that people who agreed in their evaluation of certain books in the past are likely to agree again in the future. It captures the users’ interests from their ratings of books they have read. It matches these ratings against the ratings of other users of the same book to find users with similar interests so as to group them as similar users. Here, a user profile is the unit of comparison, which contains the information provided by the user whether in explicit manner or implicit manner. The comparison of user profiles is used in finding the overlap in interests among different users. The profiles with high overlap are then grouped to be used in future recommendations. We can think of it as giving opinions or taking opinions from friends regarding any books which they have used.
In this technique, the user is given recommendation on the basis of similar user profiles which are calculated through various measures. Mostly Pearson correlation coefficient, Cosine similarity measure and Euclidian distance are taken as widely applied similarity measures.

The two main methods are item based and user based.

## User based
User-item filtering calculates the distance between users based on their ratings rather than calculating the distance between items, while coming up with recommendations for a particular user, we consider the users that are closest to them and then suggest books which those users also liked but our user hasn’t read yet. So, if you’ve read and liked a certain kind and number of books, the model can look at other users who liked those same books and recommend one that they also liked but which you might not have read yet.

# Item based
Item based collaborative filtering was originally developed by Amazon and draws inferences about the relationship between different books based on which set of books are/were purchased together. The more often two items appear in the same user history, the “closer” they’re said to be to one another. So, when someone comes and adds a book to their cart, the algorithm will suggest books that are close, over things that aren’t.


# 2.3 	Distance metrics
A metric or distance function is a function that defines the distance between elements of a set as a non-negative real number. If the distance is zero, both elements are equivalent under that specific metric. 
Distance functions thus provide a way to measure how close two items are, where items do not have to be numbers but can also be vectors, matrices or arbitrary objects. 
Distance functions are often used as error or cost functions to be minimized in an optimization problem.
Following are the distance functions on vectors and arrays:

## 1. Cosine-based Similarity
## 2. Correlation-based Similarity
## 3. Jacquard
## 4. Euclidean

# Data Set
You can find the dataset from the given link :

