# My School Assignment 4

My first haskell program. Following is the background of the program:

Write a program in Haskell for local famous chefs to rate the Muchelin restaurants service level. The chef judge may add new potential Muchelin award restaurants into the database, grade the
restaurants service level between 0 and 9, and display the details of various restaurant. The site maintains a “database” (a textfile) of many Muchelin award and potential award winning restaurants;
each record of which gives a restaurant an restaurantID, the name of the restaurant, the area this restaurant is operating, the number of stars awarded by Muchelin (0 for those potential ones) and a list
of chefs rating on the restaurant service level. The average of the rated service level for a restaurant gives the restaurant performance. If a restaurant has been added to the website but is there is no
service level rating to that restaurant, its performance is 0. You should begin by developing purely functional code to cover the core functionality, and then add to this the functions/actions that will
comprise the program’s user interface. You may assume that no two restaurant have the same restaurantID – i.e. any restaurant can be uniquely identified by its restaurantID.

**Core Functionality**
The program should include pure (non-I/O) functions that perform the following:
- add a new restaurant to the database
- show all restaurants in the database
- give all restaurants operating in a certain area
- give all restaurants that have a performance of 8 or higher
- give the average performance for the restaurants in a certain area
- give the names of the restaurants a given chef has rated the service level, along with that rating result for each restaurant.
- give the names of the restaurants a given chef has yet to rate the service level, along with that restaurant performance.
- allow a given chef rating to be entered (or updated) for a restaurant he has rated (note that only the latest result from the chef should remain recorded)
