This file is presented as a JSON array, it contains the labeled results for each system. The results contain:  
1) Average DCG of this system;
2) Average precision of this system;
3) Details of the result. 100 query and their results returned by this system are recorded. For each query, the following contents are recorded:
   a) The query string in LaTeX;
   b) DCG of this query;
   c) "Active" is an attribute for our labelling system. Please ignore it;
   d) Precision of this query;
   e) The top 10 results in order returned by this system. (Note: There will be less than 10 results for some queries, if this system returns less than 10 results in total.)

====Definition of score====
score = {1, 2, 3, 4, 5}
1: No relevant to the query;
2-5: The higher score denotes the more relevant to the query.