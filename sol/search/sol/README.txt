Programmers: Valerie Aguilar Dellisanti and Catherine Baumgarten

Instructions for use: In order to use our program, a user would need to
upload a wiki file to the repository. Once uploaded, the user would copy
the file path and use it as the first argument in the Index file. The last
three arguments for the Index file are  titles.txt, docs.txt and words.txt.
After putting the arguments, run the Index file to populate three files,
titles.txt, which maps document IDs to document titles, docs.txt, which
stores the rankings computed by PageRank, and words.txt, which stores the
relevance of documents to words. After, in the Query file put the same
arguments except for the file path and depending on the usage of pageRank,
we add --pageRank for a calculation without it. Then, run the search file
and in the terminal input a search term to get the most relevant wiki articles.


Design overview: Our Index file works to populate a series of hashmaps
that track various characteristics of the wikifiles. Most importantly, the
index file allows the program to find the frequency of important words
inside of the wikifile. Our search file takes in a query and handles
calculations for evaluating how relevant articles are to the inputted query.


How the program was tested: In populating our hashmaps within the Index file,
we tested our helper methods AddFunWordtoPage and GetTitle in our
TesterFile.scala. To make sure our index was populating properly, we made a
small wiki file. Based on running our small wiki file through our index, we
were able to confirm our hashmaps were correctly created. To support errors
we confronted, we debugged specific areas of our Index file. In addition,
for PageRank we made sure that our id to Rank hashmap was being filled with
numbers to represent the PageRank. We also used PageRank.wiki to ensure that
our numbers for PageRank were reasonable.

Known bugs: When checking for PageRank, our PageRank for document 100 was
not drastically different from the other page ranks.
