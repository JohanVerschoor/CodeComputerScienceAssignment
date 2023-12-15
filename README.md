What is this project about?

This project is about finding similar products amongst webshops. The past years, there has been a steady rise in the amount of webshops. 
It is hard to manually compare all these products, as there is a vast amount of them and the product descriptions usually differ accross webshops. 
Therefore, there are plenty of algorithms that aim to detect duplicate products, but by computing a minimal amount of comparisons across products. T
his increases the efficiency, which is necessary, as comparing all products would be computationally too demanding. 
One of the advanced algorithms that is able to detect duplicate products amongst multiple webshops is the Multi-component Similarity Method with Preselection (MSMP), proposed by Bezu et al.
Their paper can be used as a reference:
van Dam, I., van Ginkel, G., Kuipers, W., Nijenhuis, N., Vandic, D., Frasincar, F.:
Duplicate detection in web shops using LSH to reduce the number of computations.
In: 31th ACM Symposium on of Applied Computing (SAC 2016). pp. 772–779.
ACM (2016)

This code tries to improve MSMP. This has been done before, compare for example the proposed MSMP+ method, which can also be used as a reference:
Hartveld, A., van Keulen, M., Mathol, D., van Noort, T., Plaatsman, T., Frasincar, F., Schouten, K.: 
An LSH-based model-words-driven product duplicate detection method, 
In: 30th International Conference on Advanced Information Systems Engineering (CAiSE 2018). 
Lecture Notes in Computer Science, vol. 10816, pp. 149-161. Springer (2018)

The model I proposed is called the Multi-component Similarity Method with Specific Preselection, which differs from MSMP by selecting model words by using a different definition. 
This improves an often used evaluation measure of the model, namely F1*. 
The model is implemented on a large dataset containing 1624 TV's, of which their are 399 duplicate pairs. I aim to implement 3 different definitions of model words. 
These are used to perform pre-selection on possible product duplicates, which is the first step of implementing MSMP.
Then, the products are clustered using MSM. The LSH performance is evaluated by bootstrapping the data using 63% of the products. Later on, the MSMSP performance is evaluated as well.

So this project aims to improve the performance of MSMP, with the aim to find duplicates amongst multiple webshops in a computationally less demanding way.

Structure of the code
To discuss the structure of the code, I will group lines that are meant for specific purposes. In general, lines 1-258 are used to perform and evaluate Locality Sensitve Hashing (LSH). 
Lines 260-601 are used to perform and evaluate MSMP and MSMSP.

LSH
1-13: Load all necessary packages, load the data, abstract the ModelIDs that uniquely identify the TVs.
14-38: Clean and structure the data
39-65: Create an overview of al 1624, and download a list of all TV brands.
66-79: Extract model words, choose the definition, create a binary matrix for LSH
80-92: Set bootstrapping variables, create dataframes to store the evaluation statistics
93-118: Start of the bootstrapping, create a signature matrix
119-136: Place products in buckets
137-154: Extract candidate pairs
155-170: Create a matrix which represents candidate pairs in a boolean way
171-192: Obtain the amount of true positives
193-212: Create and store the evaluation measures, namely Pair Quality, Pair Completeness and F1
213-223: Find the optimal F1 score
224-239: Average the bootstrap results
240-258: Performing the above code for every model word setting gives three lists of evaluation methods
Placing these in Excel files and importing them, run the code to create graphs comparing the three methods.

MSM
259-271: Set the hyperparameters of MSM
272-275: Create a list of webshops
276-290: Function to calculate the q-gram similarity for 2 strings
291-295 Function that checks if 2 TVs come from the same shop
296-307: Function that checks if TVs have the same brand
308-314: Function to obtain the key-value pairs of a TV
315-330: Function to obtain model words. Again, three different possibilities are accessed
331-340: Function that gives the percentage of matching model words from two sets of model words
341-358: Function that calculates the TMWM similarity between two TVs
359-372: Function that returns the brand if it can be found in the title
373-432: Using all the above mentioned functions, MSMP is performed
433-437: Create a list of clusters
438-456: Function that checks if a potential new cluster contains at least one combination of TVs with infinite distance
457-462: Function to calculate the dissimilarity between two clusters using single linkage
463-492: Main loop that uses above mentioned functions to perform hierarchical clustering
493-498: Print the final clusters
499-542: Print the true clusters based on the ModelIDs of the TVs
543-583: Calculate and print evaluation measures
584-589: Function to calculate the F1 measure
590-601: Print evaluation measures, namely Pair Quality, Pair Completeness and the F1 measure

How to use the code
The code can be used by importing the data file in line 10.
In line 70-72 one of the three title model word definitions must be selected.
In line 83, the amount of bootstraps can be set.
In line 84, the bands which you wish to evaluate can be set. NB: n=r*b, keep this in mind when selecting the bands 
In line 106, the amount of LSH permuations can be set. 
In line 107, a seed can be set

Running lines 1-239 gives output for a certain definition of title model words, which is stored in average_results. 
Export this data to Excel, and perform these lines again for all model word definitions.
Than structure the data such that the first column contains the fraction of comparisons, and the 2nd till 4th column respectively contain the F1 measure, PQ and PC.
Make graphs using lines 246-258.

To run MSM, first the LSH model should be computed again, but for the entire dataset.
In lines 263-267, set hyperparparameters for MSM. They are now set to the optimal values given by Bezu et al.

van Bezu, R., Borst, S., Rijkse, R., Verhagen, J., Frasincar, F., Vandic, D.: 
Multicomponent similarity method for web product duplicate detection. 
In: 30th Symposium on Applied Computing (SAC 2015). pp. 761–768. ACM (2015)

In lines 324-326, select the definition of the title model words.

Use the printed evaluation methods the evaluate the definition of the title model words.


