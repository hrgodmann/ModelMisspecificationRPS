
# BayesianBalaton

## Overview of the Data

This repository contains different data sets, each with two opponents playing rock paper scissors against each other. All data sets are structured similar, with the choice of each players per round:

- Variable 1: Round: 1 to N
- Variable 2: Player_A: 1 (Rock), 2 (Paper), 3 (Scissors)
- Variable 3: Player_B: 1 (Rock), 2 (Paper), 3 (Scissors)

Folders data_g and data_p contain data sets that were simulated with some underlying data generating process. You can try to detect it. Each of these two folders contains 4 files with different sample sizes. When you perform intense MCMC sampling, lower sample sizes reduce the time the models need for the sampling process. The models within a folder are still based on the same data generating process, so this is just for you to choose based on your computational power. However, you could also "model build" using a smaller data set (e.g., the 50 trials), and when the model is developed, you might throw it on a data set with more samples to derive your conclusions.

There is one additional data set in the folder real_data. This is of same structure but contains data from Mona and Chiara playing rock paper scissors against each other. Of course, we do not know the true data generating process here. But if you have a nice model, feel free to try to find out how people actually play RPS.

Finally, there is a data set of rock paper scissors dice playing each other (thrown 250 times by me). If you maybe want to compare random to unrandom data, this could be interesting.


