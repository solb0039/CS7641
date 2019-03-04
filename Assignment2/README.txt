Sean Solberg
CS7641 Assignment 2

Instructions for running code.

All code is located on public Github repository https://github.com/solb0039/CS7641/tree/master/Assignment2

-Code uses the ABAGAIL package.  This package must be complied using the Ant compiler.  Instructions were followed according the ABAGAIL package documentation.
-Jython was used to run the python scripts included in the code repository.  For example, to execute the cont_peaks.py file, type the command $ java -jar jython.jar cont_peaks.py
-Code was adopted from the code originally published by Jonathan Tay 
-Data file is included in the repo but can also be downloaded from https://archive.ics.uci.edu/ml/datasets/Adult
-Python 2.7 was used.  A directory of required packages is provided in the requirements.txt file.
-Important files:
1. ParseData.py: Convert the raw txt file into usable form for Python.
2. DataSetPrep.py: Split data set consistently to be used by all algorithms
3. HW1_ANN.py:  Find the optimal ANN model using scikit learn.
4. HW2_GeneticAlgorithm.py:  Genetic algorithm of the adult dataset. Must be run with Jython.
5. HW2_SimulatedAnnealing.py:  Simulated annealing of the adult dataset. Must be run with Jython.
6. HW2_RandomHillClimb.py:  Random hill climbin of the adult dataset. Must be run with Jython.
7. TravelingSalesman.py:  Code to solve the traveling salesman problem. Must be run with Jython.
8. flipflop.py:  Code to solve the flip flop problem.  Must be run with Jython.
9. cont_peaks.py:  Code to solve the continous peaks problem.  Must be run with Jython
