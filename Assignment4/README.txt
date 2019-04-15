CS7641 Assignment 4
Sean Solberg (ssolberg3)
April-14-2019

Entire code is located in public GitHub repo: 
https://github.com/solb0039/CS7641/tree/master/Assignment4

Prior to using, create a Python3.7 Virtual Environment and load necessary packages using $ pip install -r requirements.txt

Important code files are:
easyGW.py : Run this file for easy grid world
hardGW.py : Run this file for hard grid world

Code use BURLAP to solve the Value & Policy iteration problems as well as Q-learning.

Since main driver code is written in Python and BURLAP is Jave, Jython intermediate is needed.

Example of how to run easyGW.py file:
$ java -jar jython-standalone-2.7.0.jar /path/to/folder/Assignment4/src/easyGW.py

Graphs were generated using R ggplot and code is available in file Graphs.R.  Note that output .csv files must be created first by running easyGW.py and hardGW.py before creating plots. 

Code was adopted from public repo available on JonTay GitHub repo.
