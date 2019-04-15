#! /usr/bin/env jython
"""
RHC NN training on Segmentation data
"""
from __future__ import with_statement


from java.lang import System
import os
import csv
import time
import sys
#sys.path.append(".\ABAGAIL\ABAGAIL.jar")
sys.path.append('/Users/Sean/School/GeorgiaTech/CS7641/Assignment2/ABAGAIL/ABAGAIL.jar')
from func.nn.backprop import BackPropagationNetworkFactory
from shared import SumOfSquaresError, DataSet, Instance
from opt.example import NeuralNetworkOptimizationProblem
from func.nn.backprop import RPROPUpdateRule, BatchBackPropagationTrainer
import opt.RandomizedHillClimbing as RandomizedHillClimbing

from func.nn.activation import LogisticSigmoid

# Network parameters found "optimal" in Assignment 1
INPUT_LAYER = 56
HIDDEN_LAYER1 = 56
HIDDEN_LAYER2 = 56
HIDDEN_LAYER3 = 56
OUTPUT_LAYER = 1
TRAINING_ITERATIONS = 100001
TRIALS = 11
ERROR_SAMPLE_INTERVAL = 100
HALT_COUNT_MAX = 5000
HALT_COUNT_THRESHOLD = .000001
OUTFILE = "/Users/Sean/School/GeorgiaTech/CS7641/Assignment2/NN_OUTPUT/RHC_LOG_56.txt"



def initialize_instances(infile):
    """Read the m_trg.csv CSV data into a list of instances."""
    instances = []

    # Read in the CSV file
    with open(infile, "r") as dat:
        reader = csv.reader(dat)

        for row in reader:
            instance = Instance([float(value) for value in row[:-OUTPUT_LAYER]])
            instance.setLabel(Instance([float(value) for value in row[-OUTPUT_LAYER:]]))
            instances.append(instance)

    return instances


def errorOnDataSet(network, ds, measure):
    N = len(ds)
    error = 0.
    correct = 0
    incorrect = 0
    for instance in ds:
        network.setInputValues(instance.getData())
        network.run()
        actual = instance.getLabel().getData().argMax()
        predicted = network.getOutputValues().argMax()
        if actual == predicted:
            correct += 1
        else:
            incorrect += 1
        output = instance.getLabel()
        output_values = network.getOutputValues()
        example = Instance(output_values, Instance(output_values))
        error += measure.value(output, example)
    MSE = error / float(N)
    acc = correct / float(correct + incorrect)
    return MSE, acc


def train(oa, network, oaName, training_ints, validation_ints, testing_ints, measure):
    """Train a given network on a set of instances.
    """
    print "\nError results for %s\n---------------------------" % (oaName,)
    times = [0]
    halt_count = 0
    lastScore = 0
    iterations = 0
    for iteration in xrange(TRAINING_ITERATIONS):
        total_iter = iteration
        start = time.clock()
        score = oa.train()
        elapsed = time.clock() - start
        times.append(times[-1] + elapsed)
        if score - lastScore < HALT_COUNT_THRESHOLD:
            halt_count += 1
            if halt_count >= HALT_COUNT_MAX:
                iterations = iteration
                break
        else:
            halt_count = 0
        if score > lastScore:
            lastScore = score
        if iteration % ERROR_SAMPLE_INTERVAL == 0:
            current_weights = oa.getOptimal()
            network.setWeights(current_weights.getData())
            MSE_trg, acc_trg = errorOnDataSet(network, training_ints, measure)
            MSE_val, acc_val = errorOnDataSet(network, validation_ints, measure)
            MSE_tst, acc_tst = errorOnDataSet(network, testing_ints, measure)
            txt = '%s,%s,%s,%s,%s,%s,%s,%s\n'%(iteration, MSE_trg, MSE_val, MSE_tst, acc_trg, acc_val, acc_tst,
                                                     times[-1]);
            print txt
            with open(OUTFILE, 'a+') as f:
                f.write(txt)
    optimal_instance = oa.getOptimal()
    network.setWeights(optimal_instance.getData())
    MSE_trg, acc_trg = errorOnDataSet(network, training_ints, measure)
    MSE_val, acc_val = errorOnDataSet(network, validation_ints, measure)
    MSE_tst, acc_tst = errorOnDataSet(network, testing_ints, measure)
    txt = '%s,%s,%s,%s,%s,%s,%s,%s,%s\n'%(iterations, MSE_trg, MSE_val, MSE_tst, acc_trg, acc_val, acc_tst,times[-1], 'optimal')
    print txt
    with open(OUTFILE, 'a+') as f:
        f.write(txt)


def main():
    """Run this experiment"""
    training_ints = initialize_instances('/Users/Sean/School/GeorgiaTech/CS7641/Assignment2/s_trg.csv')
    testing_ints = initialize_instances('/Users/Sean/School/GeorgiaTech/CS7641/Assignment2/s_test.csv')
    validation_ints = initialize_instances('/Users/Sean/School/GeorgiaTech/CS7641/Assignment2/s_val.csv')
    factory = BackPropagationNetworkFactory()
    measure = SumOfSquaresError()
    data_set = DataSet(training_ints)
    sig = LogisticSigmoid()
    rule = RPROPUpdateRule()
    oa_names = ["RHC"]
    classification_network = factory.createClassificationNetwork([INPUT_LAYER, HIDDEN_LAYER1,HIDDEN_LAYER2,HIDDEN_LAYER3, OUTPUT_LAYER],sig)
    for trial in xrange(TRIALS):
        oa = RandomizedHillClimbing(NeuralNetworkOptimizationProblem(data_set, classification_network, measure))
        train(oa, classification_network, 'RHC', training_ints,validation_ints,testing_ints, measure)


if __name__ == "__main__":
    with open(OUTFILE,'w') as f:
        f.write('%s,%s,%s,%s,%s,%s,%s,%s\n' % ('iteration','MSE_trg','MSE_val','MSE_tst','acc_trg','acc_val','acc_tst','elapsed'))
    main()