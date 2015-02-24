import pandas
import csv
import numpy as np

DIABETES_FILENAME = 'kff_diabetes_2012.csv'
OVERWEIGHT_FILENAME = 'kff_overweight_2012.csv'

def aggregate():
    diabetes = read_kff(DIABETES_FILENAME)
    overweight = read_kff(OVERWEIGHT_FILENAME)
    return pandas.DataFrame({'diagnosed_diabetes': diabetes,
                             'overweight': overweight})

def parse_pcts(series):
    return series.apply(lambda x: float(x.rstrip('%'))/100)

def read_kff(filename):
    """read in a kff obestiy or diabetes file and return the parse percentages in the first col"""
    return parse_pcts(pandas.read_csv(filename, index_col='Location').icol(0))

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--outfile')
    args = parser.parse_args()
    data = aggregate()
    if args.outfile:
        data.to_csv(args.outfile, sep='\t', float_format="%0.3f")
