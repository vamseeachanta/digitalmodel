import argparse


def argumentParseFunction():

    parser = argparse.ArgumentParser(description='Get AssetID')
    parser.add_argument('--dataSource', dest='dataSource', nargs=1,
 	    help='You must pass the --dataSource value')

    args = parser.parse_args()

    return(args)

