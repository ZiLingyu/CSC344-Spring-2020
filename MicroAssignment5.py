import os
import sys


def wc_directory_processing(argument):
    for filename in os.listdir(argument):
        os.system("wc -l " + argument + "/" + filename)


def main():
    argument = sys.argv[1]
    # os.system("wc -l " + argument)
    wc_directory_processing(argument)


main()


