#!/bin/bash
 paste -sd+ < input/Day01.txt | sed 's/++/\n/g' | bc | sort | tail -n 3 | paste -sd+ | bc

