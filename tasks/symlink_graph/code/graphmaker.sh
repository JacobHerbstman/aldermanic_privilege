#!/bin/bash

echo -e 'digraph G {' > ../output/graph.txt

find ../../*/code -maxdepth 1 -name "Makefile" | xargs grep -o -E '\.\./input.*:.*\.\./\.\./.*output' |
sed 's/\.\.\/\.\.\///g' |
sed 's/\/code\/Makefile:\.\.\/input.*:/ ->/' |
sed 's/\/output$//' |
sed 's/ | / /' |
awk -F ' -> ' '{ print "\"" $2 "\"->\"" $1 "\"" }' |
sort | uniq >> ../output/graph.txt

echo '}' >> ../output/graph.txt
