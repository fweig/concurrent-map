#!/bin/bash
set -euo pipefail

bin="dist/build/benchmark-skiplist/benchmark-skiplist"
n=100000
step=10000

mkdir -p benchmarks

function repeat() {
    base=$1
    action=$2
    target=$3

    counter=$step
    while [ $counter -lt 110000 ]; do
        $base $counter $action $counter >> $target
        let counter=$counter+$step
    done
}


echo "# 0.1 5" > benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.1 5 1" add benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt

echo "pnewlayer - Done"

echo "# 0.2 7" >> benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.2 7 1" add benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt

echo "# 0.3 10" >> benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.3 10 1" add benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt

echo "# 0.4 13" >> benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.4 13 1" add benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt

echo "# 0.5 17" >> benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.5 17 1" add benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt
echo "" >> benchmarks/pnewlayer.txt

echo "# 0.6 23" >> benchmarks/pnewlayer.txt
repeat "$bin skiplist 0.6 23 1" add benchmarks/pnewlayer.txt

echo "pnewlayer - Done"


echo "# 1" > benchmarks/speedup.txt
repeat "$bin skiplist 0.3 10 1" add benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# 2" >> benchmarks/speedup.txt
repeat "$bin skiplist 0.3 10 2" add benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# 4" >> benchmarks/speedup.txt
repeat "$bin skiplist 0.3 10 4" add benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# 8" >> benchmarks/speedup.txt
repeat "$bin skiplist 0.3 10 8" add benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# 16" >> benchmarks/speedup.txt
repeat "$bin skiplist 0.3 10 16" add benchmarks/speedup.txt

echo "speedup - Done"


echo "# Skiplist" > benchmarks/add-random.txt
repeat "$bin skiplist 0.3 10 4" add benchmarks/add-random.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# Binarytree" >> benchmarks/add-random.txt
repeat "$bin binarytree 4" add benchmarks/add-random.txt

echo "random add - Done"


echo "# Skiplist" > benchmarks/sorted-add.txt
repeat "$bin skiplist 0.3 10 4" sorted-add benchmarks/sorted-add.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# Binarytree" >> benchmarks/sorted-add.txt
repeat "$bin binarytree 4" sorted-add benchmarks/sorted-add.txt

echo "sorted add - Done"


echo "# Skiplist" > benchmarks/remove.txt
repeat "$bin skiplist 0.3 10 4" remove benchmarks/remove.txt
echo "" >> benchmarks/speedup.txt
echo "" >> benchmarks/speedup.txt

echo "# Binarytree" >> benchmarks/remove.txt
repeat "$bin binarytree 4" remove benchmarks/remove.txt

echo "remove - Done"


echo "# Skiplist - Scenario 1" > benchmarks/scenarios.txt
$bin skiplist 0.3 10 4 $n mix1 0 >> benchmarks/scenarios.txt
echo "# Binarytree - Scenario 1" >> benchmarks/scenarios.txt
$bin binarytree 4 $n mix1 0.5 >> benchmarks/scenarios.txt
echo "" >> benchmarks/scenarios.txt
echo "" >> benchmarks/scenarios.txt

echo "Scenario 1 - Done"


echo "# Skiplist - Scenario 2" >> benchmarks/scenarios.txt
$bin skiplist 0.3 10 4 $n mix2 1.5 >> benchmarks/scenarios.txt
echo "# Binary Tree - Scenario 2" >> benchmarks/scenarios.txt
$bin binarytree 4 $n mix2 2 >> benchmarks/scenarios.txt
echo "" >> benchmarks/scenarios.txt
echo "" >> benchmarks/scenarios.txt

echo "Scenario 2 - Done"


echo "# Skiplist - Scenario 3" >> benchmarks/scenarios.txt
$bin skiplist 0.3 10 4 $n mix3 3 >> benchmarks/scenarios.txt
echo "# Binary Tree - Scenario 3" >> benchmarks/scenarios.txt
$bin binarytree 4 $n mix3 3.5 >> benchmarks/scenarios.txt

echo "Scenario 3 - Done"
