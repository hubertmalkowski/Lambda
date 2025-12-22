#!/bin/bash

echo "================================"
echo "Running Lambda Calculus Examples"
echo "================================"
echo ""

for file in examples/*.lambda; do
    if [ -f "$file" ]; then
        echo "--- $(basename "$file") ---"
        dune exec lambda "$file"
        echo ""
    fi
done

echo "================================"
echo "Running with --lazy flag"
echo "================================"
echo ""

for file in examples/*.lambda; do
    if [ -f "$file" ]; then
        echo "--- $(basename "$file") [LAZY] ---"
        dune exec lambda -- --lazy "$file"
        echo ""
    fi
done
