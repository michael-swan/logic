#!/bin/bash
cd `stack path --project-root`
mkdir deps
cd deps
stack list-dependencies | while read pkg ver; do
    stack unpack $pkg
done
