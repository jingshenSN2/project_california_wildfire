#!/bin/bash

# This script is used to pack the files into a zip file.

# Allow user to select: (1) for Gao (2) for Lei
echo "Please select the name: (1) for Gao (2) for Lei"
read name

# If 1, name will be gao_jingsong, 2 will be lei_haiwen
if [ $name -eq 1 ]; then
    name="gao_jingsong"
elif [ $name -eq 2 ]; then
    name="lei_haiwen"
else
    echo "Invalid input"
    exit 1
fi

# Create a zip file with the name
git archive --format zip --output gis_project_$name.zip HEAD

echo "Saved as gis_project_$name.zip"
