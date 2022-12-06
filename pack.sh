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

# Allow user to select: (1) for whole project (2) for shiny app only
echo "Please select the directory to pack (1) whole project (2) shiny"
read pack_dir

# If 1, name will be gao_jingsong, 2 will be lei_haiwen
if [ $pack_dir -eq 1 ]; then
    # Create a zip file with the name
    git archive --format zip --output gis_project_$name.zip HEAD
    echo "Saved as gis_project_$name.zip"
elif [ $pack_dir -eq 2 ]; then
    # Pack only shiny app
    git archive --format zip \
      --output gis_project_shiny_app_$name.zip HEAD:output/calfire_app
    echo "Saved as gis_project_shiny_app_$name.zip"
else
    echo "Invalid input"
    exit 1
fi
