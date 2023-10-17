#!/bin/zsh

# Check if a directory argument is provided
if [ $# -ne 1 ]; then
  echo "Usage: $0 <directory>"
  exit 1
fi

directory="$1"

# Check if the specified directory exists
if [ ! -d "$directory" ]; then
  echo "Error: Directory '$directory' does not exist."
  exit 1
fi

# Iterate through .grc files in the directory and execute the command
for grc_file in "$directory"/*.grc; do
  if [ -f "$grc_file" ]; then
    ./gracec -o "$grc_file"
    exit_code=$?
        
    if [ $exit_code -eq 0 ]; then
      echo "Command './gracec -o \"$grc_file\"' executed successfully (Exit code: $exit_code)"
    else
      echo "Error: './gracec -o \"$grc_file\"' exited with an error (Exit code: $exit_code)"
    fi
    
    echo -e "\n"
  fi
done

echo "Processing complete."

