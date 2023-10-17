#!/bin/bash

# Check if arguments are provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <file1> <file2> ..."
    exit 1
fi

# Check for the existence of the 'gracec' executable in the same directory as the script
if [ ! -x "$(dirname "$0")/gracec" ]; then
    read -p "The 'gracec' executable does not exist. Build it from source? [Y/n] " choice
    if [ "$choice" == "Y" ] || [ "$choice" == "y" ] || [ "$choice" == "" ]; then
        if [ -f "$(dirname "$0")/Makefile" ]; then
            make -C "$(dirname "$0")"
        else
            echo "Makefile not found. Cannot build 'gracec'. Exiting."
            exit 1
        fi
    else
        exit 1
    fi
fi

# Loop through each argument
for arg in "$@"; do
    # Check if the argument is a file that exists
    if [ -f "$arg" ]; then
        # Execute gracec to compile the Grace program
        "$(dirname "$0")/gracec" -of < "$arg" > temp.asm
        
        # Compile the generated assembly code with clang
        /usr/lib/llvm15/bin/clang -o a.out temp.asm libgrc.a -no-pie
        
        # Run the executable
        ./a.out
        
        # Clean up by deleting temporary files
        rm -f temp.asm a.out
    else
        echo "File not found: $arg. Skipping."
    fi
done

