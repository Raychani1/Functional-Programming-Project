#!/bin/bash

function run_project() {
    stack build
    clear

    read -r -p $'\e[35mFPP λ >\e[0m ' input

    while [ "$input" != "exit()" ]; do
        stack exec -- Functional-Programming-Project-exe "${input}" 
        read -r -p $'\e[35mFPP λ >\e[0m ' input
    done

    clear
}

run_project