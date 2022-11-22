#!/bin/bash

BINS="/usr/local/bin/"

is_installed(){
	if [ -f "${BINS}lambda" ]; 
	then
		true
	else
		false
	fi	
}

if is_installed;
then
	echo 'Lambda is already installed.'
	echo 'Want to unistall it? y/n'
	read user_input
	if [ $user_input == "y" ];
	then
		echo "Removing..."
		rm "${BINS}lambda"
		echo 'Removed!'
	fi
else
	echo 'Installing...'
	cp lambda $BINS
	echo 'Installed!'
fi
