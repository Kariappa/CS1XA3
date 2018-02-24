#!/bin/bash


#Question 1 
#Checks if you're local repo is up to date with the remote repo

git remote update
if [ "$(git status | grep -c "up-to-date")" -eq 1 ] 
then echo "Your repository is up to date"

else echo "It seems your repository is not up to date. Would you like to update it [Y/N]"
read input

if [ $input == "Y" ]
then git pull
echo "Your repository has now been updated"

elif [ $input == "N" ]
then  echo "Your respository has not been updated"
fi
fi
