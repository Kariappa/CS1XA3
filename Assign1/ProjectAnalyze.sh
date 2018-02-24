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




#Question 2
#Puts uncommited changes in a file changes.log
git diff > changes.log
echo "Uncommited changes have been moved to changes.log"



#Question 3
# Takes all files with #TODO in them and puts them in TODO.log
grep "#TODO" -r --exclude=ProjectAnalyze.sh --exclude=changes.log --exclude=TODO.log  > TODO.log

#Quesiton  4







#Quick Search
#Given file name or key words of a file name will search and fill all matches

echo " Would you like to search for a file? [Y/N]"
read input1
if [ $input1 == "N" ]
echo " Ok, no file will be searched for"
elif [ $input1 == "Y" ]
then echo " Do you know full file name? [Y/N]"
read input2
if [ $input2 == "Y" ] 






#Given a Directory name will add,commit and push to GitHub with a README file that has the date
echo "What is the name of the directory you would like to create"
read name
mkdir $name
touch $name/README 
today=`date +%Y-%m-%d`
echo "This file was created on $today" >>$name/README
git add $name/README
git add $name
git commit "$name" -m "Created $name"
git push
echo "$name has now been pushed"
























