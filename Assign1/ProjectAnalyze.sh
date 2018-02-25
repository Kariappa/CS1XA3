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
echo "All work TODO has been put into TODO.log"

#Quesiton  4
#Checks all Haskell files with errors and puts them into error.log
if [ "$(find -name "*.hs" | wc -l)" -gt 0 ] 
then find -name "*.hs" |xargs ghc -fno-code >> error.log 2>&1
echo "error.log has been created"
else echo "There are no Haskell files" 
fi




#Bonus Feature 1
#Quick Search
#Given file/Diretory name or key words of one will search and fill all matches

echo " Would you like to search for a file/Directory? [Y/N]"
read input1

if [ $input1 == "N" ]
	then echo " Ok, no search will be made"
	else echo "Do you remember the exact name of the file? [Y/N]"
        read input2
		if [ $input2 == "Y" ]
		then echo "Enter file/directory name"
		read input3
		find -iname "$input3"
		echo "Here are the matching files/directories"

		else echo "Type any words you remember from the file/directory name"
		read input4
		find -iname "*$input4*"
		echo "Here are the matching files/directories" 
		fi
fi
 
#Bonus Feature 2"   			
#Given a Directory name will add,commit and push to GitHub with a README file that has the date
echo "Would you like to create a dictionary? [Y/N]"
read answer
if [ $answer == "N" ] 
then echo "Ok no directory will be created"
else echo "What is the name of the directory you would like to create"
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
fi 























