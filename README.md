Repository Structure

    [data] ---> used to save datasets
        raw_datasets
        processed_datasets

    [doc] ---> used to save and share ideas, diagrams, information even questions.

    [code] ---> used to save codes (R)
        build ---> to build the project (not sure yet)
        src ---> to put codes, other files necessary to run the project

    [temp] ---> used to save templates

Basic Git Commands That You Might Need

    git clone <remote_URL> --> To create local copy of an existing remote repository, here you could do this 'git clone https://github.com/YUECHEN0830/DSP_Data_Geek_Solar_Energy.git'
    git pull --> To get the latest version of a repository
    git status --> To check the current state of the local repository compared with remote one
    git add <file or directory name> --> To add a specific file or folder (including files) to the staging area for git
    git add . --> To add all file
    git commit -m <commit message in quotes> --> Record the changes made to files to a local repository (Note: please add a message within the command and let us know what you have done in this submission !)
    git checkout <file_path_in_proj> --> To overwrite the file from remote repository
    git rm -f <file_name> --> To delete a file
    git reset <file or directory name> --> To remove a file from the staging area
    git push --> To send local commits to the remote repository (Note: please make sure not submit files that would make the whole project breakdown !!!)
    git log --> To show the commit logs (e.g. commit 41972ebd3041447fe43877573XXXXXXXXXXXX)
    git reset --hard <commit version> --> To revert to a specific version (need use git push -f -u origin <branch name> to push codes back to Github)

Git Commands Related to Branch Management


Basic branching:

    git checkout -b <branch_name> --> To create a new branch and switch to it at the same time
    this is shorthand for:

    git branch <new_branch_name> --> To create a new branch

    git checkout <new_branch_name> --> To switch to the new branch

    git branch --> To list branches

    git checkout <branch_name> --> To switch branch

    git branch -d <branch_name> --> To delete branch

    git push origin <branch_name> --> To push a branch to the remote repository -- here is 'origin'; the command always been used when you want to create a new branch

    git push -u origin <branch_name> --> Using "-u" option for upstream

Merge:

    git merge <branch_name>
    git merge –-no-ff <branch_name> --> To merge <branch_name> (this) to your current branch
    Undo a merge:
    git reset --hard <commit-before-merge> --> == the hash of commit
    or
    git reset --hard HEAD~1
    Undo a pushed merge:
    git revert -m 1 <merge-commit-hash> --> == the hash of commit

Note: If you need any other extra help related to git, you could check these websites:

    https://git-scm.com/docs
    http://guides.beanstalkapp.com/version-control/common-git-commands.html
    https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging https://uoftcoders.github.io/studyGroup/lessons/git/branches/lesson/ https://stackoverflow.com/questions/11722533/rollback-a-git-merge/29110174 https://git-scm.com/docs/git-merge https://segmentfault.com/q/1010000000140446 (Chinese)

    If you want to create a new branch or modify an old one, please check doc/Branches We Have.md first !!!

How to upload large file (>100MB) to Github

    download and install LFS

    brew install git-lfs --> Homebrew
    git lfs install --> set up LFS

    tell LFS which file you want to mark it as 'a large file'

    git lfs track "<file_name>"

    add '.gitattributes' and commit it first

    git add .gitattributes
    git commit -m "modify .gitattributes for lfs"
    git push

    add large files and commit them

    git add <files>
    git commit -m "<message>"
    git push

Note:

    Highly recommand upload '.gitattributes' first, then upload large files. Otherwise, you might get trouble in 'git push' !!!!
    If you get trouble with 'git commit' and want to withdraw that action, please type in 'git reset --soft HEAD~1' or 'git reset --soft HEAD^'. If you need more help you can check this website: https://git-scm.com/docs/git-reset
    If you need any other extra help related to lfs, you could check these websites:

    https://git-lfs.github.com/
    
    

# Esther's Additions 
  
### Create a Commit with Multiple Authors

- Helps with collaboration work and giving credit! :clap:
  
- You may also receive a GitHub achievement for co-authoring a couple of times! :trophy:


#### How to Add Extra Authors 
- Type your commit message and a short, meaningful description of your changes. 
- After your commit description, instead of a closing quotation, add two empty lines.
- On the next line of the commit message, type Co-authored-by: name <name@example.com> with specific information for each co-author. After the co-author information, add a closing quotation mark.

e.g.
```

$ git commit -m "Refactor usability tests.
>
>
Co-authored-by: name <name@example.com>
Co-authored-by: another-name <another-name@example.com>"


```

[Additional Docs for Multiple Authors](https://docs.github.com/en/pull-requests/committing-changes-to-your-project/creating-and-editing-commits/creating-a-commit-with-multiple-authors)


  
## Guide to help with writing clean code

Try your best to follow some of the tips in this [Tidyverse Style Guide](https://style.tidyverse.org/). Clean code shows you care about your team mates who will have to either spend a lot of time understanding what the hell is going on or cleaning it up! It will also help you remember   :sparkles:

<center>


<img src="https://preview.redd.it/yxams8xqlr141.jpg?auto=webp&s=bbe0abcd909c74890f57131e72186a47ce1a8f15" width="400">




# Important Git Rules


:warning: Do not **ever** push on master/main. :warning:

I repeat

:warning: Do not **EVER** push on master/main. :warning:

</center>


# Practice Repository

[Practice using GitHub using this practice repo](https://github.com/esthercsoke/practice-repo)
