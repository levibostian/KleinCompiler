git checkout master
git pull
git branch --merged | grep -v "\*" | xargs -n 1 git branch -d

