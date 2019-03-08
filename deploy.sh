set -x

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout master

# Build new files
stack exec site rebuild

# Get previous files
git fetch --all
git checkout -b built-site --track origin/built-site

# Overwrite existing files with new files
cp -a _site/. .

# Commit
git add -A
git commit -m "Build"

# Push
git push origin built-site:built-site

# Restore previous state
git checkout master
git branch -D built-site
git stash pop
