#!/bin/bash

echo -e "\033[0;32mDeploying website...\033[0m"

# Ensure we start from a clean state
rm -rf target/website

# Copy the entire website directory to target/website
cp -r website/ target/

# Generate the compiler API docs
cargo doc --no-deps --workspace

# Move the API docs into a directory called "compiler-docs" within the website
mv target/doc target/website/compiler-docs

# delete old gh-pages-tmp branch
git branch -D gh-pages-tmp

# Recreate it
git checkout -b gh-pages-tmp

# Add changes to git.
git add -f target/website

# Commit changes.
msg="Generated site on `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

git subtree split -P target/website -b gh-pages-dist

# Push to Github Pages
git push -f upstream gh-pages-dist:gh-pages

# Go where we came from
git checkout -

# Delete temporary branches
git branch -D gh-pages-tmp
git branch -D gh-pages-dist
