#!/bin/bash

echo -e "\033[0;32mBuilding website...\033[0m"

# Ensure we start from a clean state
rm -rf target/website

# Copy the entire website directory to target/website
cp -r website/ target/

# Generate the compiler API docs
cargo doc --no-deps --workspace

# Move the API docs into the compiler-docs subdirectory within the website
mv target/doc target/website/compiler-docs

# Generate the Fe guide into the docs subdirectory within the website
$(cd docs && mdbook build -d ../target/website/docs)

if [[ "$*" == --serve ]]
then
    echo -e "\033[0;32mServing website locally...\033[0m"
    cd target/website
    python3 -m http.server 8000
else
    echo -e "\033[0;32mDeploying website...\033[0m"
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

fi
