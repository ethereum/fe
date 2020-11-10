#!/bin/bash

echo -e "\033[0;32mDeploying website...\033[0m"

# Ensure we start from a clean state
rm -rf target/doc

cargo doc --no-deps --all

# delete old gh-pages-tmp branch
git branch -D gh-pages-tmp

# Recreate it
git checkout -b gh-pages-tmp

# Rustdoc generates the index.html in a child folder and leaves it up to us to how
# to get there. For now, we just use this client side redirect.
cat > target/doc/index.html <<EOF
<!DOCTYPE html>
<html lang="en">
<head>
<link rel="canonical" href="fe/index.html"/>
<noscript>
	<meta http-equiv="refresh" content="0;URL=fe/index.html">
</noscript>
<!--[if lt IE 9]><script type="text/javascript">var IE_fix=true;</script><![endif]-->
<script type="text/javascript">
	var url = "fe/index.html";
	if(typeof IE_fix != "undefined") // IE8 and lower fix to pass the http referer
	{
		document.write("redirecting...");
		var referLink = document.createElement("a");
		referLink.href = url;
		document.body.appendChild(referLink);
		referLink.click();
	}
	else { window.location.replace(url); } // All other browsers
</script>
</head>
</html>
EOF

cat > target/doc/CNAME <<EOF
fe.ethereum.org
EOF

# Add changes to git.
git add -f target/doc

# Commit changes.
msg="Generated site on `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

git subtree split -P target/doc -b gh-pages-dist

# Push to Github Pages
git push -f upstream gh-pages-dist:gh-pages

# Go where we came from
git checkout -

# Delete temporary branches
git branch -D gh-pages-tmp
git branch -D gh-pages-dist
