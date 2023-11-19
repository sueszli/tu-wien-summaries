git add .
git commit -m "add files via git lfs"

# install lfs (see: https://git-lfs.com/)
git lfs install

# track files (pdf, html, png)
git lfs track "*.pdf, *.html, *.png"
git add .gitattributes

# add and update
git add .
git commit -m "add files via GIT LFS"

# push (make sure to have enough space: https://github.com/settings/billing)
git lfs push --all origin main
git push
