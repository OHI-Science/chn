# Instructions for updating the content of your WebApp's pages.

# 1. install dependencies
for (p in c('git2r')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

# 2. setup
wd = '~/github/chn'
repo = git2r::repository(wd)

# 3. switch to gh-pages branch
git2r::checkout(repo, 'gh-pages')
git2r::fetch(repo, 'origin')

# 4. You are now in the gh-pages branch, which displays information on the WebApp.
# Edit text for the following pages:
  # homepage: ~/github/chn/index.md
  # regions:  ~/github/chn/regions/index.md
  # layers:   ~/github/chn/layers/index.md
       # edit the header text only. To edit layer descriptions, modify layers.csv in the draft branch.
  # goals:    ~/github/chn/goals/index.md
       # edit equations in LaTex. Use current equations as a template
       # and learn more syntax at https://en.wikibooks.org/wiki/LaTeX/Mathematics
  # scores:   ~/github/chn/scores/index.md
       # edit the header text only. Calculated scores will be updated automatically.

# 5. commit and push normally from gh-pages

# 6. switch back to draft branch
git2r::checkout(repo, 'draft')
git2r::fetch(repo, 'origin')

