set -e
set -x

git config --global user.email "leanprover.community@gmail.com"
git config --global user.name "leanprover-community-bot"

git clone "https://github.com/leanprover-community/mathlib4_docs.git" mathlib4_docs

# Workaround for the lake issue
lake new workaround math
cd workaround
echo 'require «doc-gen4» from ".." / "doc-gen4" ' >> lakefile.lean
mkdir -p lake-packages
cd ..

# carry the already built doc-gen4 over
cp -r doc-gen4/lake-packages/* workaround/lake-packages

cd workaround
# doc-gen4 expects to work on github repos with at least one commit
git add .
git commit -m "workaround"
git remote add origin "https://github.com/leanprover/workaround"

lake update
lake exe cache get
lake build Mathlib:docs Std:docs

cd ..
rm -rf mathlib4_docs/docs/
cp -r "workaround/build/doc" mathlib4_docs/docs
mkdir ~/.ssh
echo "$MATHLIB4_DOCS_KEY" > ~/.ssh/id_ed25519
chmod 600 ~/.ssh/id_ed25519
cd mathlib4_docs/docs
git remote set-url origin "git@github.com:leanprover-community/mathlib4_docs.git"
git config user.email "leanprover.community@gmail.com"
git config user.name "leanprover-community-bot"
git add -A .
git checkout --orphan master2
git commit -m "automatic update to mathlib4 $mathlib_short_git_hash using doc-gen4 $DOC_GEN_REF"
git push -f origin HEAD:master

