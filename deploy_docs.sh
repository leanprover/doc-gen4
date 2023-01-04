# Arguments:
# $1 : path to mathlib4 from working directory
# $2 : path to doc-gen4 from working directory
# $3 : whether to deploy

set -e
set -x

cp -r $2/lake-packages/* $1/lake-packages
cd $1
mathlib_short_git_hash="$(git log -1 --pretty=format:%h)"

cd ../$2
DOC_GEN_URL="$GITHUB_SERVER_URL/$GITHUB_REPOSITORY"

cd ../

git clone "https://github.com/leanprover-community/mathlib4_docs.git" mathlib4_docs

# skip if docs for this commit have already been generated
if [ "$(cd mathlib4_docs && git log -1 --pretty=format:%s)" == "automatic update to mathlib4 $mathlib_short_git_hash using doc-gen4 $DOC_GEN_REF" ]; then
  exit 0
fi

# generate the docs
cd $1
sed -i "s|from git \"https://github.com/leanprover/doc-gen4\" @ \"main\"| from \"..\" / \"doc-gen4\" with NameMap.empty|" lakefile.lean
# This can destroy caching properties if std4 or aesop master are out of sync with mathlib4 but
# we need it since lake does not provide the ability to update singular deps
# right now. Once the feature is implemented we can stop doing that.
lake -Kdoc=on update
lake -Kdoc=on build Mathlib:docs Std:docs

if [ "$3" = "true" ]; then
  cd ..
  rm -rf mathlib4_docs/docs/
  cp -r "$1/build/doc" mathlib4_docs/docs
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
fi
