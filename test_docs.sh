# Arguments:
# $1 : path to std4 from working directory
# $2 : path to doc-gen4 from working directory

set -e
set -x

# carry the already built doc-gen4 over
mkdir -p "$1"/lake-packages
rsync -av --exclude=".*" "$2"/lake-packages/* "$1"/lake-packages

# generate the docs
cd "$1"
sed -i "s|from git \"https://github.com/leanprover/doc-gen4\" @ \"main\"| from \"..\" / \"doc-gen4\" with NameMap.empty|" lakefile.lean

lake -Kdoc=on update
lake -Kdoc=on build Std:docs
