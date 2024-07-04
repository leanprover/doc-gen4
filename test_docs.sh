# Arguments:
# $1 : path to batteries from working directory
# $2 : path to doc-gen4 from working directory

set -e
set -x

# generate the docs
cd "$1"
echo 'require «doc-gen4» from ".." / "doc-gen4"' >> lakefile.lean

lake update doc-gen4
lake build Batteries:docs
