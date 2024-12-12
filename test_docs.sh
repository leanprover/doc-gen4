# Arguments:
# $1 : path to batteries from working directory

set -e
set -x

# generate the docs
cd "$1"
cat >> lakefile.toml << 'EOL'
[[require]]
name = "«doc-gen4»"
path = "../doc-gen4"
EOL

export LEAN_ABORT_ON_PANIC=1

lake update doc-gen4
lake build Batteries:docs
