#!/bin/bash

# Script to find Livebook notebooks with duplicate slugs
#
# Usage:
#   ./find_duplicate_slugs.sh <directory>
#
# Example:
#   ./find_duplicate_slugs.sh ./lib/livebook/notebook/learn

set -e

# Check arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 <directory>" >&2
    echo "" >&2
    echo "Example:" >&2
    echo "  $0 ./lib/livebook/notebook/learn" >&2
    exit 1
fi

if [ $# -gt 1 ]; then
    echo "Error: Too many arguments" >&2
    echo "Usage: $0 <directory>" >&2
    exit 1
fi

DIRECTORY="$1"

# Validate directory
if [ ! -d "$DIRECTORY" ]; then
    echo "Error: '$DIRECTORY' is not a valid directory" >&2
    exit 1
fi

# Find all .livemd files
mapfile -t NOTEBOOKS < <(find "$DIRECTORY" -name "*.livemd" -type f 2>/dev/null | sort)

if [ ${#NOTEBOOKS[@]} -eq 0 ]; then
    echo "No .livemd files found in '$DIRECTORY'"
    exit 0
fi

echo "Scanning ${#NOTEBOOKS[@]} notebook(s) in '$DIRECTORY'..."
echo ""

# Extract slug from filename (basename without extension, underscores replaced with dashes)
extract_slug() {
    local path="$1"
    basename "$path" .livemd | tr '_' '-'
}

# Extract title from file (first # heading)
extract_title() {
    local path="$1"
    local title
    title=$(grep -m 1 '^# ' "$path" 2>/dev/null | sed 's/^# //' | sed 's/[[:space:]]*$//')
    if [ -z "$title" ]; then
        echo "(no title)"
    else
        echo "$title"
    fi
}

# Build a temporary file with slug, path, and title
TMPFILE=$(mktemp)
trap "rm -f $TMPFILE" EXIT

for notebook in "${NOTEBOOKS[@]}"; do
    slug=$(extract_slug "$notebook")
    title=$(extract_title "$notebook")
    echo -e "${slug}\t${notebook}\t${title}" >> "$TMPFILE"
done

# Find duplicate slugs
DUPLICATE_SLUGS=$(cut -f1 "$TMPFILE" | sort | uniq -d)

if [ -z "$DUPLICATE_SLUGS" ]; then
    echo "No duplicate slugs found."
    exit 0
fi

# Count duplicates
NUM_DUPLICATES=$(echo "$DUPLICATE_SLUGS" | wc -l)
echo "Found $NUM_DUPLICATES slug(s) with duplicates:"
echo ""

# Display duplicates grouped by slug
echo "$DUPLICATE_SLUGS" | while read -r slug; do
    echo "Slug: $slug"
    echo "----------------------------------------"

    grep "^${slug}	" "$TMPFILE" | while IFS=$'\t' read -r _ path title; do
        echo "  File:  $path"
        echo "  Title: $title"
        echo ""
    done
done
