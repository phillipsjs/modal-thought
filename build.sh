#!/usr/bin/env bash
set -e

pandoc content/*.md \
  --template=templates/article.html \
  --output=index.html \
  --from=markdown+smart \
  --citeproc

echo "Built index.html"
