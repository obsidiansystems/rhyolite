pkgdb=$(ghc-pkg list | head -1)
rev=$(git rev-parse HEAD | cut -c1-8)
tmpdir=$(mktemp -d -t rhyo-docs-$rev-XXXXXX)

function cleanup {      
  rm -rf "$tmpdir"
}

trap cleanup EXIT

standalone-haddock \
  --package-db "$pkgdb" \
  --dist-dir "$tmpdir" \
  -o rhyolite-docs-$rev \
  backend \
  common \
  email \
  frontend \
  notify-listen/notify-listen/ \
  notify-listen/notify-listen-beam/ \
  psql-extras/psql-serializable/ \
  psql-extras/psql-simple-beam/ \
  psql-extras/psql-simple-class/ \
  semimap/ \
  signed-data/signed-data/ \
  signed-data/signed-data-clientsession/ \
  widgets/ \
  account/types \
  account/backend
  # groundhog-legacy/groundhog-legacy \
  # groundhog-legacy/groundhog-legacy-types/ \
  # psql-extras/psql-simple-groundhog/ \


