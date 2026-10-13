# thermae

A static notes site generated from a Roam Research DataScript export with
Clojure, Instaparse, Hiccup, and Stasis.

## Build and deploy Mikaelogy

The private export belongs at `resources/mikaelogy.edn`. It is deliberately
ignored by Git; never commit it or generated HTML containing its contents.

```sh
bin/prepare-fonts
THERMAE_BASE=/mikaelogy THERMAE_OUT=/tmp/mikaelogy \
  /nix/var/nix/profiles/default/bin/nix \
  --extra-experimental-features 'nix-command flakes' \
  shell nixpkgs#leiningen -c lein run
rsync -a --delete /tmp/mikaelogy/ /srv/mikaelogy/
```

`THERMAE_BASE` is the URL prefix (no trailing slash; default empty).
`THERMAE_OUT` is the export directory (default `../meekaale`). Caddy already
serves `/srv/mikaelogy` under `/mikaelogy`; these changes need no Caddy edits.
Use a fresh output directory when replacing the source export, so stale pages
are not retained. Keep generated sites outside the repository.

## Equity webfonts

`bin/prepare-fonts` reads the owner's licensed OTF files from
`~/.local/share/fonts` (or a directory passed as its first argument), converts
six faces to WOFF2, and preserves the fonts' copyright and trademark metadata.
It requires Python with `fonttools` and `brotli`. Alternatively run
`uv run --with fonttools --with brotli bin/prepare-fonts`.
The generated `resources/static/fonts/` directory is ignored by Git. Stasis
exports these files as binary assets. Without them, CSS falls back to Georgia.
Roam page links use the dedicated Equity Caps face after CSS lowercasing,
including bold caps where appropriate; no synthetic small caps are used.

The [MB Type license](https://mbtype.com/license), version 220504, checked
2026-10-13, permits WOFF2 webfonts on up to three websites owned by the license
buyer (paragraph 8). Paragraph 10 permits format conversion with copyright and
trademark notices intact. Paragraph 7 prohibits giving copies to unlicensed
users or putting them in an accessible source repository. These font files
are separately licensed and are **not** covered by this project's AGPL license.
Keep them out of commits. The website allowance is per buyer, across websites;
it is not a separate three-site allowance for each deployment.

## Browser checks

Preview an export at the same base path, for example by placing a `mikaelogy`
symlink in a temporary server root and serving it with `python3 -m http.server`.
Use `wd` to inspect the index, a daily note, a book, and a page with incoming
links at phone widths (320 and 390 CSS pixels) and desktop width (1280).
Wait for `document.fonts.ready`, check that `scrollWidth` does not exceed
`clientWidth`, and inspect screenshots for wrapping and real small caps.

## License

Copyright (C) 2021  Mikael Brockman

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
