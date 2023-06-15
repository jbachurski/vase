dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html && (chromium _coverage/index.html >/dev/null 2>/dev/null &)