#!/bin/sh -ue

if [ ! -d "packages" ] || [ ! -d ".git" ] ; then
  echo "Please run from a git clone of an opam repository"
  exit 2
fi

OUT="pkg-stats.svg"

DATA=$(mktemp pkg-data.XXXX)
trap "rm -f \"$DATA\"" EXIT

echo "Gathering git data..."
git log --date-order --reverse --format=%ct --name-status --diff-filter=ACD 2>&1 |\
awk -n '/^[0-9]/ { printf "%s\t%s\n",$1,count }
        /^[AC].*\/opam$/ { count++ }
        /^D.*\/opam$/ { count-- }' \
>"$DATA"

echo "Generating plot..."
gnuplot <<EOF
set xdata time
set timefmt "%s"
unset key
set style data lines
set style fill transparent solid 0.4
set grid
set xlabel 'Time'
set ylabel 'Total number of packages'
set term svg size 800,400 font "Arial,10"
set output "$OUT"
plot '$DATA' using 1:2 with filledcurves below x1 lt rgb 'dark-blue' lw 3
EOF

echo "Done, output is in $OUT"
display "$OUT"
