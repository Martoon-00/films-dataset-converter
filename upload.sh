set -e

./launch.sh
cp movie_metadata.csv.out shared/movie_metadata.csv
gdrive sync upload shared 0B7FvDoY3Iiy1QV95dVpkM3hjdjQ

