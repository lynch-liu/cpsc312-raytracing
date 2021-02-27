echo "=====START Hackage Installation====="
cabal update
cabal install gloss
cabal install JuicyPixels
cabal install gloss-juicy
echo "=====END Hackage Installation====="
echo "=====START MAIN PROGRAM====="
ghcii.sh main.hs