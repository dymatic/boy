#/bin/bash
ghc --make Main.hs
mv ./Main ./boy
find . -name "*.o" -exec rm -f '{}' +
find . -name "*.hi" -exec rm -f '{}' +

rm *.deb
rm ./DEBIAN/usr/share/doc/boy/README

cp boy ./DEBIAN/usr/bin
cp README ./DEBIAN/usr/share/doc/boy/

fakeroot dpkg -b ./DEBIAN
dpkg-name *.deb

sudo cp ./boy /usr/bin/
git add *
git add -u
