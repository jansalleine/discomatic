#!/bin/sh
rm -rf *.bin;
rm -rf *.exo;
for i in *.sid; do
    [ -f "$i" ] || break
    FILENAME="${i%%.*}.bin";
    dd if=$i of=$FILENAME bs=1 skip=124;
    EXONAME="${i%%.*}.exo";
    exomizer mem -l none -o $EXONAME $FILENAME;
done
mv -f -v *.exo ../exo/ && rm -rf *.bin;
