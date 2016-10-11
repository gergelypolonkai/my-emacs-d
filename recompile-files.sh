#! /bin/sh

cd `dirname $0`
pwd=`pwd`

for file in `find -iname '*.elc'`
do
    elc=`basename "$file"`
    el="${elc%c}"
    dir=`dirname "$file"`

    cd "$dir"

    if test "$elc" -ot "$el"
    then
        echo "Recompiling ${el}"

        emacs --batch --eval '(byte-compile-file "'${el}'")'
    fi

    cd $pwd
done
