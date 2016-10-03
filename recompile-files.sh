#! /bin/sh

pwd=`pwd`

for file in `find -iname '*.elc'`
do
    cd `dirname "$file"`

    if test $file -ot ${file%c}
    then
        echo "Recompiling ${file%c}"
        emacs --batch --eval '(byte-compile-file "'${file%c}'")'
    fi

    cd $pwd
done
