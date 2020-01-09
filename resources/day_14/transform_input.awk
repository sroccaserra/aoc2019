#!/bin/awk -f

BEGIN { FS=" => "; print "{" }

{
    split($2, a,/ /);
    split($1,b,/[, ]/);
    printf a[2]" {:quantity "a[1]" :materials {";
    for (i=length(b) ; i >= 0 ; --i) {
        printf " "b[i];
    }
    print "}}";
}

END { print "}" }
