#!/bin/awk -f

BEGIN { FS=" => "; print "[" }

{
    split($1, reagents,/, /);
    split($2, product,/ /);
    printf "(\""product[2]"\", ("product[1]", [";
    sep = ", "
    for (i=1; i <= length(reagents) ; ++i) {
        if (i == length(reagents))
            sep = ""
        split(reagents[i], pair, / /)
        printf "(\""pair[2]"\", "pair[1]")"sep;
    }
    print "])),";
}

END { print "]" }
