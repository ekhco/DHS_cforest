# Pseudocode for permutation statistic for frequency fo important variables
# Eric Chow, Manisha Desai, Eran Bendavid
# Nov 6, 2018

For each important variable id by Boruta from any survey (ie("age", "age_cat", "etc" ...)): {
    repeat 10000x {
        1. permute "age" randomly in all country-surveys (~58x2 = 116 country-survey-sexes)
            - the NULL distributions depend on mix of country-surveys ... and conditional on if not available in given surveys
            - ie: if relationship to hh not available in half, NULL distribution accounts for it
        2. rerun analysis and count in how many country-surveys-sexes this variable shows up "important" by Boruta
        ie: freq table
            --------------------------------
            Variable             Freq
            Age                  116 (100%)
            Age at first sex     110 (95%)
            relationship to hh   84  (72)
            --------------------------------
    }
    build a NULL distribution from 10000x repeats: ie: (3,5,1,1,2,0,0,6,1) of 58 ... )
    see where frequency of un-permuted data lies on NULL distribution (ie: if important in 26 of 58 surveys, was the NULL median around 3?) -> p-value
    do some FDR adjustment ... because testing many p-vals for each variable
}
