
/*
    Utils

    This module is just a random collection of utility functions that I find myself using quite often.
    I should probabyl put this up on my github and figure out how to do a submodule thing so that I don't have to keep duplicating all these functions.
    
*/


#load "type_info.jai";
#load "arrays.jai";


// ========== General Purpose ==========

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };


