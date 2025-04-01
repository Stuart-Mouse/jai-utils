
/*
    Utils

    This module is just a random collection of utility functions that I find myself using quite often.
    Many of these are somewhat particular to the sort of dynamic type-info stuff done in gon, dyncall, data packer, and lead sheets.
    I should probably put this up on my github and figure out how to do a submodule thing so that I don't have to keep duplicating all these functions.
*/


#load "type_info.jai";
#load "arrays.jai";


// ========== General Purpose ==========

memzero :: (t: *$T) { memset(t, 0, size_of(T)); };

all_bytes_are_zero :: (any: Any) -> bool {
    return all_bytes_are_zero(any.value_pointer, any.type.runtime_size);
}

// TODO: could be much more efficient
all_bytes_are_zero :: (data: *void, count: int) -> bool {
    begin := data.(*u8);
    end   := begin + count - 1;
    for begin..end  if it.*  return false;
    return true;
}

// restores the value of a variable at this time upon scope exit
// kind of like push_allocator
defer_restore :: (value: *$T) #expand {
    value_before := value.*;
    `defer value.* = value_before;
}
