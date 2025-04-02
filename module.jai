
/*
    Stuart's Utils

    This module is just a random collection of utility functions that I find myself using quite often.
    Many of these are somewhat particular to the sort of dynamic type-info stuff done in gon, dyncall, data packer, and lead sheets.
    I should probably put this up on my github and figure out how to do a submodule thing so that I don't have to keep duplicating all these functions.
*/


#load "any.jai";
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

offset_of :: ($T: Type, ident: Code) -> s64 #expand {
    t: T = ---;
    return cast(*void) (*t.#insert ident) - cast(*void) *t;
}

// ========== Math-ish ==========

// I don't really wanna put too much math stuff in this module since that should probably get its own separate module,
// but I'm including a couple simple things that I could see myself using quite often.

delerp :: (a: float64, b: float64, val: float64) -> float64 {
    high := max(a, b);
    low  := min(a, b);
    return (val - low) / (high - low);
}

modulo :: (n: $T, d: T) -> T #modify { return T.(*Type_Info).type == .INTEGER; } {
    return n % d + ifx n < 0 then d else 0;
}

// copied from Hash_Table module...
next_power_of_two :: inline (x : int) -> int {
    // @Speed: This could certainly be faster.
    // There is probably something in Hacker's Delight for this.
    assert(x != 0);
    p := 1;
    while x > p p += p;
    return p;
}




#scope_module

#import "Reflection"; // for get_array_count_and_data
#import "Basic";

