
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

is_valid_index :: inline (array: []$T, index: int) -> bool {
    return index >= 0 && index < array.count;
}

// restores the value of a variable at this time upon scope exit
// kind of like push_allocator
defer_restore :: (value: *$T) #expand {
    value_before := value.*;
    `defer value.* = value_before;
}

offset_of :: inline ($T: Type, $member: string) -> int {
    return #run -> int {
        for type_info(T).members {
            if it.name == member return it.offset_in_bytes;
        }
        assert(false, "Type '%' does not have member '%'", T, member);
        return -1;
    };
}

unreachable :: inline (loc := #caller_location) {
    assert(false, "unreachable at %: line %, column %", loc.fully_pathed_filename, loc.line_number, loc.character_number);
}

// just tprint but with automatic null-termination, and returning a *u8
tprintc :: (format_string: string, args: .. Any) -> *u8 #must {
    builder: String_Builder;
    builder.allocator = temp;

    print_to_builder(*builder, format_string, ..args);
    append(*builder, "\0");

    return builder_to_string(*builder,, temp).data;
} @PrintLike


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

// ========== Flat Pool ==========

// Again, I feel kinda bad putting this in a general Utils module that I'm asking people to download as a dependency...

// is_this_yours for flat pool allocator
is_this_yours :: (pool: *Flat_Pool, memory: *void) -> bool {
    return flat_pool_allocator_proc(.IS_THIS_YOURS, 0, 0, memory, pool).(bool);
}

copy_to_pool_if_needed :: (pool: *Flat_Pool, any: Any) -> Any {
    if is_this_yours(pool, any.value_pointer)  return any;
    
    copy := New_Any(any.type, initialized = false,, get_pool_allocator(pool));
    memcpy(copy.value_pointer, any.value_pointer, any.type.runtime_size);
    return copy;
}

copy_to_pool_if_needed :: (pool: *Flat_Pool, value: *$T) -> *T {
    if is_this_yours(pool, value)  return value;
    
    copy := get(pool, size_of(T)).(*T);
    copy.* = value.*;
    return copy;
}

get_pool_allocator :: (pool: *Flat_Pool) -> Allocator {
    return .{ flat_pool_allocator_proc, pool };
}






#scope_module

#import "Reflection"; // for get_array_count_and_data
#import "Basic";
#import "Flat_Pool";

