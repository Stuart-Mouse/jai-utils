/*
    This is a modified version of the standard Fixed_Bit_Array which uses a fixed size instead of performing a dynamic allocation for the array slots.
    
    TODO: we could also make the slot size a parameter, so that we can tune it for a particular use case
*/

Fixed_Bit_Array :: struct(count: int) {
    #assert(count > 0);
    slots: [#run max(count >> 6, 1)] s64;
}

set_bit :: (using array: *Fixed_Bit_Array, i: int) {
    assert(i < count);
    slots[i >> 6] |= (1 << (i & 63));    
}

clear_bit :: (using array: *Fixed_Bit_Array, i: int) {
    assert(i < count);
    slots[i >> 6] &= ~(1 << (i & 63));
}

toggle_bit :: (using array: *Fixed_Bit_Array, i: int) {
    assert(i < count);
    slots[i >> 6] ^= (1 << (i & 63));
}

set_bit_to :: (using array: *Fixed_Bit_Array, i: int, value: bool) {
    assert(i < count);
    slots[i >> 6] = set_bits(slots[i >> 6], 1 << (i & 63), value);
    assert(array.*[i] == value);
}

operator [] :: (a: Fixed_Bit_Array, i: int) -> bool {
    assert(i < a.count);
    return cast(bool) (a.slots[i >> 6] & (1 << (i & 63)));
}

operator []= :: inline (a: *Fixed_Bit_Array, i: int, value: bool) {
    inline set_bit_to(a, i, value);
}


clear_all_bits :: (using array: *Fixed_Bit_Array) {
    memset(slots.data, 0, slots.count * size_of(s64));
}

set_all_bits :: (using array: *Fixed_Bit_Array) {
    memset(slots.data, 0xFF, slots.count * size_of(s64));
}

toggle_all_bits :: (using array: *Fixed_Bit_Array) {
    for * slots { it.* ^= 0xFFFFFFFF_FFFFFFFF; }
}



// for_expansion generates 'it' as a boolean, true or false for each bit.
// A little more code, a little easier to understand?

for_expansion :: (array: *Fixed_Bit_Array, body: Code, flags: For_Flags) #expand {
    #assert(flags == 0);  // No options are supported.

    // Avoid variable-shift in order to be fast. Just left-shift by 1 each time.

    for slot, slot_index: array.slots {
        base_index := slot_index * 64;
        limit      := Basic.min(array.count - base_index - 1, 63);  // Don't go past the number of bits that are actually stored.

        bit := 1;
        for i: 0..limit {
            `it := (slot & bit) != 0;
            bit <<= 1;
            `it_index := base_index + i;

            #insert (remove=#assert(false), break=break slot) body;
        }            
    }
}

/*
// Modern Programmer Version that even supports reversal.
// It would be interesting to see how good the codegen ends up being here.

for_expansion :: (array: Fixed_Bit_Array, body: Code, flags: For_Flags) #expand {
    #assert(!(flags & .POINTER));
    
    for <=cast(bool)(flags & .REVERSE) `it_index: 0..array.count-1 {
        `it := inline array[it_index];
        #insert (remove=#assert(false)) body;
    }
}
*/

// Iterate only bits that are set, or only bits that are unset.
only_set   :: #bake_arguments only_set_or_unset(target_value = true);
only_unset :: #bake_arguments only_set_or_unset(target_value = false);

#scope_file

Basic :: #import "Basic";
assert :: Basic.assert;

only_set_or_unset :: (array: *Fixed_Bit_Array, body: Code, flags: For_Flags, target_value: bool) #expand {
    #assert(!flags);  // No options are supported.

    `it := target_value; // 'it' will just be true, or false, always.

    // @Speed: This could be sped up, for sparse bit arrays, using the new bit scan intrinsics bit_scan_forward and bit_scan_reverse.
    // To clear the least bit, once the bit scan tells you where to go, subtract 1 and AND it with the old value.
    // Isolate least set bit: (x & -x); clear least set bit: (x & (x - 1)).
    // Unfortunately, reversed iteration cannot clear the bit without a variable shift, which is slower.
    // But we said above we do not want to bother with reverse!
    for slot, slot_index: array.slots {
        #if !target_value slot = ~slot;   // Static 'if' ... no runtime cost if false.
        if !slot  continue;

        base_index := slot_index * 64;
        limit      := Basic.min(array.count - base_index - 1, 63);  // Don't go past the number of bits that are actually stored.
        
        bit := 1;
        for i: 0..limit {
            is_set := slot & bit;
            bit <<= 1;
            if is_set {
                `it_index := base_index + i;

                #insert (remove=#assert(false), break=break slot) body;
            }
        }
    }
}

// See "Conditionally set or clear bits without branching" at http://graphics.stanford.edu/~seander/bithacks.html
set_bits :: inline (w: s64, m: s64, b: bool) -> s64 {
    return (w & ~m) | ((-cast(s64)b) & m);
}



// This file contains fixes placed into the public domain by Matija.
