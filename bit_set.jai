
Int_Bit_Set :: struct(MIN: int, MAX: int) {
    MASK_SIZE :: ((MAX - MIN + 1) / 8) + cast(int) ((MAX - MIN + 1) % 8 != 0);
    mask: [MASK_SIZE] u8;
}

check_bit :: (using bit_set: Int_Bit_Set, check_bits: ..int) -> bool {
    for check_bits {
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        if it < MIN || it > MAX || bit_index & mask[byte_index] == 0 {
            log("Bit % was not set", it);
            return false;
        }
    }
    return true;
}

set_bit :: (using bit_set: *Int_Bit_Set, set_bits: ..int) -> bool {
    for set_bits {
        if it < MIN || it > MAX { 
            log("Failed to set bit %", it);
            return false;
        }
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        mask[byte_index] |= bit_index;
    }
    return true;
}



Enum_Bit_Set :: struct($T: Type) {
    MIN :: #run -> int { 
        min := type_info(T).values[0]; 
        for type_info(T).values  if it < min  min = it;
        return min;
    };
    MAX :: #run -> int { 
        max := type_info(T).values[0]; 
        for type_info(T).values  if it > max  max = it;
        return max;
    };
    MASK_SIZE :: ((MAX - MIN + 1) / 8) + cast(int) ((MAX - MIN + 1) % 8 != 0);

    mask: [MASK_SIZE] u8;
    
    from :: #procedure_of_call make_bit_set((0).(T));
}

check_bit :: (using bit_set: Enum_Bit_Set, bit: bit_set.T) -> bool {
    byte_index := (bit - MIN) / 8;
    bit_index  := cast(u8) 1 << ((bit - MIN) % 8);
    return bit < MIN || bit > MAX || bit_index & mask[byte_index] == 0;
}

check_bits :: (using bit_set: Enum_Bit_Set, check_bits: ..bit_set.T) -> bool {
    for check_bits {
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        if it < MIN || it > MAX || bit_index & mask[byte_index] == 0 {
            log("Bit % was not set", it);
            return false;
        }
    }
    return true;
}

set_bit :: (using bit_set: *Enum_Bit_Set, bit: bit_set.T) -> bool {
    if bit < MIN || bit > MAX { 
        log("Failed to set bit %", bit);
        return false;
    }
    byte_index := (bit - MIN) / 8;
    bit_index  := cast(u8) 1 << ((bit - MIN) % 8);
    mask[byte_index] |= bit_index;
}

set_bits :: (using bit_set: *Enum_Bit_Set, bits_to_set: ..bit_set.T) -> bool {
    for bits_to_set {
        if it < MIN || it > MAX { 
            log("Failed to set bit %", it);
            return false;
        }
        byte_index := (it - MIN) / 8;
        bit_index  := cast(u8) 1 << ((it - MIN) % 8);
        mask[byte_index] |= bit_index;
    }
    return true;
}

make_bit_set :: (bits_to_set: ..$T) -> Enum_Bit_Set(T) #modify {
    ti := cast(*Type_Info_Enum) T;
    return ti.type == .ENUM && !(ti.enum_type_flags & .FLAGS);
} {
    set: Enum_Bit_Set(T);
    for bits_to_set {
        if it >= set.MIN && it <= set.MAX { 
            byte_index := (it - set.MIN) / 8;
            bit_index  := cast(u8) 1 << ((it - set.MIN) % 8);
            set.mask[byte_index] |= bit_index;
        }
    }
    return set;
}

