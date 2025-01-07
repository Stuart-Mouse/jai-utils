
// Combination of an Any and an Array_View_64
// Removes the need for 2 levels of indirection to underlying array data.
// This is a joke, I think.
Many_Any :: struct {
    type  : *Type_Info; // element type
    data  : *void;
    count : s64;
}

// ========== Allocation ==========

// more convenient version of Dynamic_New which gives you back an Any
Dynamic_New_Any :: inline (ti: *Type_Info, initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}

// Dynamic_New was removed from the included modules for some reason, so I am including it here.
// Capitalized for the sole purpose of matching naming of New().
Dynamic_New :: (ti: *Type_Info, initialized := true) -> *void {
    size := ti.runtime_size;
    if size < 0 return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(size);

    if initialized {
        if ti.type == .STRUCT {
            // Structs can have initializers. If the initializer is null, the struct is to be zeroed.
            tis := cast(*Type_Info_Struct) ti;
            if tis.initializer  tis.initializer(memory);
            else                memset(memory, 0, size);
        } else {
            // Non-structs always default to 0.
            // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
            memset(memory, 0, size);
        }
    }

    return memory;
}

// ========== Reflection ==========

get_member_any :: (parent: Any, member_name: string) -> Any {
    assert(parent.type.type == .STRUCT);
    
    parent_ti := cast(*Type_Info_Struct) parent.type;
    
    member: *Type_Info_Struct_Member;
    for *parent_ti.members {
        if it.name == member_name {
            member = it;
            break;
        }
    }
    if !member  return Any.{};
    
    return get_member_any(parent.value_pointer, member);
}

get_member_any :: (parent_ptr: *void, member_ti: *Type_Info_Struct_Member) -> Any {
    if !member_ti  return Any.{};
    value_pointer := parent_ptr + member_ti.offset_in_bytes;
    return Any.{ member_ti.type, value_pointer };
}
