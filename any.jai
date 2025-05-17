
// Combination of an Any and an Array_View_64
// Removes the need for 2 levels of indirection to underlying array data.
// This is a joke, I think.
Many_Any :: struct {
    type:  *Type_Info; // element type
    data:  *void;
    count: s64;
}

Null_Any :: Any.{};
Void_Any :: Any.{ type = type_info(void) };

// ========== Allocation ==========

// A more convenient version of Dynamic_New which gives you back an Any
New_Any :: inline (ti: *Type_Info, initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}

// Dynamic_New was removed from the included modules for some reason, so I am including it here.
// Capitalized for the sole purpose of matching naming of New().
Dynamic_New :: (type: *Type_Info, initialized := true) -> *void {
    if type.runtime_size < 0  return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(type.runtime_size);
    if initialized  initialize_any(Any.{ type, memory });
    return memory;
}

initialize_any :: (any: Any) {
    if any.type.type == .STRUCT {
        tis := any.type.(*Type_Info_Struct);
        if tis.initializer  tis.initializer(any.value_pointer);
        else                memset(any.value_pointer, 0, any.type.runtime_size);
    } else {
        // Non-structs always default to 0.
        // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
        memset(any.value_pointer, 0, any.type.runtime_size);
    }
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

unwrap_variant_type :: (any: Any) -> Any, bool {
    if any.type.type == .VARIANT {
        return Any.{
            any.type.(*Type_Info_Variant).variant_of,
            any.value_pointer
        }, true;
    }
    return any, false;
}

dereference_any_pointer :: (any: Any) -> Any, bool {
    if any.type.type == .POINTER {
        return Any.{
            any.type.(*Type_Info_Pointer).pointer_to,
            any.value_pointer.(**void).*
        }, true;
    }
    return any, false;
}

memcpy_any :: (dst: Any, src: Any) {
    assert(dst.type == src.type, "% != %", as_type(dst.type), as_type(src.type));
    memcpy(dst.value_pointer, src.value_pointer, src.type.runtime_size);
}

// ========== Any_Proc ==========

/*
    While this may seem totally redundant with regular Any at first glance, 
        note that the pointer here points directly to the procedure, 
        whereas in a normal Any, we actually get a pointer to a pointer to a procedure.
    The removal of this extra indirection is really important if you want to store or pass around a procedure with its type info.
    For example, I use this in Lead Sheets for all external procedures that are passed into the script's context.
*/

Any_Proc :: struct {
    type:       *Type_Info_Procedure;
    pointer:    *void;
    
    from :: to_any_proc;
}

to_any_proc :: inline (any: Any) -> Any_Proc { 
    assert(any.type.type == .PROCEDURE);
    return .{ xx any.type, any.value_pointer.(**void).* };
}

to_any :: inline (any: *Any_Proc) -> Any {
    return .{ any.type, *any.pointer };
}


// ========== Small_Any ==========

/*
    Small_Any is an Any, but we only use it for small values (<= 8 bytes), which get packed into the space of the value_pointer.
    Can also be used as a standard Any if we wanted to, but I haven't written any such procedures for this use case yet.
*/

Small_Any :: struct {
    type: *Type_Info;
    union {
        using value_number:  Number_Union;
        value_bytes:   [8] u8;
        value_pointer: *void;
    }
    
    from :: to_small_any;
}

to_any :: inline (any: *Small_Any) -> Any { 
    return Any.{ any.type, *any.value_bytes }; 
};

get_as :: inline ($T: Type, any: Small_Any) -> T, bool #modify {
    return T.(*Type_Info).runtime_size <= 8;
} {
    // TODO: add case for reading through value_pointer later, if we decide to do that sort of thing.
    return (*any.value_bytes).(*T).*, any.type == type_info(T);
}

to_small_any :: inline (value: $T) -> Small_Any #modify {
    return T.(*Type_Info).runtime_size <= 8;
} { 
    ret := Small_Any.{ type_info(T) }; 
    (*ret.value_bytes).(*T).* = value;
    return ret;
}

to_small_any :: inline (value: Any) -> Small_Any {
    assert(value.type.runtime_size <= 8);
    
    ret := Small_Any.{ value.type }; 
    memcpy(*ret.value_bytes, value.value_pointer, value.type.runtime_size);
    return ret;
}

// TODO: maybe we want to move this to some separate file for dynamic math stuff...
negate_number :: inline (any: *Small_Any) -> bool {
    if any.type.type == {
      case .INTEGER;
        ti_integer := any.type.(*Type_Info_Integer);
        if ti_integer.signed == false {
            log("Small_Any passed to negate_number was an unsigned integer type! (was %)", as_type(any.type));
            return false;
        }
        if any.type.runtime_size == {
          case 1; val := (*any.value).(*s\8); val.* = -val.*;
          case 2; val := (*any.value).(*s16); val.* = -val.*;
          case 4; val := (*any.value).(*s32); val.* = -val.*;
          case 8; val := (*any.value).(*s64); val.* = -val.*;
        }
        return true;
        
      case .FLOAT;
        if any.type.runtime_size == {
          case 4; val := (*any.value).(*float32); val.* = -val.*;
          case 8; val := (*any.value).(*float64); val.* = -val.*;
        }
        return true;
    }
    
    log("Small_Any passed to negate_number was not a valid numeric type! (was %)", as_type(any.type));
    return false;
}


// Although it has uses outside the Small_Any, the purpose of the Number_Union is similar enough that I felt like putting it here.
// NOTE: as_pointer is omitted because the compiler has special autism for pointers returned from #run. Just use a u64 for that I guess.
Number_Union :: union {
    as_u\8: u\8;
    as_u16: u16;
    as_u32: u32;
    as_u64: u64;
    as_s\8: s\8;
    as_s16: s16;
    as_s32: s32;
    as_s64: s64;
    as_float32: float32;
    as_float64: float64;
    
    from :: to_number_union;
}

to_number_union :: inline (value: $T) -> Number_Union #modify {
    ti := T.(*Type_Info);
    return ti.type == .INTEGER || ti.type == .FLOAT || ti.type == .ENUM  || ti.type == .POINTER;
} {
    result: Number_Union;
    memcpy(*result, *value, size_of(T));
    return result;
}

// NOTE: just a simple bitwise comparison, does consider actual stored type
operator == :: (a: Number_Union, b: Number_Union) -> bool {
    return (*a).(*u64).* == (*b).(*u64).*;
}

// ========== Any_Array ==========

// While this is literally just an Any, it's helpful to have a distinct type with array-specific procedures.
// The main reason I really created this was for the for expansion, which can be quite useful when iterating over an array of unknown type.
Any_Array :: struct {
    type: *Type_Info_Array;
    value_pointer: *void;
    
    from :: make_any_array;
}

make_any_array :: (any: Any) -> Any_Array {
    assert(any.type.type == .ARRAY);
    return .{ xx any.type, any.value_pointer };
}

get_count_and_data :: inline (array: Any_Array) -> (int, *void) {
    count, data := get_array_count_and_data(array.value_pointer, array.type);
    return count, data;
}

get_index :: inline (array: Any_Array, index: int) -> Any {
    count, data := get_count_and_data(array);
    assert(index >= 0 && index < count);
    return Any.{ array.element_type, data + (array.element_type.runtime_size * index) };
}

for_expansion :: (array: Any_Array, body: Code, for_flags: For_Flags) {
    count, data := get_count_and_data(array);
    `it = Any.{ array.element_type, data };
    for `it_index: 0..count-1 {
        #insert body;
        it.value_pointer += array.element_type.runtime_size;
    }
}
