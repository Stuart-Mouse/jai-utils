// ========== General ==========

NULL_ANY :: Any.{};
VOID_ANY :: Any.{ type = type_info(void) };

compare_any :: (a: Any, b: Any) -> bool {
    if a.type != b.type  return false;
    return a.type == null || !memcmp(a.value_pointer, b.value_pointer, a.type.runtime_size);
}

// for when you want to quickly cast an any to a pointer to the expected type, and assert if the type is wrong
any_isa :: inline (any: Any, $T: Type) -> *T {
    // assert(any.type == xx T, "any.type == %, T == %", as_type(any.type), T);
    return ifx any.type == xx T then any.value_pointer.(*T);
}

// this is primarily so that we can get an Any of an Any, 
// which is not possible when using implicit conversion to Any
make_any :: inline (t: *$T) -> Any {
    return Any.{ xx T, t };
}


// ========== Allocation ==========

// A more convenient version of Dynamic_New which gives you back an Any
New_Any :: inline (ti: *Type_Info, $$initialized := true) -> Any {
    memory := Dynamic_New(ti, initialized);
    return ifx memory then Any.{ ti, memory } else Any.{};
}

// Dynamic_New was removed from the included modules for some reason, so I am including it here.
// Capitalized for the sole purpose of matching naming of New().
Dynamic_New :: inline (type: *Type_Info, $$initialized := true) -> *void {
    if type.runtime_size < 0  return null;   // Maybe they gave us a weird polymorphic type for some reason.

    memory := alloc(type.runtime_size);
    if initialized  initialize_any(Any.{ type, memory });
    return memory;
}

initialize_any :: (any: Any) {
    _any := any;
    while _any.type.type == .VARIANT {
        _any.type = _any.type.(*Type_Info_Variant).variant_of;
    }
    
    if _any.type.type == {
      case  .STRUCT;
        struct_info := _any.type.(*Type_Info_Struct);
        if struct_info.initializer  
            then struct_info.initializer(_any.value_pointer);
            else memset(_any.value_pointer, 0, _any.type.runtime_size);
        
      case .ARRAY;
        array := Any_Array.from(_any);
        if array.array_type == {
          case .FIXED;
            for array  initialize_any(it);
          case .VIEW;
            memset(_any.value_pointer, 0, _any.type.runtime_size);
          case .RESIZABLE;
            memset(_any.value_pointer, 0, _any.type.runtime_size);
            array.value_pointer.(*Resizable_Array).allocator = context.allocator;
        }
        
      case;
        // Non-structs always default to 0.
        // @Incomplete: Here we are not handling variants, or arrays of things that might have initializers.
        memset(_any.value_pointer, 0, _any.type.runtime_size);
    }
}

// ========== Reflection ==========

get_member :: (any_struct: Any, member_name: string) -> bool, Any, *Type_Info_Struct_Member {
    struct_info := any_struct.type.(*Type_Info_Struct);
    assert(struct_info.type == .STRUCT);
    
    member_info, member_offset := get_field(struct_info, member_name);
    if member_info {
        if member_info.flags & .CONSTANT {
            return true, Any.{ member_info.type, struct_info.constant_storage.data + member_info.offset_into_constant_storage }, member_info;
        }
        return true, Any.{ member_info.type, any_struct.value_pointer + member_info.offset_in_bytes }, member_info;
    }
    return false, NULL_ANY, null;
}

// NOTE: this version will only accept members that are directly on the given struct, not fields that were retrieved through a using.
get_member :: (any_struct: Any, member_info: *Type_Info_Struct_Member) -> member_any: Any {
    assert(any_struct.type.type == .STRUCT, "was %", as_type(any_struct.type));
    struct_info := any_struct.type.(*Type_Info_Struct);
    
    // verify that the member belongs to this struct
    assert(is_within_array(struct_info.members, member_info));
    
    if member_info.flags & .CONSTANT {
        return Any.{ member_info.type, struct_info.constant_storage.data + member_info.offset_into_constant_storage };
    }
    return Any.{ member_info.type, any_struct.value_pointer + member_info.offset_in_bytes };
}


Struct_Member_Iterator_Flags :: enum_flags {
    SKIP_CONSTANT;
    SKIP_OVERLAY;
    
    SKIP_CONSTANT_AND_OVERLAY :: SKIP_CONSTANT | SKIP_OVERLAY;
}

// also provides 'it_info' iterator pointing to Type_Info_Struct_Member
iterate_struct_members :: (any: Any, $flags: Struct_Member_Iterator_Flags, body: Code) #expand {
    assert(any.type.type == .STRUCT);
    ti_struct := any.type.(*Type_Info_Struct);
    
    #if flags & .SKIP_OVERLAY {
        last_member_offset := -1;
    }
    for *`it_info, `it_index: ti_struct.members {
        #if flags & .SKIP_CONSTANT {
            if it_info.flags & .CONSTANT  continue;
        }
        #if flags & .SKIP_OVERLAY {
            if it_info.flags & .OVERLAY  continue;
        }
        #if flags & .SKIP_OVERLAY {
            // NOTE: This skips unions entirely, since all members have the same offset. 
            //       So probably just don't use .SKIP_PLACE with unions I guess.
            if it_info.offset_in_bytes <= last_member_offset  break;
            last_member_offset = it_info.offset_in_bytes;
        }
        `it := Any.{ it_info.type, any.value_pointer + it_info.offset_in_bytes };
        #insert body;
    }
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
    _type:      Type;   // Underscore is to prevent collision with _type_info.type
    pointer:    *void;
    
    #overlay(_type) using _type_info: *Type_Info_Procedure;
    
    from :: to_any_proc;
}

to_any_proc :: inline (any: Any) -> Any_Proc { 
    assert(any.type.type == .PROCEDURE);
    return .{ as_type(any.type), any.value_pointer.(**void).* };
}

to_any :: inline (any: *Any_Proc) -> Any {
    return Any.{ any._type_info, *any.pointer };
}


// ========== Any_Number ==========

/*
    Any_Number is an Any, but we only use it for small values (<= 8 bytes), which get packed into the space of the value_pointer.
    While it's called 'Any_Number', it's also perfectly fine to use for pointers. 
    In fact I used to have value_pointer as a member of the union, but that doesn't always play nice at compile-time.
    But, you can still stick pointers in there and get them out all the same with to_any_number and get_as.
    Could also be used as a standard Any if we wanted to, but I haven't written any such procedures for this use case yet.
    
    TODO: some kind of metaprogram thing to prevent passing these special Any_X structs implicitly as an Any would be helpful.
          or, would be nice if we could just modify how these structs get coerced to Any so that they have more intuitive behavior.
*/

Any_Number :: struct {
    type:  *Type_Info;
    using value:  Number_Union;
    
    from :: to_any_number;
    from :: any_to_any_number;
    
    zero_of_type :: inline (T: Type) -> Any_Number { 
        return zero_of_type(T.(*Type_Info)); 
    };
    zero_of_type :: inline (T: *Type_Info) -> Any_Number { 
        assert(can_store_type(T));
        return Any_Number.{ type = T }; 
    };
    
    can_store_type :: inline (T: Type) -> bool { return can_store_type(T.(*Type_Info)); };
    can_store_type :: inline (T: *Type_Info) -> bool { return T.runtime_size <= 8; };
}

to_any :: inline (any: *Any_Number) -> Any { 
    return Any.{ any.type, *any.value }; 
};

get_as :: inline ($T: Type, any: Any_Number) -> T, bool #modify {
    return Any_Number.can_store_type(T);
} {
    // TODO: add case for reading through value_pointer later, if we decide to do that sort of thing.
    return (*any.value).(*T).*, any.type == type_info(T);
}

to_any_number :: inline (value: $T) -> Any_Number #modify {
    return Any_Number.can_store_type(T);
} { 
    ret := Any_Number.{ type = type_info(T) }; 
    (*ret.value).(*T).* = value;
    return ret;
}

any_to_any_number :: inline (value: Any) -> Any_Number {
    assert(Any_Number.can_store_type(value.type));
    
    ret := Any_Number.{ type = value.type }; 
    memcpy(*ret.value, value.value_pointer, value.type.runtime_size);
    return ret;
}

// TODO: maybe we want to move this to some separate file for dynamic math stuff...
negate_number :: inline (any: *Any_Number) -> bool {
    if any.type.type == {
      case .INTEGER;
        ti_integer := any.type.(*Type_Info_Integer);
        if ti_integer.signed == false {
            log("Any_Number passed to negate_number was an unsigned integer type! (was %)", as_type(any.type));
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
    
    log("Any_Number passed to negate_number was not a valid numeric type! (was %)", as_type(any.type));
    return false;
}


Number_Union :: union {
    as_u\8:     u\8;
    as_u16:     u16;
    as_u32:     u32;
    as_u64:     u64;
    as_s\8:     s\8;
    as_s16:     s16;
    as_s32:     s32;
    as_s64:     s64;
    as_float32: float32;
    as_float64: float64;
    as_bytes:   [8] u8;
    
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
    using array_info:   *Type_Info_Array;
    value_pointer:      *void;
    
    from :: make_any_array;
}

make_any_array :: (any: Any) -> Any_Array {
    assert(any.type.type == .ARRAY, "Cannot make an Any_Array from type '%'.", as_type(any.type));
    return .{ xx any.type, any.value_pointer };
}

get_count_and_data :: inline (array: Any_Array) -> (int, *void) {
    if array.array_type == .FIXED {
        return array.array_count, array.value_pointer;
    } else {
        raw_array := array.value_pointer.(*Resizable_Array);
        return raw_array.count, raw_array.data;
    }
}


get_index :: inline (array: Any_Array, index: int) -> Any, bool {
    count, data := get_count_and_data(array);
    if index < 0 || index >= count {
        return NULL_ANY, false;
    }
    return Any.{ array.element_type, data + (index * array.element_type.runtime_size) }, true;
}
operator [] :: get_index;


set_index :: inline (array: Any_Array, index: int, value: Any) {
    count, data := get_count_and_data(array);
    assert(index > 0 && index < count, "Array index % is out of bounds. array.count = %", index, count);
    
    element := Any.{ array.element_type, data + (index * array.element_type.runtime_size) };
    memcpy_any(element, value);
}
operator []= :: set_index;


for_expansion :: (array: Any_Array, body: Code, for_flags: For_Flags) #expand {
    count, data := get_count_and_data(array);
    `it := Any.{ array.element_type, data };
    for `it_index: 0..count-1 {
        #insert body;
        it.value_pointer += array.element_type.runtime_size;
    }
}

is_null :: (array: Any_Array) -> bool {
    count, data := get_count_and_data(array);
    return data == null;
}

get_element_index :: (array: Any_Array, element_pointer: *void) -> int {
    count, data := get_count_and_data(array);
    index := (element_pointer - data) / array.element_type.runtime_size;
    assert(index >= 0 && index < count, "element_pointer % was not within valid array data range: %, %. calculated index: %", element_pointer, data, data + count * array.element_type.runtime_size, index);
    // assert((element_pointer - data) % array.element_type.runtime_size == 0, "element_pointer was not properly aligned");
    return index;
}

resize :: (array: Any_Array, new_count: int, $initialize := false) {
    assert(array.array_type != .FIXED);
    
    old_count: int;
    if array.array_type == {
      case .RESIZABLE;
        raw_array := array.value_pointer.(*Resizable_Array);
        old_count  = raw_array.count;
        
        array_reserve(raw_array, new_count, array.element_type.runtime_size);
        raw_array.count = new_count;
        
      case .VIEW;
        raw_array := array.value_pointer.(*Array_View_64);
        old_count  = raw_array.count;
        
        if old_count < new_count {
            raw_array.data = realloc(raw_array.data,
                new_count * array.element_type.runtime_size, 
                old_count * array.element_type.runtime_size
            );
        }
        raw_array.count = new_count;
    }
    
    #if initialize  init_range(array, old_count, new_count - old_count);
}

// TODO: maybe init_range should just use our dynamic array_view and then call initialize_any for each element
//       that could be slightly slower, but it would be more simple and robust to changes in how we intitialize Any's
init_range :: (array: Any_Array, begin: s64, count: s64) {
    if count <= 0  return;
    
    array_count, array_data := get_count_and_data(array);
    assert(begin >= 0 && begin + count <= array_count, "begin: %, count: %, array_count: %", begin, count, array_count);
    
    if array.element_type.type == .STRUCT {
        struct_info := array.element_type.(*Type_Info_Struct);
        if struct_info.initializer {
            for array_view(array, begin, count) {
                struct_info.initializer(it.value_pointer);
            }
            return;
        }
    }
    
    memset(array_data + begin * array.element_type.runtime_size, 0, count * array.element_type.runtime_size );
}

array_view :: (array: Any_Array, index: int, count := S64_MAX) -> Many_Any {
    array_count, array_data := get_count_and_data(array);
    
    assert(index >= 0, "index = %", index);
    assert(count >= 0, "count = %", count);
    
	result: Many_Any = ---;
    result.info = array.element_type;
    
    if index >= array_count {
        return result;
    }
    if index + count > array_count {
        count = array_count - index;
    }
    
    result.count = count;
    result.data  = array_data + index * array.element_type.runtime_size;
    return result;
}

/*
    The Many_Any is sort of a combination of an Any and an Array_View_64.
    Using this removes the need for 2 levels of indirection to the underlying array data.
*/
Many_Any :: struct {
    using info:  *Type_Info; // element type info
    data:        *void;
    count:       s64;
}

for_expansion :: (view: Many_Any, body: Code, for_flags: For_Flags) #expand {
    `it := Any.{ view.info, view.data };
    for `it_index: 0..view.count-1 {
        #insert body;
        it.value_pointer += view.info.runtime_size;
    }
}

// TODO: make_many_any
// TODO: many_any resize / allocate

