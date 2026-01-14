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

memcpy_any :: (dst: Any, src: Any) {
    assert(dst.type == src.type, "% != %", as_type(dst.type), as_type(src.type));
    memcpy(dst.value_pointer, src.value_pointer, src.type.runtime_size);
}

unwrap_any_variant :: (any: Any) -> Any {
    assert(any.type.type == .VARIANT);
    return Any.{
        any.type.(*Type_Info_Variant).variant_of,
        any.value_pointer
    };
}

dereference_any_pointer :: (any: Any) -> Any {
    assert(any.type.type == .POINTER);
    return Any.{
        any.type.(*Type_Info_Pointer).pointer_to,
        any.value_pointer.(**void).*
    };
}

// TODO: Figure out how to make type_type an optional bake. Compiler doesn't like it like that, but I thought I saw some workaround on the discord a while back...
assert_base_type :: (type: *Type_Info, $type_type: Type_Info_Tag) -> $R #modify {
    if type_type == {
      case .VARIANT;    R = *Type_Info_Variant;
      case .STRUCT;     R = *Type_Info_Struct;
      case .ARRAY;      R = *Type_Info_Array;
      case .INTEGER;    R = *Type_Info_Integer;
      case .FLOAT;      R = *Type_Info_Float;
      case .PROCEDURE;  R = *Type_Info_Procedure;
      case .ENUM;       R = *Type_Info_Enum;
      case .POINTER;    R = *Type_Info_Pointer;
      case;             R = *Type_Info;
    }
    return true;
} {
    base_type := get_variant_base_type(type);
    assert(base_type.type == type_type, "type must be a %, but it was a %", type_type, base_type.type);
    return xx base_type;
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

// ========== Structs ==========

// NOTE: The name Any_Struct is already taken by Preload.jai, unfortunately.
Any_Structure :: struct {
    using struct_info:  *Type_Info_Struct;
    value_pointer:      *void;
    
    #overlay(struct_info) as_any: Any;
    
    from :: to_any_structure;
}

to_any_structure :: (any: Any) -> Any_Structure {
    return .{ assert_base_type(any.type, .STRUCT), any.value_pointer };
}

// NOTE: `member_offset` is returned in case we retrieved a member through a using.
//       If the returned member is a constant, member_offset will always be returned as -1.
get_member :: (any_struct: Any_Structure, name: string, recursive := false) -> (
    found:          bool, 
    member_any:     Any, 
    member_info:    *Type_Info_Struct_Member, 
    member_offset:  int
) {
    member_info, member_offset := get_member_info(any_struct.struct_info, name, recursive);
    if member_info == null  return false, Any.{}, null, 0;
    return true, Any.{ member_info.type, any_struct.value_pointer + member_offset }, member_info, member_offset;
}

get_struct_member :: (any: Any, name: string, recursive := false) -> bool, Any, *Type_Info_Struct_Member, int {
    found, member_any, member_info, member_offset := get_member(Any_Structure.from(any), name, recursive);
    return found, member_any, member_info, member_offset;
}

// NOTE: this version will only accept members that are directly on the given struct, not fields that were retrieved through a using.
get_struct_member :: (any: Any, member_info: *Type_Info_Struct_Member) -> member_any: Any {
    struct_info := assert_base_type(any.type, .STRUCT);
    
    // verify that the member belongs to this struct
    assert(is_within_array(struct_info.members, member_info));
    
    if member_info.flags & .CONSTANT {
        return Any.{ member_info.type, struct_info.constant_storage.data + member_info.offset_into_constant_storage };
    }
    return Any.{ member_info.type, any.value_pointer + member_info.offset_in_bytes };
}

get_specified_parameter :: (any_struct: Any_Structure, name: string) -> bool, Any, *Type_Info_Struct_Member {
    for *any_struct.specified_parameters {
        if it.name == name {
            return true, Any.{ it.type, *any_struct.constant_storage[it.offset_in_bytes] }, it;
        }
    }
    return false, NULL_ANY, null;
}

// also provides 'it_info' iterator pointing to Type_Info_Struct_Member
for_expansion :: (structure: Any_Structure, body: Code, flags: For_Flags) #expand {
    // TODO: it_index here is not very meaningful...
    for *`it_info, `it_index: structure.members {
        #if flags & .POINTER == 0 {
            if it_info.flags & .CONSTANT  continue;
        }
        #if flags & .REVERSE == 0 {
            if it_info.flags & .OVERLAY  continue;
        }
        
        `it := Any.{ it_info.type, structure.value_pointer + it_info.offset_in_bytes };
        
        #insert body;
    }
}

intialize :: (structure: Any_Structure) {
    if structure.initializer  structure.initializer(structure.value_pointer);
    else memset(structure.value_pointer, 0, structure.runtime_size);
}

// ========== Any_Proc ==========

/*
    While this may seem totally redundant with regular Any at first glance, 
        note that the pointer here points directly to the procedure, 
        whereas in a normal Any, we actually get a pointer to a pointer to a procedure.
    The removal of this extra indirection is really important if you want to store or pass around a procedure with its type info.
    For example, I use this in Lead Sheets for all external procedures that are passed into the script's context.
    
    TODO: See if we can make the pointer be some proceudre type like (void) -> void and still get proper remapping if we try to return an Any_Proc from compile-time execution.
*/

Any_Proc :: struct {
    using proc_info:    *Type_Info_Procedure;
    pointer:    *void;
    
    from :: to_any_proc;
}

to_any_proc :: inline (any: Any) -> Any_Proc { 
    return .{ assert_base_type(any.type, .PROCEDURE), any.value_pointer.(**void).* };
}

to_any :: inline (any: *Any_Proc) -> Any {
    return Any.{ any.proc_info, *any.pointer };
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
    
    NOTE: Any_Number uses an actual Type for its type field (unlike Any) and overlays a *Type_Info so that you can return one from compile-time execution if you so wish, e.g. `#run Any_Number.from(35)`
    TODO: Evaluate if this change is actually useful, and if not then maybe just revert it since it does add just a bit of (possibly needless) complexity.
*/

Any_Number :: struct {
    __type:  Type;
    #overlay(__type) type: *Type_Info;
    
    using,except(from) value:  Number_Union;
    
    from :: to_any_number;
    
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

get_as :: inline ($T: Type, any: Any_Number) -> bool, T #modify {
    return Any_Number.can_store_type(T);
} {
    // TODO: add case for reading through value_pointer later, if we decide to do that sort of thing.
    return any.type == type_info(T), (*any.value).(*T).*;
}

to_any_number :: inline (value: $T) -> Any_Number #modify {
    return T.(*Type_Info).type == .ANY || Any_Number.can_store_type(T);
} { 
    #if type_info(T).type == .ANY {
        assert(Any_Number.can_store_type(value.type));
        ret := Any_Number.{ type = value.type }; 
        memcpy(*ret.value, value.value_pointer, value.type.runtime_size);
        return ret;
    } else {
        ret := Any_Number.{ __type = T }; 
        (*ret.value).(*T).* = value;
        return ret;
    }
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

Number_Union :: struct {
    as_bytes:   [8] u8;
    
    #overlay(as_bytes) as_u\8:     u\8;
    #overlay(as_bytes) as_u16:     u16;
    #overlay(as_bytes) as_u32:     u32;
    #overlay(as_bytes) as_u64:     u64;
    #overlay(as_bytes) as_s\8:     s\8;
    #overlay(as_bytes) as_s16:     s16;
    #overlay(as_bytes) as_s32:     s32;
    #overlay(as_bytes) as_s64:     s64;
    #overlay(as_bytes) as_float32: float32;
    #overlay(as_bytes) as_float64: float64;
    #overlay(as_bytes) as_pointer: *void;
    
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

// NOTE: just a simple bitwise comparison, does not consider actual stored type
operator == :: (a: Number_Union, b: Number_Union) -> bool {
    return (*a).(*u64).* == (*b).(*u64).*;
}

// ========== Any_Array ==========

// While this is literally just an Any, it's helpful to have a distinct type with array-specific procedures.
// The main reason I really created this was for the for expansion, which can be quite useful when iterating over an array of unknown type.
Any_Array :: struct {
    using array_info:   *Type_Info_Array;
    value_pointer:      *void;
    
    #overlay(array_info) as_any: Any;
    
    from :: to_any_array;
}

to_any_array :: (any: Any) -> Any_Array {
    return .{ assert_base_type(any.type, .ARRAY), any.value_pointer };
}

// NOTE: basically duplicated from Reflection module
get_array_count_and_data :: inline (value_pointer: *void, array_info: *Type_Info_Array) -> (int, *void) {
    if array_info.array_type == .FIXED {
        return array_info.array_count, value_pointer;
    } else {
        raw_array := value_pointer.(*Resizable_Array);
        return raw_array.count, raw_array.data;
    }
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
    `it       := Any.{ array.element_type, data };
    `it_index := 0;
    while it_index < count {
        defer {
            it.value_pointer += array.element_type.runtime_size;
            it_index += 1;
        }
        #insert body;
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

resize :: (array: Any_Array, new_count: int, $$initialize := false) {
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
    
    if initialize  init_range(array, old_count, new_count - old_count);
}

init_range :: (array: Any_Array, begin: s64, count: s64) {
    array_count, array_data := get_count_and_data(array);
    view := Many_Any.{ 
        element_type = array.element_type, 
        count        = array_count, 
        data         = array_data,
    };
    init_range(view, begin, count);
}

array_view :: (array: Any_Array, index: int, count := S64_MAX) -> Many_Any {
    array_count, array_data := get_count_and_data(array);
    
    assert(index >= 0, "index = %", index);
    assert(count >= 0, "count = %", count);
    
    result: Many_Any = ---;
    result.element_type = array.element_type;
    
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
    Many_Any is sort of a combination of an Any and an Array_View_64.
    Using this removes the need for 2 levels of indirection to the underlying array data.
    We also provide some nice operator overloads, a for_expansion, and some useful procedures for handling allocations.
    
    For most reflection-related purposes, the Any_Array (also included in my Utils module) is probably a better choice.
    With the Many_any, we aren't referencing any underlying array data type (e.g. [4] int, [] int, [..] int) like we are with an Any_Array.
    Instead, the Many_Any is just a view onto the array data that any of these structures would point at.
    This gives us the freedom to slice the Many_Any in a way we can't really do with the Any_Array.
    (You may notice that calling array_view on an Any_Array actually produces a Many_Any.)
    
    In essence, the Many_Any is the the Any_Array what the array view is to a fixed or resizable array.
    You will find it most useful in situations where you need to modify only the elements, or consider sub-views of some larger array of unknown type.
    For an example of its use, you can see Any_Hash_Table.jai, where it is used to handle the Any_Table's `entries` array.
*/

Many_Any :: struct {
    using element_type: *Type_Info; // element type info
    using array_view:   Array_View_64;
    
    from :: make_many_any;
}

make_many_any :: (any: Any) -> Many_Any {
    assert(!(any.type.type == .ARRAY && any.type.(*Type_Info_Array).array_type == .VIEW), "A Many_Any can only be made from an array view. Got type '%'.", (*any.type).(*Type).*);
    raw_array := any.value_pointer.(*Array_View_64).*;
    return .{ xx any.type, raw_array };
}

for_expansion :: (view: Many_Any, body: Code, for_flags: For_Flags) #expand {
    `it       := Any.{ view.element_type, view.data };
    `it_index := 0;
    while it_index < view.count {
        defer {
            it.value_pointer += view.element_type.runtime_size;
            it_index += 1;
        }
        #insert body;
    }
}

get_index :: inline (view: Many_Any, index: int) -> Any {
    assert(index >= 0 && index < view.count);
    return Any.{ view.element_type, view.data + (index * view.element_type.runtime_size) };
}
operator [] :: get_index;


set_index :: inline (view: Many_Any, index: int, value: Any) {
    assert(index > 0 && index < view.count, "Array index % is out of bounds. count = %", index, view.count);
    
    element := Any.{ view.element_type, view.data + (index * view.element_type.runtime_size) };
    memcpy(element.value_pointer, value.value_pointer, element.type.runtime_size);
}
operator []= :: set_index;


array_view :: (view: Many_Any, index: int, count := S64_MAX) -> Many_Any {
    assert(index >= 0, "index = %", index);
    assert(count >= 0, "count = %", count);
    
    result: Many_Any = ---;
    result.element_type = view.element_type;
    
    if index >= view.count {
        return result;
    }
    if index + count > view.count {
        count = view.count - index;
    }
    
    result.count = count;
    result.data  = view.data + index * view.element_type.runtime_size;
    return result;
}

New_Many_Any :: (count: int, type: *Type_Info, $$initialize := true) -> Many_Any {
    view := Many_Any.{
        element_type = type,
        data         = alloc(count * type.runtime_size),
        count        = count,
    };
    if initialize  init_range(view, 0, count);
    return view;
}

resize :: (view: Many_Any, new_count: int, $$initialize := false) {
    old_count := view.count;
    if old_count < new_count {
        view.data = realloc(view.data,
            new_count * view.element_type.runtime_size, 
            old_count * view.element_type.runtime_size
        );
    }
    view.count = new_count;
    
    if initialize  init_range(view, old_count, new_count - old_count);
}

// TODO: deduplicate with Any_Array's init_range, just pass required info for both to work
//       maybe also do the same for array_view, idk
init_range :: (view: Many_Any, begin: s64, count: s64) {
    if count <= 0  return;
    
    assert(begin >= 0 && begin + count <= view.count, "begin: %, count: %, array_count: %", begin, count, view.count);
    
    if view.element_type.type == .STRUCT {
        struct_info := view.element_type.(*Type_Info_Struct);
        if struct_info.initializer {
            for array_view(view, begin, count) {
                struct_info.initializer(it.value_pointer);
            }
            return;
        }
    }
    
    memset(view.data + begin * view.element_type.runtime_size, 0, count * view.element_type.runtime_size);
}

// Any_Poly :: struct(T: Type) #modify {
//     struct_info := T.(*Type_Info);
//     if struct_info.type != .STRUCT  return false, "Provided type must be a struct.";
//     return true;
// } {
//     #insert -> string {
//         // given type T must also be polymorphic
//         // then we scan the declaration/type info and we copy over fields
//         // if the field is of polymorphic type, we replace it with an Any
//         // if the field is not polymorphic, we just replace it with a pointer to the base type
        
//         // we probably need to take the base type/struct as code so that we can reject it if it does anything too funky
//         // for example, there's no realistic way we can handle some polymorphic type like an Enumerated_Array
        
//         // this idea may not really work, tbh...
//         // and would even do us any good in, for example, the Table.Entry case, since that type is not really polymoprhic within the context of the table itself.
//     }
// }

// something maybe smarter to replace the Many_Any
// if no Iterator_Type is provided, this is just a Many_Any
// we will try to do some crazy for_expansion code gen stuff
// Any_Array_View :: struct(Iterator_Type: Type = Any) #modify {
//     if Iterator_Type == Any  return true;
    
//     struct_info := Element_Type.(*Type_Info);
//     if struct_info.type != .STRUCT  return false, "Provided type must be a struct.";
//     return true;
// } {
    
    
// }


