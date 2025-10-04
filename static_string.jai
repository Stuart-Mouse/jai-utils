
// NOTE: functional capacity is capacity - 1, since we always leave a null-terminator
Static_String :: struct(capacity: int) {
    count:  s64;
    data:   [capacity] u8;
}

to_string :: inline (s: *Static_String) -> string {
    return string.{ s.count, s.data.data };
}

to_c_string :: inline (s: *Static_String) -> *u8 {
    s.data[min(s.count, s.capacity-1)] = 0; // null terminate at nominal length just in case
    return s.data.data;
}

to_static_string :: inline (src: string, $capacity: int) -> Static_String(capacity), bool {
    dst: Static_String(capacity);
    ok := copy_from_string(*dst, src);
    return dst, ok;
}

copy_from_string :: inline (dst: *Static_String, src: string) -> bool {
    if src.count >= dst.capacity  return false;
    dst.count = src.count;
    memcpy(*dst.data, src.data, dst.count);
    dst.data[dst.count] = 0;
    return true;
}

copy_to_string :: inline (s: *Static_String) -> string {
    return copy_string(string.{ s.count, s.data.data });
}

// NOTE: You can technically index the null-terminator directly here and not get an index out of bounds error. Would anyone care though? 
operator *[] :: inline (s: *Static_String, index: int) -> *u8 { return *s.data[index]; }
operator  [] :: inline (s:  Static_String, index: int) ->  u8 { return  s.data[index]; }

// TODO: These are commented out because for some reason they lead to weird compiler errors with comparisons to struct literls without explicit types
// operator == :: inline (s1: Static_String, s2: Static_String) -> bool { return to_string(s1) == to_string(s2); }
// operator == :: inline (s1: Static_String, s2: string) -> bool #symmetric { return to_string(*s1) == s2; }

compare :: inline (s1: Static_String, s2: Static_String) -> bool { return to_string(s1) == to_string(s2); }
compare :: inline (s1: Static_String, s2: string) -> bool #symmetric { return to_string(*s1) == s2; }

for_expansion :: (s: *Static_String, body: Code, flags: For_Flags) #expand {
    REVERSE    :: cast(bool) flags & .REVERSE;
    DO_POINTER :: cast(bool) flags & .POINTER;
    for <=REVERSE *=DO_POINTER  `it, `it_index: to_string(s) {
        #insert body;
    }
}

reflect_static_string :: (a: Any) -> count_ptr: *int, data: *u8, capacity: int {
    assert(is_static_string(a.type));
    struct_info := a.type.(*Type_Info_Struct);
    
    assert(struct_info.specified_parameters[0].name == "capacity");
    assert(struct_info.specified_parameters[0].type == xx int);
    capacity_cs_offset := struct_info.specified_parameters[0].offset_into_constant_storage;
    assert(capacity_cs_offset != -1);
    
    count_ptr := a.value_pointer.(*int);
    data      := (a.value_pointer + size_of(int)).(*u8);
    capacity  := (struct_info.constant_storage.data + capacity_cs_offset).(*int).*;
    return count_ptr, data, capacity;
}

is_static_string :: (a: Any) -> bool { return is_static_string(a.type); }
is_static_string :: (ti: *Type_Info) -> bool {
    return ti.type == .STRUCT && ti.(*Type_Info_Struct).polymorph_source_struct == type_info(Static_String(0)).polymorph_source_struct;
}

copy_string_to_any_static_string :: (a: Any, s: string) -> bool { 
    if !is_static_string(a)  return false;
    
    count_ptr, data, capacity := reflect_static_string(a);
    if s.count > capacity  return false;
    
    count_ptr.* = s.count;
    memcpy(data, s.data, s.count);
    return true;
};

// imgui_input_static_string :: (
//     label     : *u8, 
//     str       : *Static_String($capacity), 
//     flags     : imgui.InputTextFlags    = {}, 
//     callback  : imgui.InputTextCallback = {}, 
//     user_data : rawptr                  = nil
// ) -> bool {
//     result := imgui.InputTextEx(
//         label, static_string_to_c_string(str), 
//         uint(capacity+1), flags, callback, user_data,
//     )
//     if result {
//         str.len = len(*u8(raw_data(str.data[:])))
//     }
//     return result
// }

// imgui_input_static_string_multiline :: (
//     label     : *u8, 
//     str       : *Static_String($capacity), 
//     size      : imgui.Vec2 = {},
// ) -> bool {
//     result := imgui.InputTextMultilineEx(
//         label, 
//         static_string_to_c_string(str), 
//         uint(capacity+1),
//         size, {}, nil, nil,
//     )
//     if result {
//         str.len = len(*u8(raw_data(str.data[:])))
//     }
//     return result
// }

// static_string_gon_callback :: (node: *gon.DOM_Node) -> gon.Callback_Results {
//     if types_share_polymorphic_base_type(node.data_binding.id, Static_String(0)) {
//         data_field     := reflect.struct_field_by_name(node.data_binding.id, "data")
//         data_field_ptr := mem.ptr_offset(cast(*u8)node.data_binding.data, data_field.offset)
//         data_field_cap := data_field.type.variant.(runtime.Type_Info_Array).count - 1
        
//         // TODO: we would like this to unescape strings properly
        
//         // copy into data field
//         if len(node.value.text) > data_field_cap {
//             return {.ERROR}
//         }
//         mem.copy(data_field_ptr, raw_data(node.value.text), len(node.value.text))
//         (cast([*]u8)data_field_ptr)[len(node.value.text)] = 0
        
//         // set len field
//         (cast(*int)node.data_binding.data)* = len(node.value.text)
        
//         return {.SKIP_BINDING}
//     }
//     return {}
// }
