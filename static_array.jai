
Static_Array :: struct(capacity: int, T: Type) {
    count:  int;
    data:   [capacity] T;
}

to_view :: (array: *Static_Array) -> [] array.T {
    return .{ array.count, array.data.data };
}

copy_from_view :: (array: *Static_Array, view: [] array.T) -> bool {
    if view.count > array.capacity  return false;
    
    array.count = view.count;
    memcpy(*array.data, view.data, view.count);
}

operator *[] :: inline (array: *Static_Array, index: int) -> *array.T { return *array.data[index]; }
operator  [] :: inline (array:  Static_Array, index: int) ->  array.T { return  array.data[index]; }

try_append :: inline (array: *Static_Array, elem: array.T) -> bool {
    if array.count >= array.capacity  return false;
    array.data[array.count] = elem;
    array.count += 1;
    return true;
}

for_expansion :: (array: *Static_Array, body: Code, flags: For_Flags) #expand {
    REVERSE    :: cast(bool) flags & .REVERSE;
    DO_POINTER :: cast(bool) flags & .POINTER;
    
    for <=REVERSE *=DO_POINTER `it, `it_index: array.data {
        if it_index == array.count  break;
        #insert body;
    }
}

push :: inline (array: *Static_Array, elem: array.T) -> bool {
    assert(array.count < array.capacity);
    array.data[array.count] = elem;
    array.count += 1;
}

pop :: inline (array: *Static_Array) -> array.T {
    assert(array.count > 0);
    defer array.count -= 1;
    return array.data[array.count];
}

reflect_static_array :: (a: Any) -> count_ptr: *int, data: *u8, capacity: int, element_type: *Type_Info {
    assert(is_static_array(a.type));
    struct_info := a.type.(*Type_Info_Struct);
    
    assert(struct_info.specified_parameters[0].name == "capacity");
    assert(struct_info.specified_parameters[0].type == xx int);
    capacity_cs_offset := struct_info.specified_parameters[0].offset_into_constant_storage;
    assert(capacity_cs_offset != -1);
    
    assert(struct_info.specified_parameters[1].name == "T");
    assert(struct_info.specified_parameters[1].type == xx Type);
    type_cs_offset := struct_info.specified_parameters[1].offset_into_constant_storage;
    assert(type_cs_offset != -1);
    
    return a.value_pointer.(*int), 
           a.value_pointer + size_of(int),
           struct_info.constant_storage[capacity_cs_offset].(*int).*,
           struct_info.constant_storage[type_cs_offset].(*Type).*.(*Type_Info);
}

is_static_array :: (a: Any) -> bool { return is_static_array(a.type); }
is_static_array :: (ti: *Type_Info) -> bool {
    return ti.type == .STRUCT && ti.(*Type_Info_Struct).polymorph_source_struct == type_info(Static_String(0)).polymorph_source_struct;
}
