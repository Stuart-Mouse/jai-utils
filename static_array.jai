
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
