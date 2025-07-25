
// ========== Array Search ==========

// NOTE: use backtick in body if you want to refer to things from caller's scope
//       body is inserted in scope so that you have access to it and it_index
array_find_if :: (array: []$T, body: Code, $reverse := false) -> *T #expand {
    for <=reverse *array {
        if #insert,scope() body {
            return it;
        }
    }
    return null;
}

array_find_where :: inline (array: []$T, $$cond: (T) -> bool, $reverse := false) -> *T {
    #if is_constant(cond) {
        for <=reverse *array  if inline cond(it)  return it;
    } else {
        for <=reverse *array  if cond(it)  return it;
    }
    return null;
}

array_find_where :: inline (array: []$T, $$cond: (T, P) -> bool, $$data: $P, $reverse := false) -> *T {
    #if is_constant(cond) {
        for <=reverse *array  if inline cond(it, data)  return it;
    } else {
        for <=reverse *array  if cond(it, data)  return it;
    }
    return null;
}

array_contains_any :: (array: []$T, elems: ..T) {
    for a: array for e: elems if a == e  return true;
}

array_contains_all :: (array: []$T, elems: ..T) {
    if a.count < e.count  return false;
    for e: elems {
        for a: array if a == e  continue e;
        return false;
    }
    return true;
}

// ========== Dynamic Array Allocation ==========

// For other dynamic array-related functions, look at the Any_Array in any.jai.

array_add_any :: inline (array: Any) -> Any {
    assert(array.type.type == .ARRAY);
    
    ti_array := array.type.(*Type_Info_Array);
    array    := array.value_pointer.(*Resizable_Array);
    
    // maybe grow array
    if array.count >= array.allocated {
        reserve := max(2 * array.allocated, 8);
        array_reserve(array, reserve, ti_array.element_type.runtime_size);
    }
     
    defer array.count += 1; // increment after return
    return Any.{ elem_type, array.data + (array.count * elem_type.runtime_size) };
}

array_add_any_at_index :: (array: Any, index: int) -> Any {
    assert(array.type.type == .ARRAY);
    if array.value_pointer == null || array.type.type != .ARRAY {
        return Any.{};
    }
    
    ti_array  := array.type.(*Type_Info_Array);
    raw_array := array.value_pointer.(*Resizable_Array);
    new_cap   := max(next_power_of_two(index + 1), 8);
    
    array_reserve(raw_array, new_cap, ti_array.element_type.runtime_size);
    
    raw_array.count = max(raw_array.count, index + 1);
    
    return Any.{ ti_array.element_type, raw_array.data + (index * ti_array.element_type.runtime_size) };
}


// this may be kind risky but oh well
make_resizable :: (array: [] $T, allocator := context.allocator) -> [..] T {
    ret: [..] T;
    ret.data      = array.data;
    ret.count     = array.count;
    ret.allocated = array.count;
    ret.allocator = allocator;
    return ret;
}