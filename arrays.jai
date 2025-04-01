
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

// ========== Dynamic Array Allocation ==========

// For other dynamic array-related functions, look at the Any_Array in any.jai.

array_reserve_nonpoly :: (array: *Resizable_Array, desired_items: s64, elem_size: s64) {
    if desired_items <= array.allocated return;
    
    if !array.allocator.proc  remember_allocators(array);
    
    array.data = realloc(array.data, desired_items * elem_size, array.allocated * elem_size,, array.allocator);
    assert(array.data != null); // The program will die if we couldn't get memory. We can think about an alternative strategy, but it's hard to know what would be reasonable.
    
    array.allocated = desired_items;
}

array_add_any :: inline (array: Any) -> Any, bool {
    // verify that array is in fact an array
    if array.type.type != .ARRAY {
        log("Error: Array any is not an array!\n");
        return Any.{}, false;
    }
    ti_array := array.type.(*Type_Info_Array);
    
    any := array_add_any_nocheck(xx array.value_pointer, ti_array.element_type);
    return any, true;
}

// Technically, this proc will never return an invalid value, since the only possibe failure is failing to allocate memory, in which case we just crash.
array_add_any_nocheck :: (array: *Resizable_Array, elem_type: *Type_Info) -> Any {
    // maybe grow array
    if array.count >= array.allocated {
        reserve := 2 * array.allocated;
        if reserve < 8  reserve = 8;
        array_reserve_nonpoly(xx array, reserve, elem_type.runtime_size);
    }
    
    ret := Any.{ elem_type, array.data + (array.count * elem_type.runtime_size) };
    array.count += 1;
    
    return ret;
}

array_add_any_at_index :: (array_any: Any, index: int) -> Any {
    if array_any.value_pointer == null || array_any.type.type != .ARRAY 
        then return false;
    
    ti_array  := array_any.type.(*Type_Info_Array);
    raw_array := array_any.value_pointer.(*Resizable_Array);
    new_cap   := max(next_power_of_two(index + 1), 8);
    
    array_reserve_nonpoly(xx raw_array, new_cap, ti_array.element_type.runtime_size);
    
    raw_array.count = max(raw_array.count, index + 1);
    
    return Any.{ ti_array.element_type, raw_array.data + (index * ti_array.element_type.runtime_size) };
}

