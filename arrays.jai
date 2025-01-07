
// NOTE: use backtick in body if you want to refer to things from caller's scope
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


