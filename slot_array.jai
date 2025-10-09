
// This is like a crappy low tech version of the bucket array that I had been using in Odin mario, should probably just use bucket array instead
// TODO: we can improve at least somewhat by using a bitfield for the occupied flags.
//       this will make it faster to find the first unoccupied slot, and will make the actual array elements packed properly
//       easier to compact down elements, track the count of currently occupied slots

Slot_Array :: struct(T: Type, capacity: int) {
    next:       int;
    occupied:   Fixed_Bit_Array(capacity);
    slots:      [capacity] T;
}

get_first_empty_slot :: (array: *Slot_Array, set_occupied := false) -> *array.T, int {
    for :only_set array.occupied {
        array.occupied[it_index] = set_occupied;
        return *array.slots[it_index], it_index; 
    }
    return null, -1;
}

get_next_empty_slot :: (using array: *Slot_Array, set_occupied := false) -> *array.T, int {
    for next..capacity-1  if !array.occupied[it] { 
        array.occupied[it] = set_occupied;
        array.next = it + 1;
        return *slots[it], it; 
    }
    for 0..next-1  if !array.occupied[it] { 
        array.occupied[it] = set_occupied;
        array.next = it + 1;
        return *slots[it], it; 
    }
    return null, -1;
}

get_next_slot :: (array: *Slot_Array, set_occupied := false) -> *array.T, int {
    array.occupied[array.next] = ifx set_occupied then true else array.occupied[array.next];
    index := array.next;
    slot  := *array.slots[array.next];
    array.next = (array.next + 1) % array.capacity;
    return slot, index;
}

// TODO: maybe also check the alignment of the element
get_element_index :: (array: *Slot_Array, element: *array.T) -> bool, int {
    if (element < array.slots.data || element >= array.slots.data + array.capacity) return false, -1;
    return true, (element.(s64) - array.slots.data.(s64)) / size_of(array.T);
}

is_valid_index :: inline (array: Slot_Array, index: int) -> bool {
    return index >= 0 && index < array.capacity;
}

index_is_occupied :: inline (array: Slot_Array, index: int) -> bool {
    return is_valid_index(array, index) && array.occupied[index];
}

// TODO: optimize to just count set bits manually
//       may need to make sure out-of-bounds bits at end of last slot are properly zeroed
get_count_occupied :: (array: Slot_Array) -> int {
    count := 0;
    for :only_set array.occupied {
        count += 1;
    }
    return count;
}

set_index_occupied :: inline (array: *Slot_Array, index: int, $$occupied: bool) {
    array.occupied[index] = occupied;
}

set_index_occupied :: inline (array: *Slot_Array, element: *array.T, $$occupied: bool) {
    ok, index := get_element_index(array, element);
    if ok  set_index_occupied(array, index, occupied);
}

for_expansion :: (array: *Slot_Array, body: Code, flags: For_Flags) #expand {
    REVERSE    :: (flags & .REVERSE).(bool);
    DO_POINTER :: (flags & .POINTER).(bool);
    
    #assert(!REVERSE);
    
    for :only_set _, `it_index: array.occupied {
        `it := #ifx DO_POINTER then *array.slots[it_index] else array.slots[it_index];
        #insert (remove={array.occupied[it_index] = false;}) body;
    }
}

operator[] :: (array: *Slot_Array, index: int) -> array.T {
    return array.slots[index];
}

operator*[] :: (array: *Slot_Array, index: int) -> *array.T {
    return *array.slots[index];
}

clone_to_view :: (array: Slot_Array) -> [] array.T {
    count := get_count_occupied(array);
    view  := NewArray(count, array.T, initialized = false); 
    index := 0;
    for array {
        view[index] = it;
        index += 1;
    }
    return view;
}

clone_from_view :: (array: *Slot_Array, view: [] array.T) -> bool {
    if array.capacity < view.count  return false;
    for view  get_next_slot(array, true).* = it;
    return true;
}
