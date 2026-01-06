/*
    Any_Hash_Table.jai
    
    This acts as a sort of wrapper around the default Table from Hash_Table.jai that allows you to do all the essential 
    Table operations in a runtime-generic way. This should be helpful for those who want to support Tables in their 
    (de)serialization libraries or in other applications that are doing funky reflection stuff.
    
    
    NOTE:
    If there is no given_compare_function for the table, memcmp is used instead of operator == 
        This should not matter for primitive types, but if you are using #poke_name to insert operator == for other types, 
        then compare_keys will not do what you want it to do!
        Since #poke_name is going away anyhow, I see no reason to fix this. Just pass a given_compare_function.
    
    
    TODO: 
    Try to make sure this is independent of as much of my utils module stuff as possible.
        Looks like we will be taking a dependency on Many_Any, but maybe that's not dependent on anything else and we can just include that directly as another file for those who don't want to download the utils module.
    
*/


// This struct lines up with any Table, allowing us to access the values of the backing table through the Any_Table.
Table_Base :: struct {
    count:          int;
    allocated:      int;
    slots_filled:   int;
    allocator:      Allocator;
    entries:        Array_View_64;
    
    #if Hash_Table.COUNT_COLLISIONS {
        add\_collisions: s64;
        find_collisions: s64;
    }
    
    SIZE_MIN :: Dummy_Table.SIZE_MIN;
}

Table_Info :: struct {
    struct_info:            *Type_Info_Struct;
    
    key_type:               *Type_Info;
    value_type:             *Type_Info;
    entry_type:             *Type_Info_Struct;
    
    given_hash_function:    (Dummy_Table.Key_Type) -> u32;
    given_compare_function: (Dummy_Table.Key_Type, Dummy_Table.Key_Type) -> bool;
    
    load_factor_percent:    u32;
    refill_removed:         bool;
}

get_table_info :: (info: *Type_Info_Struct) -> Table_Info {
    Basic.assert(info.type == .STRUCT && info.polymorph_source_struct == type_info(Dummy_Table).polymorph_source_struct, "Type provided to Any_Table.from() was not a table!");
    
    using table_info: Table_Info;
    
    struct_info = info;
    
    key\ _type = (*info.constant_storage[info.specified_parameters[0].offset_into_constant_storage]).(**Type_Info).*;
    value_type = (*info.constant_storage[info.specified_parameters[1].offset_into_constant_storage]).(**Type_Info).*;
    entry_type = (*info.constant_storage[info.members[4].offset_into_constant_storage]).(**Type_Info_Struct).*;
    
    given_hash_function     = (*info.constant_storage[info.specified_parameters[2].offset_into_constant_storage]).(*type_of(given_hash_function)).*;
    given_compare_function  = (*info.constant_storage[info.specified_parameters[3].offset_into_constant_storage]).(*type_of(given_compare_function)).*;
    
    load_factor_percent = (*info.constant_storage[info.specified_parameters[4].offset_into_constant_storage]).(*type_of(load_factor_percent)).*;
    refill_removed      = (*info.constant_storage[info.specified_parameters[5].offset_into_constant_storage]).(*type_of(refill_removed)).*;
    
    return table_info;
}

Any_Table :: struct {
    using base: *Table_Base;
    using table_info: Table_Info;
    
    from :: (any: Any) -> Any_Table {
        table: Any_Table;
        table.table_info = get_table_info(xx any.type);
        table.base = xx any.value_pointer;
        return table;
    }
}

get_entries :: (table: Any_Table) -> Many_Any {
    return .{ 
        element_type = table.entry_type,
        array_view = table.entries,
    };
}

// also returns individual members of entry
get_entry :: (table: Any_Table, index: int) -> entry: Any, hash: *u32, key: Any, value: Any {
    Basic.assert(index >= 0 && index < table.entries.count);
    entry := Any.{ table.entry_type, table.entries.data + index * table.entry_type.runtime_size };
    return entry, 
        entry.value_pointer.(*u32),
        Any.{ table.entry_type.members[1].type, entry.value_pointer + table.entry_type.members[1].offset_in_bytes },
        Any.{ table.entry_type.members[2].type, entry.value_pointer + table.entry_type.members[2].offset_in_bytes };
}

/*
    I think this will turn out to be a nice pattern for using the Many_Any, 
    especially for cases like the entires array here, which is a polymorphic type,
    but we know the meta-level structure of it and can provide iterators for the individual components.
    
    In theory, we could also export some helper macros for doing things like getting/setting the individual members.
    And maybe we can even have an extra special macro that generates all this kind of stuff automatically.
    Not doing that here, of course, but it seems like an area worth exploring in the future...
    
    Instead of exporting several non-standard iterators, we could generate a struct to use as the iterator like so:
    
    Any_Entry :: struct {
        hash:   *u32;
        key:    Any;
        value:  Any;
    }
    
    The only issue is that we probably also want to give the user the entry itself as an Any.
    So we will either need to put the base element inside the iterator struct 
        (which could potentially be a name conflict if we are generating the struct), 
        or we could use a third iterator like it_members or something.
        Maybe we could use one of the for flags to denote that the user wants or doesn't want the it_member, too since it does add some slight overhead.
    
    We may also want to make something like the Many_Any, but one that is polymorphic so we can store the iterator type there instead of doing all the code gen in the for expansion.
*/
iterate_table_entries :: (entries: Many_Any, body: Code, flags: For_Flags) #expand {
    entry_type := entries.element_type.(*Type_Info_Struct);
    
    `it         := Any.{ entries.element_type, entries.data };
    `it_index   := 0;
    
    `it_hash    := it.value_pointer.(*u32);
    
    // Best we can do for key and value is give you an Any, so at least you have the types and value pointers readily available, and don't have to get the offset manually.
    // Most likely you'll just be passing them to another function that accepts and Any, anyhow.
    `it_key     := Any.{ entry_type.members[1].type, it.value_pointer + entry_type.members[1].offset_in_bytes };
    `it_value   := Any.{ entry_type.members[2].type, it.value_pointer + entry_type.members[2].offset_in_bytes };
    
    while it_index < entries.count {
        defer {
            it_index                += 1;
            it.value_pointer        += it.type.runtime_size;
            it_hash                  = it_hash.(*void) + it.type.runtime_size;
            it_key.value_pointer    += it.type.runtime_size;
            it_value.value_pointer  += it.type.runtime_size;
        }
        
        #insert body;
    }
}

get_key_hash :: (using table: Any_Table, key: Any) -> u32 {
    Basic.assert(key.type == key_type);
    
    if given_hash_function {
        if is_aggregate(key.type) {
            // If the key type is an aggregate (struct or array type), then we just pass the value_pointer directly.
            // This relies on the fact that 'maybe-by-reference' actually means 'always-by-reference' for aggregate types.
            // NOTE: If that were to change in the future, we would have a problem here!
            return given_hash_function(key.value_pointer);
        }
        
        // If the key type is a primitive, then we need to pass it by value.
        // We also recast the function pointer just to be safe...
        hash: u32;
        if key.type.runtime_size == {
            case 1; hash = given_hash_function.((u\8) -> u32)(key.value_pointer.(*u\8).*);
            case 2; hash = given_hash_function.((u16) -> u32)(key.value_pointer.(*u16).*);
            case 4; hash = given_hash_function.((u32) -> u32)(key.value_pointer.(*u32).*);
            case 8; hash = given_hash_function.((u64) -> u32)(key.value_pointer.(*u64).*);
        }
        return hash;
    }
    
    // TODO: should also support array type keys, I suppose...
    
    // if we have no hash function, then we fallback to the default for the given type
    Basic.assert(ifx 1 { 
        ti := key_type;
        while ti.type == .VARIANT  ti = ti.(*Type_Info_Variant).variant_of;
        ti.type == .INTEGER || ti.type == .BOOL || ti.type == .ENUM || ti.type == .POINTER || ti.type == .FLOAT || ti.type == .STRING || ti.type == .ARRAY;
    }, "Unable to use default hash for type: %", as_type(key_type));
    
    h := HASH_INIT;
    
    if key_type.type == {
      case .ARRAY;
        array := Any_Array.from(key);
        count, data := get_count_and_data(array);
        return inline Hash.sdbm_hash(data, count * array.element_type.runtime_size, h);
        
      case .STRING;
        s := key.value_pointer.(*string).*;
        return cast,trunc(u32) inline Hash.fnv1a_hash(s.data, s.count, h);
        
      case .FLOAT;
        return inline Hash.sdbm_hash(key.value_pointer, key.type.runtime_size, h);
      
      case;
        x: u64;
        memcpy(*x, key.value_pointer, key.type.runtime_size);
        return (Hash.knuth_hash(x ^ h) >> 32).(u32);
    }
}

compare_keys :: (table: Any_Table, a: Any, b: Any) -> bool {
    assert(a.type == table.key_type && b.type == table.key_type);
    
    if table.given_compare_function {
        if is_aggregate(table.key_type) {
            return table.given_compare_function(a.value_pointer, b.value_pointer);
        }
        
        result: bool;
        if table.key_type.runtime_size == {
            case 1; result = table.given_compare_function.((u\8, u\8) -> bool)(a.value_pointer.(*u\8).*, b.value_pointer.(*u\8).*);
            case 2; result = table.given_compare_function.((u16, u16) -> bool)(a.value_pointer.(*u16).*, b.value_pointer.(*u16).*);
            case 4; result = table.given_compare_function.((u32, u32) -> bool)(a.value_pointer.(*u32).*, b.value_pointer.(*u32).*);
            case 8; result = table.given_compare_function.((u64, u64) -> bool)(a.value_pointer.(*u64).*, b.value_pointer.(*u64).*);
        }
        return result;
    }
    
    return memcmp(a.value_pointer, b.value_pointer, table.key_type.runtime_size) == 0;
}

expand :: (table: *Any_Table) {
    old_entries := get_entries(table); 
    
    new_allocated: s64 = ---;
    
    if (table.count * 2 + 1) * 100 < table.allocated * table.load_factor_percent {  // The *2 is to say, if we double the size, are we still small enough to fit into the current memory? The reason we are doing this test is, if we removed a bunch of elements, maybe we are full of REMOVED_HASH markers, and if we just rehash to the same size, we can get rid of those. An alternate version (simpler?) might be to check table.count vs table.slots_filled. Note that this becomes less necessary if REFILL_REMOVED is enabled.
        new_allocated = table.allocated;
    } else {
        new_allocated = table.allocated * 2;
    }
    
    if new_allocated < table.SIZE_MIN  new_allocated = table.SIZE_MIN;
    
    _internal_resize_memory(table, new_allocated);
    
    table.count        = 0;
    table.slots_filled = 0;
    
    for :iterate_table_entries old_entries {
        if it_hash.* >= Hash_Table.FIRST_VALID_HASH  table_add(table, it_key, it_value);
    }
    
    Basic.free(old_entries.data,, table.allocator);
}

table_add :: (table: *Any_Table, key: Any, value: Any) -> Any {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key  .type == table.key\ _type, "Incorrect key type provided to table_add! Expected: %, got %", table.key_type, key.type);
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_add!");
    Basic.assert(value.type == table.value_type, "Incorrect value type provided to table_add! Expected: %, got %", table.value_type, value.type);
    
    Basic.assert(table.load_factor_percent < 100);  // A 100% full table will infinite loop (and you will want to be substantially smaller than this for reasonable performance).
    
    if (table.slots_filled + 1) * 100 >= table.allocated * table.load_factor_percent  expand(table);
    
    Basic.assert(table.slots_filled < table.allocated);
    
    entries := get_entries(table);
    Walk_Any_Table(#code {
        if table.refill_removed {
            hash := entries[index].value_pointer.(*u32).*;
            if hash == Hash_Table.REMOVED_HASH {
                table.slots_filled -= 1;  // 1 will get re-added below, for total increment 0.
                break;
            }
        }
        #if Hash_Table.COUNT_COLLISIONS  table.add_collisions += 1;
    });
    
    table.count        += 1;
    table.slots_filled += 1;
    
    _, entry_hash, entry_key, entry_value := get_entry(table, index);
    entry_hash.* = hash;
    memcpy_any(entry_key, key);
    
    // NOTE: value.value_pointer may be null, since we want to allow 
    //       find_or_add to work without needing to provide a dummy value 
    //       (which would require either an allocation, or some other shenanigans).
    if value.value_pointer  memcpy_any(entry_value, value);
    
    return entry_value;
}

// NOTE: We don't have a table_find_pointer, since of course, we are always getting a pointer in the Any.
table_find :: (table: *Any_Table, key: Any) -> bool, Any {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_find!");
    Basic.assert(key.type == table.key_type, "Incorrect key type provided to table_find! Expected: %, got %", table.key_type, key.type);
    
    if !table.allocated  return false, Any.{};
    
    entries := get_entries(table);
    Walk_Any_Table(#code {
        _, entry_hash, entry_key, entry_value := get_entry(table, index);
        if entry_hash.* == hash {
            if compare_keys(table, entry_key, key)  return true, entry_value;
        }
        #if Hash_Table.COUNT_COLLISIONS  table.find_collisions += 1;
    });
    
    return false, Any.{};
}

table_find_multiple :: (table: *Any_Table, key: Any) -> [] Any {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_find!");
    Basic.assert(key.type == table.key_type, "Incorrect key type provided to table_find! Expected: %, got %", table.key_type, key.type);
    
    if !table.allocated  return .[];
    
    results: [..] Any;
    results.allocator = Basic.temporary_allocator;
    
    entries := get_entries(table);
    Walk_Any_Table(#code {
        _, entry_hash, entry_key, entry_value := get_entry(table, index);
        if entry_hash.* == hash {
            if compare_keys(table, entry_key, key) {
                Basic.array_add(*results, entry_value);
            } else {
                #if Hash_Table.COUNT_COLLISIONS  table.find_collisions += 1;
            }
        } else {
            #if Hash_Table.COUNT_COLLISIONS  table.find_collisions += 1;
        }
    });
    
    return results;
}

init :: (using table: *Any_Table, slots_to_allocate: s64 = 0) {
    Basic.remember_allocators(table.base);
    _internal_resize_memory(table, slots_to_allocate);
}

deinit :: (table: *Any_Table) {
    Basic.free(table.entries.data,, table.allocator);
}

table_reset :: (using table: *Any_Table) {
    count        = 0;
    slots_filled = 0;
    for :iterate_table_entries get_entries(table)  it_hash.* = 0;
}

table_set :: (table: *Any_Table, key: Any, value: Any) -> Any {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key.type == table.key_type, "Incorrect key type provided to table_set! Expected: %, got %", table.key_type, key.type);
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_set!");
    Basic.assert(value.type == table.value_type, "Incorrect value type provided to table_set! Expected: %, got %", table.value_type, value.type);
    
    found, value_found := table_find(table, key);
    if found {
        memcpy(value_found.value_pointer, value.value_pointer, table.value_type.runtime_size);
        return value_found;
    } else {
        return table_add(table, key, value);
    }
}

table_contains :: (table: *Any_Table, key: Any) -> bool {
    return table_find_pointer(table, key).type != null;
}

table_ensure_space :: (table: *Any_Table, items: s64) {
    if (table.slots_filled + items) * 100 >= table.allocated * table.LOAD_FACTOR_PERCENT  expand(table);
}

// NOTE: table_remove can't give you back the removed element for an Any_Table, because we have remove the backing storage.
//       In theory, we could just temp allocate a copy to return, but that feels like a bad design here.
//       Or, we could return a pointer to the element even though its slot has been marked as empty, and the user can just understand that doing any other table operations will invalidate the returned Any.
//       I dunno man, maybe if someone actually uses this code they can figure out what solution works for them there.
table_remove :: (table: *Any_Table, key: Any) -> bool {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key.type == table.key_type, "Incorrect key type provided to table_remove! Expected: %, got %", table.key_type, key.type);
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_remove!");
    
    if !table.allocated  return false;
    
    entries := get_entries(table);
    Walk_Any_Table(#code {
        _, entry_hash, entry_key, entry_value := get_entry(table, index);
        if entry_hash.* == hash && compare_keys(table, entry_key, key) {
            entry_hash.* = Hash_Table.REMOVED_HASH;  // No valid entry will ever hash to REMOVED_HASH.
            table.count -= 1;
            return true;
        }
    });
    
    return false;
}

table_remove_multiple :: (table: *Any_Table, key: Any) -> (num_removed: s64) {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key.type == table.key_type, "Incorrect key type provided to table_remove! Expected: %, got %", table.key_type, key.type);
    Basic.assert(key.value_pointer != null, "Invalid (null) key provided to table_remove!");
    
    if !table.allocated  return 0;
    
    orig_count := table.count;
    
    entries := get_entries(table);
    Walk_Any_Table(#code {
        _, entry_hash, entry_key, entry_value := get_entry(table, index);
        if entry_hash.* == hash && compare_keys(table, entry_key, key) {
            entry_hash.* = Hash_Table.REMOVED_HASH;  // No valid entry will ever hash to REMOVED_HASH.
            table.count -= 1;
        }
    });
    
    return orig_count - table.count;
}


find_or_add :: (table: *Any_Table, key: Any) -> (entry: Any, newly_added: bool) {
    found, value := table_find(table, key);
    if value return value, false;
    
    value = table_add(table, key, Any.{ type = table.value_type });
    return value, true;
}

// TODO: do we care to implement the _multiple procs?

#scope_file

Basic :: #import "Basic";
Hash :: #import "Hash";
Hash_Table :: #import "Hash_Table";

// TODO: remove dependency as much as possible 
#import "Utils";

// This is #scope_file in Hash for some reason, so I had to duplicate it here...
// I assume if this changes and the two get out of sync then things will break, so that's nice.
HASH_INIT : u32 : 5381;

Dummy_Table :: Hash_Table.Table(*void, *void);


/*
    NOTE: I am keeping this as close to the original Walk_Any_Table as possible, even though it is kinda weird, tbh.
    I would probably rather use a named for_expansion for this, but oh well. Maybe I'll do that later if it seems worthwhile.
    The only major change to the interface here is that we also need `entries` to be defined in the calling scope using get_entries(table)
    This is because in 90% of situations, the user caller will want to refer to this anyhow, and we don't need to repeat the work required by get_entries()
*/
Walk_Any_Table :: (code: Code) #expand {
    mask := cast,trunc(u32)(`table.allocated - 1);
    
    `hash := get_key_hash(`table, `key);
    if hash < Hash_Table.FIRST_VALID_HASH  hash += Hash_Table.FIRST_VALID_HASH;
    
    `index := hash & mask;
    
    probe_increment: u32 = 1;
    
    while `table_while_loop := true {
        hash := `entries[index].value_pointer.(*u32).*;
        if hash == 0  break;
        
        #insert code;
        
        index = (index + probe_increment) & mask;    // Since table.allocated is always power of two, the & wraps our index within the table. (Requires that probe_increment is not big enough to wrap index into negative numbers, which it won't be because it's u32 and index is s64.);
        probe_increment += 1;
    }
}

_internal_resize_memory :: (table: *Any_Table, slots_to_allocate: int) {
    if slots_to_allocate == 0 slots_to_allocate = table.SIZE_MIN;
    n := Hash_Table.next_power_of_two(slots_to_allocate);
    table.allocated = n;
    
    new_entries := New_Many_Any(n, table.entry_type, true,, table.allocator);
    
    for :iterate_table_entries new_entries {
        it_hash.* = 0;
    }
    table.entries = new_entries.array_view;
}
