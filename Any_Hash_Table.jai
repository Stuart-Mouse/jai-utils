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
    
    #if HT.COUNT_COLLISIONS {
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
    
    Basic.log("given_hash_function: %", given_hash_function);
    Basic.log("given_compare_function: %", given_compare_function);
    Basic.log("given_hash_function offset: %", info.specified_parameters[2].offset_into_constant_storage);
    Basic.log("given_compare_function offset: %", info.specified_parameters[3].offset_into_constant_storage);
    
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
            it.value_pointer        += it.type.runtime_size;
            it_hash                 += it.type.runtime_size;
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
    
    // if we have no hash function, then we fallback to the default
    // usually this would be Hash.get_hash, which only accepts primitive data types
    // in order to copy the standard behaviour as closely as possible, that unfortunately means doing an assert here
    // Now, this should never actually be tripped since, I think, it would be impossible to instantiate a Table that would case this.
    // NOTE: logic for assert matches the #modify of get_hash
    Basic.assert(ifx 1 { 
        ti := key_type;
        while ti.type == .VARIANT  ti = ti.(*Type_Info_Variant).variant_of;
        ti.type == .INTEGER || ti.type == .BOOL || ti.type == .ENUM || ti.type == .POINTER || ti.type == .FLOAT || ti.type == .STRING;
    }, "Unable to use default hash for type: %", as_type(key_type));
    
    h := HASH_INIT;
    
    if key_type.type == {
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
    assert(a.type == key_type && b.type == key_type);
    
    if table.given_compare_function {
        if is_aggregate(table.key_type) {
            return given_compare_function(key.value_pointer, key.value_pointer);
        }
        
        result: bool;
        if table.key_type.runtime_size == {
            case 1; result = given_compare_function.((u\8, u\8) -> u32)(a.value_pointer.(*u\8).*, b.value_pointer.(*u\8).*);
            case 2; result = given_compare_function.((u16, u16) -> u32)(a.value_pointer.(*u16).*, b.value_pointer.(*u16).*);
            case 4; result = given_compare_function.((u32, u32) -> u32)(a.value_pointer.(*u32).*, b.value_pointer.(*u32).*);
            case 8; result = given_compare_function.((u64, u64) -> u32)(a.value_pointer.(*u64).*, b.value_pointer.(*u64).*);
        }
        return result;
    }
    
    return memcmp(a.value_pointer, b.value_pointer, key_type.runtime_size) == 0;
}

expand :: (table: *Any_Table) {
    old_entries := get_entries(table); 
    
    slots_to_allocate: s64 = ---;
    
    if (table.count * 2 + 1) * 100 < table.allocated * table.load_factor_percent {  // The *2 is to say, if we double the size, are we still small enough to fit into the current memory? The reason we are doing this test is, if we removed a bunch of elements, maybe we are full of REMOVED_HASH markers, and if we just rehash to the same size, we can get rid of those. An alternate version (simpler?) might be to check table.count vs table.slots_filled. Note that this becomes less necessary if REFILL_REMOVED is enabled.
        slots_to_allocate = table.allocated;
    } else {
        slots_to_allocate = table.allocated * 2;
    }
    
    if slots_to_allocate < table.SIZE_MIN  slots_to_allocate = table.SIZE_MIN;
    
    // _internal_resize_memory(table, new_allocated);
    {
        if slots_to_allocate == 0 slots_to_allocate = table.SIZE_MIN;
        n := HT.next_power_of_two(slots_to_allocate);
        table.allocated = n;
        
        new_entries := New_Many_Any(n, table.entry_type, true,, table.allocator);
        for :iterate_table_entries  new_entries {
            it_hash.* = 0;
        }
        table.entries = new_entries.array_view;
    }
    
    table.count        = 0;
    table.slots_filled = 0;
    
    for :iterate_table_entries old_entries {
        if it_hash.* >= FIRST_VALID_HASH  table_add(table, it_key, it_value);
    }
    
    Basic.free(old_entries.data,, table.allocator);
}

table_add :: (using table: *Any_Table, key: Any, value: Any) -> Any {
    Basic.assert(table.base != null && table.table_info.struct_info != null, "Invalid Any_Table in table_add");
    Basic.assert(key  .type == key\ _type, "Incorrect key type provided to table_add! Expected: %, got %", key_type, key.type);
    Basic.assert(value.type == value_type, "Incorrect value type provided to table_add! Expected: %, got %", value_type, value.type);
    
    Basic.assert(table.load_factor_percent < 100);  // A 100% full table will infinite loop (and you will want to be substantially smaller than this for reasonable performance).
    
    if (table.slots_filled + 1) * 100 >= table.allocated * table.load_factor_percent  expand(table);
    
    Basic.assert(table.slots_filled < table.allocated);
    
    // Walk_Table inlined here and modified to make it work
    // May factor this back out later, TBD
    hash:   u32;
    index:  u32;
    {
        mask := cast,trunc(u32)(table.allocated - 1);
        
        hash = get_key_hash(table, key);
        if hash < HT.FIRST_VALID_HASH  hash += HT.FIRST_VALID_HASH;
        
        index = hash & mask;
        
        probe_increment: u32 = 1;
        
        entries := get_entries(table);
        while true {
            entry := entries[index];
            hash  := entry.value_pointer.(*u32).*;
            if hash == 0  break;
            
            if table.refill_removed {
                if hash == REMOVED_HASH {
                    table.slots_filled -= 1;  // 1 will get re-added below, for total increment 0.
                    break;
                }
            }
            
            #if HT.COUNT_COLLISIONS  table.add_collisions += 1;
            
            index = (index + probe_increment) & mask;    // Since table.allocated is always power of two, the & wraps our index within the table. (Requires that probe_increment is not big enough to wrap index into negative numbers, which it won't be because it's u32 and index is s64.);
            probe_increment += 1;
        }
    }
    
    table.count        += 1;
    table.slots_filled += 1;
    
    _, entry_hash, entry_key, entry_value := get_entry(table, index);
    entry_hash.* = hash;
    memcpy_any(entry_key,   key);
    memcpy_any(entry_value, value);
    
    return entry_value;
}

#scope_file

Basic :: #import "Basic";
Hash :: #import "Hash";
using HT :: #import "Hash_Table";

// TODO: remove dependency as much as possible 
#import "Utils";

// This is #scope_file in Hash for some reason, so I had to duplicate it here...
// I assume if this changes and the two get out of sync then things will break, so that's nice.
HASH_INIT : u32 : 5381;

Dummy_Table :: HT.Table(*void, *void);

