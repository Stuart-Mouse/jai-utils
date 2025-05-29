
// ========== General Purpose ==========

as_type :: inline (ti: *Type_Info) -> Type { return (*ti).(*Type).*; }

is_numeric_type :: inline (T: Type) -> bool {
    return is_numeric_type(T.(*Type_Info));
}

is_numeric_type :: inline (ti: *Type_Info) -> bool {
    return ti && (ti.type == .INTEGER || ti.type == .FLOAT);
}

is_union_type :: (ti: *Type_Info) -> bool {
    return ti.type == .STRUCT && (ti.(*Type_Info_Struct).textual_flags & .UNION);
}

// two types are considered equivalent if we can memcpy between the two types validly.
are_types_equivalent :: (ti1: *Type_Info, ti2: *Type_Info) -> bool {
    if ti1.type != ti2.type  return false;
    if ti1.runtime_size != ti2.runtime_size  return false;
    if ti1.type == {
      case .STRUCT;
        return are_structs_equivalent(xx ti1, xx ti2);
        
      case .ARRAY;
        ti1_array := ti1.(*Type_Info_Array);
        ti2_array := ti2.(*Type_Info_Array);
        return ti1_array.array_type == ti2_array.array_type && are_types_equivalent(ti1_array.element_type, ti2_array.element_type);
        
      case .ENUM;
        // on what criteria are two enums equivalent?
        // based only on internal type?
        // would we want to do a more structural check here?
        ti1_enum := ti1.(*Type_Info_Enum);
        ti2_enum := ti2.(*Type_Info_Enum);
        return  ti1_enum.internal_type.signed == ti2_enum.internal_type.signed; 
        
      case .INTEGER;
        ti1_integer := ti1.(*Type_Info_Integer);
        ti2_integer := ti2.(*Type_Info_Integer);
        return ti1_integer.signed == ti2_integer.signed; 
        
      case .FLOAT;
        // we already checked that runtime size matches
        return true; 
    }
}

// ========== Enums ==========

enum_name_to_value :: (ti: *Type_Info_Enum, name: string) -> s64, bool {
    assert(ti.type == .ENUM);
    for ti.names  if name == it  return ti.values[it_index], true;
    return 0, false;
}

enum_value_to_name :: (ti: *Type_Info_Enum, value: s64) -> string {
    assert(ti.type == .ENUM);
    for ti.values  if value == it  return ti.names[it_index];
    return "";
}

enum_valid_range :: inline (t: Type) -> (min: int, max: int) {
    ti := t.(*Type_Info_Enum);
    assert(ti.type == .ENUM);
    min := S64_MAX;
    max := S64_MIN;
    for ti.values {
        if it < min  min = it;
        if it > max  max = it;
    }
    return min, max;
}

// ========== Structs ==========

are_structs_equivalent :: (ti1: *Type_Info_Struct, ti2: *Type_Info_Struct) -> bool {
    if ti1.runtime_size  != ti2.runtime_size   return false;
    if ti1.members.count != ti2.members.count  return false;
    
    // TODO: validate that all members have the exact same names and types
    // TODO: ignore constant members and #place members
    for ti1_member: ti1.members {
        ti2_member := ti2.members[it_index];
        if (ti1_member.flags & .CONSTANT) != (ti2_member.flags & .CONSTANT)  return false;
        if !are_types_equivalent(ti1_member.type, ti2_member.type)  return false;
    }
    
    return true;
}

// ========== Unions ==========

union_contains_type :: (union_info: *Type_Info_Struct, type: *Type_Info) -> bool {
    assert(is_union_type(union_info));
    for union_info.members 
        if it.type == type
            return true;
    return false;
}
