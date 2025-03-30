
as_type :: inline (ti: *Type_Info) -> Type { return (cast(*Type)*ti).*; }

is_numeric_type :: (t: Type) -> bool {
    ti := cast(*Type_Info)t;
    return ti.type == .INTEGER || ti.type == .FLOAT;
}

is_numeric_type :: (ti: *Type_Info) -> bool {
    return ti.type == .INTEGER || ti.type == .FLOAT;
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

are_structs_equivalent :: (ti1: *Type_Info_Struct, ti2: *Type_Info_Struct) -> bool {
    assert(ti1.type == .STRUCT);
    assert(ti2.type == .STRUCT);
    
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
