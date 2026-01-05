
// ========== General Purpose ==========

as_type :: inline (ti: *Type_Info) -> Type { return (*ti).(*Type).*; }

is_numeric_type :: inline (T: Type) -> bool { return is_numeric_type(T.(*Type_Info)); }
is_numeric_type :: inline (ti: *Type_Info) -> bool {
    if ti == null  return false;
    while ti.type == .VARIANT  ti = ti.(*Type_Info_Variant).variant_of;
    return ti.type == .INTEGER || ti.type == .FLOAT || ti.type == .ENUM;
}

is_union_type :: inline (T: Type) -> bool { return is_union_type(T.(*Type_Info)); }
is_union_type :: (ti: *Type_Info) -> bool {
    return ti.type == .STRUCT && (ti.(*Type_Info_Struct).textual_flags & .UNION);
}

is_tagged_union_type :: inline (T: Type) -> bool { return is_tagged_union_type(T.(*Type_Info)); }
is_tagged_union_type :: (ti: *Type_Info) -> bool {
    return ti.type == .STRUCT && (ti.(*Type_Info_Struct).textual_flags & (.UNION | UNION_IS_TAGGED) == (.UNION | UNION_IS_TAGGED));
}

is_aggregate :: inline (T: Type) -> bool { return is_aggregate(T.(*Type_Info)); }
is_aggregate :: inline (ti: *Type_Info) -> bool {
    if ti.type == {
        case .INTEGER;   return false;
        case .FLOAT;     return false;
        case .ENUM;      return false;
        case .BOOL;      return false;
        case .POINTER;   return false;
        case .PROCEDURE; return false;
        case .TYPE;      return false;
    }
    return true;
}

is_enum_flags :: inline (T: Type) -> bool { return is_enum_flags(T.(*Type_Info)); }
is_enum_flags :: inline (ti: *Type_Info) -> bool {
    return ti && ti.type == .ENUM && ti.(*Type_Info_Enum).enum_type_flags & .FLAGS == .FLAGS;
}

is_pointer_to :: inline (type: *Type_Info, target: *Type_Info) -> bool {
    return type != null && type.type == .POINTER && type.(*Type_Info_Pointer).pointer_to == target;
}

get_variant_base_type :: inline (T: Type) -> Type { return as_type(get_variant_base_type(T.(*Type_Info))); }
get_variant_base_type :: inline (ti: *Type_Info) -> *Type_Info {
    while ti.type == .VARIANT  ti = ti.(*Type_Info_Variant).variant_of;
    return ti;
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
        return ti1_enum.internal_type.signed == ti2_enum.internal_type.signed; 
        
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

get_union_kind_type :: (union_info: *Type_Info_Struct) -> *Type_Info {
    assert(is_tagged_union_type(union_info), "any_union was not a tagged union type!");
    assert(union_info.members[0].name == "kind", "tagged union did not contain 'kind' as its first member!"); // I assume this is always the case. If this is not true then we will need to search through the members to find kind.
    return union_info.members[0].type;
}

get_union_kind :: (any_union: Any) -> Any {
    assert(is_tagged_union_type(any_union.type), "any_union was not a tagged union type!");
    
    union_info := any_union.type.(*Type_Info_Struct);
    assert(union_info.members[0].name == "kind", "tagged union did not contain 'kind' as its first member!"); // I assume this is always the case. If this is not true then we will need to search through the members to find kind.
    union_kind_member_info := *union_info.members[0];
    
    return Any.{ union_kind_member_info.type, any_union.value_pointer + union_kind_member_info.offset_in_bytes };
}

get_valid_union_member :: (any_union: Any) -> Any {
    kind := get_union_kind(any_union); // will assert that any_union is actually a tagged union
    if kind.type.type == {
      case .INTEGER;
        // TODO
        assert(false, "union with kind: Type is not yet supported");
        
      case .ENUM;
        // NOTE: I am assuming here that negative values are not premitted in a union kine enum. Need to check this.
        // Also, we will hoppefully have better ability to map enum values to union members in the future, which will require new logic here...
        // kind_as_u64: u64;
        // if 
        // valid_member_info := union_info.members[kind_as_u64];
        
      case .TYPE;
        // TODO
        assert(false, "union with kind: Type is not yet supported");
    }
    
    assert(false, "invalid union kind: %", kind.type.type);
    return .{};
}
