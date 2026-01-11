
// ========== General Purpose ==========

// TODO: need to make all these procs that check if we are certain type of type use get_variant_base_type

as_type :: inline (ti: *Type_Info) -> Type { return (*ti).(*Type).*; }

is_numeric_type :: inline (T: Type) -> bool { return is_numeric_type(T.(*Type_Info)); }
is_numeric_type :: inline (type: *Type_Info) -> bool {
    if type == null  return false; // TODO: check if anything depends on this, then remove
    
    type = get_variant_base_type(type);
    while type.type == .VARIANT  type = type.(*Type_Info_Variant).variant_of;
    return type.type == .INTEGER || type.type == .FLOAT || type.type == .ENUM;
}

is_union_type :: inline (T: Type) -> bool { return is_union_type(T.(*Type_Info)); }
is_union_type :: (type: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    return type.type == .STRUCT && (type.(*Type_Info_Struct).textual_flags & .UNION);
}

is_tagged_union_type :: inline (T: Type) -> bool { return is_tagged_union_type(T.(*Type_Info)); }
is_tagged_union_type :: (type: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    return type.type == .STRUCT && (type.(*Type_Info_Struct).textual_flags & (.UNION | .UNION_IS_TAGGED) == (.UNION | .UNION_IS_TAGGED));
}

is_aggregate :: inline (T: Type) -> bool { return is_aggregate(T.(*Type_Info)); }
is_aggregate :: inline (type: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    if type.type == {
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
is_enum_flags :: inline (type: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    return type && type.type == .ENUM && type.(*Type_Info_Enum).enum_type_flags & .FLAGS == .FLAGS;
}

is_pointer_to :: inline (type: *Type_Info, target: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    return type != null && type.type == .POINTER && type.(*Type_Info_Pointer).pointer_to == target;
}

get_variant_base_type :: inline (T: Type) -> Type { return as_type(get_variant_base_type(T.(*Type_Info))); }
get_variant_base_type :: inline (type: *Type_Info) -> *Type_Info {
    while type.type == .VARIANT  type = type.(*Type_Info_Variant).variant_of;
    return type;
}


// two types are considered equivalent if we can memcpy between the two types validly.
are_types_equivalent :: (t1: *Type_Info, t2: *Type_Info) -> bool {
    t1 := get_variant_base_type(t1);
    t2 := get_variant_base_type(t2);
    
    if t1.type         != t2.type          return false;
    if t1.runtime_size != t2.runtime_size  return false;
    
    if t1.type == {
      case .STRUCT;
        return are_structs_equivalent(xx t1, xx t2);
        
      case .ARRAY;
        t1_array := t1.(*Type_Info_Array);
        t2_array := t2.(*Type_Info_Array);
        return t1_array.array_type == t2_array.array_type && are_types_equivalent(t1_array.element_type, t2_array.element_type);
        
      case .ENUM;
        // on what criteria are two enums equivalent?
        // based only on internal type?
        // would we want to do a more structural check here?
        t1_enum := t1.(*Type_Info_Enum);
        t2_enum := t2.(*Type_Info_Enum);
        return ti1_enum.internal_type.signed == ti2_enum.internal_type.signed; 
        
      case .INTEGER;
        t1_integer := t1.(*Type_Info_Integer);
        t2_integer := t2.(*Type_Info_Integer);
        return t1_integer.signed == t2_integer.signed; 
        
      case .FLOAT;
        // we already checked that runtime size matches
        return true; 
    }
    
    // TODO: should we make this unreachable?
    return false;
}

// ========== Enums ==========

enum_name_to_value :: (enum_info: *Type_Info_Enum, name: string) -> s64, bool {
    assert(enum_info.type == .ENUM);
    for enum_info.names  if name == it  return enum_info.values[it_index], true;
    return 0, false;
}

enum_value_to_name :: (enum_info: *Type_Info_Enum, value: s64) -> string {
    assert(enum_info.type == .ENUM);
    // assert(enum_info.enum_type_flags & .FLAGS == 0);
    for enum_info.values  if value == it  return enum_info.names[it_index];
    return "";
}

get_enum_valid_range :: (type: Type) -> (min: int, max: int) {
    enum_info := type.(*Type_Info_Enum);
    assert(enum_info.type == .ENUM);
    assert(enum_info.enum_type_flags & .FLAGS == 0);
    
    min := S64_MAX;
    max := S64_MIN;
    for enum_info.values {
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
    
    // If it is a tagged union, skip the first member (as this will be the tag)
    members := union_info.members;
    if is_tagged_union_type(union_info) {
        members.data  += 1;
        members.count -= 1; 
    }
    
    for members  if it.type == type  return true;
    return false;
}

get_union_tag_type :: (union_info: *Type_Info_Struct) -> *Type_Info {
    assert(is_tagged_union_type(union_info), "union_info did not refer to a tagged union type!");
    return union_info.members[0].type;
}

get_union_tag :: (any_union: Any) -> Any {
    assert(is_tagged_union_type(any_union.type), "any_union was not a tagged union type!");
    union_info := any_union.type.(*Type_Info_Struct);
    union_tag_member_info := *union_info.members[0];
    return Any.{ union_tag_member_info.type, any_union.value_pointer + union_tag_member_info.offset_in_bytes };
}

// NOTE: If the union is using a Type as tag, then the Any returned may be null!
get_valid_union_member :: (any_union: Any) -> Any, *Type_Info_Struct_Member {
    union_info := any_union.type.(*Type_Info_Struct);
    valid_member_info: *Type_Info_Struct_Member;
    
    tag := get_union_tag(any_union); // will assert that any_union is actually a tagged union
    if tag.type.type == {
      case .INTEGER;
        // NOTE: I am ignoring signedness here since the tag should presumably never have a negative value.
        kind_as_u64: u64;
        if tag.type.runtime_size == {
          case 1; kind_as_u64 = xx,no_check tag.value_pointer.(*u\8).*;
          case 2; kind_as_u64 = xx,no_check tag.value_pointer.(*u16).*;
          case 4; kind_as_u64 = xx,no_check tag.value_pointer.(*u32).*;
          case 8; kind_as_u64 = xx,no_check tag.value_pointer.(*u64).*;
        }
        valid_member_info = *union_info.members[kind_as_u64+1];
        
      case .ENUM;
        // NOTE: We will hopefully have better ability to map enum values to union members in the future, which will require new logic here...
        // NOTE: I am ignoring signedness here since the tag should presumably never have a negative value.
        kind_as_u64: u64;
        if tag.type.runtime_size == {
          case 1; kind_as_u64 = xx,no_check tag.value_pointer.(*u\8).*;
          case 2; kind_as_u64 = xx,no_check tag.value_pointer.(*u16).*;
          case 4; kind_as_u64 = xx,no_check tag.value_pointer.(*u32).*;
          case 8; kind_as_u64 = xx,no_check tag.value_pointer.(*u64).*;
        }
        valid_member_info = *union_info.members[kind_as_u64+1];
        
      case .TYPE;
        valid_type  := tag.value_pointer.(**Type_Info).*;
        struct_info := any_union.type.(*Type_Info_Struct);
        for *struct_info.members {
            if it.type == valid_type {
                valid_member_info = it;
                break;
            }
        }
        
      case;
        assert(false, "Invalid union tag type: %", tag.type.type);
    }
    
    // NOTE: only possible if union is using a Type tag
    if valid_member_info == null  return Any.{}, null;
    
    // TODO: Should it ever be constant? Need to check how Jai currently handles this situation.
    assert(valid_member_info.flags & .CONSTANT == 0);
    return Any.{ valid_member_info.type, any_union.value_pointer + valid_member_info.offset_in_bytes }, valid_member_info;
}

// NOTE: The semantics here are maybe a bit odd if the tag is not a Type.
//       We will just set the tag based on the index of the first member with a matching type.
set_union_from_any :: (any_union: Any, value: Any) -> bool {
    union_info := any_union.type.(*Type_Info_Struct);
    assert(is_union_type(union_info));
    
    is_tagged := is_tagged_union_type(union_info);
    
    members := union_info.members;
    if is_tagged {
        members.data  += 1;
        members.count -= 1; 
    }
    
    for *union_info.members {
        if it.type == value.type {
            member := Any.{ it.type, any_union.value_pointer + it.offset_in_bytes };
            memcpy_any(member, value);
            
            if is_tagged {
                tag := get_union_tag(any_union);
                if tag.type.type == {
                  case .TYPE;
                    memcpy_any(tag, member.type);
                    
                  case .INTEGER;
                  case .ENUM;
                    // NOTE: not exactly a typesafe operation here, but should be correct assuming no negative tags.
                    memcpy(tag.value_pointer, *it_index, tag.type.runtime_size);
                }
            }
            
            return true;
        }
    }
    return false;
}
