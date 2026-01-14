
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
is_union_type :: inline (type: *Type_Info) -> bool {
    type = get_variant_base_type(type);
    return type.type == .STRUCT && (type.(*Type_Info_Struct).textual_flags & .UNION);
}

is_tagged_union_type :: inline (T: Type) -> bool { return is_tagged_union_type(T.(*Type_Info)); }
is_tagged_union_type :: inline (type: *Type_Info) -> bool {
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


// Two types are considered equivalent if we can memcpy between the two types validly.
Type_Comparison_Flags :: enum_flags {
    COMPARE_VARIANTS;
    COMPARE_STRUCTS;
    COMPARE_PROCEDURES;     // TODO
    COMPARE_ENUMS;          // TODO
}

DEFAULT_TYPE_COMPARISON_FLAGS :: Type_Comparison_Flags.COMPARE_VARIANTS;

are_types_equivalent :: (t1: *Type_Info, t2: *Type_Info, flags := DEFAULT_TYPE_COMPARISON_FLAGS) -> bool {
    if t1 == t2  return true;
    
    t1 := get_variant_base_type(t1);
    t2 := get_variant_base_type(t2);
    if t1 == t2  return true;
    
    if t1.type         != t2.type          return false;
    if t1.runtime_size != t2.runtime_size  return false;
    
    if #complete t1.type == {
      case .UNTYPED_LITERAL;        #through;
      case .UNTYPED_ENUM;           #through;
      case .OVERLOAD_SET;           #through;
      case .POLYMORPHIC_VARIABLE;   #through;
      case .VARIANT;
        unreachable();
        
      case .CODE;   #through;
      case .BOOL;   #through;
      case .TYPE;   #through;
      case .STRING; #through;
      case .ANY;    #through;
      case .VOID;
        return true;
        
      case .PROCEDURE;
        // TODO: check type signature?
        return false;
        
      case .STRUCT;
        return compare_structs && are_structs_equivalent(xx t1, xx t2);
        
      case .ARRAY;
        t1_array := t1.(*Type_Info_Array);
        t2_array := t2.(*Type_Info_Array);
        return t1_array.array_type == t2_array.array_type && are_types_equivalent(t1_array.element_type, t2_array.element_type);
        
      case .ENUM;
        // TODO: on what criteria are two enums equivalent?
        // based only on internal type?
        // would we want to do a more structural check here?
        // t1_enum := t1.(*Type_Info_Enum);
        // t2_enum := t2.(*Type_Info_Enum);
        // return ti1_enum.internal_type.signed == ti2_enum.internal_type.signed; 
        return false;
        
      case .INTEGER;
        t1_integer := t1.(*Type_Info_Integer);
        t2_integer := t2.(*Type_Info_Integer);
        return t1_integer.signed == t2_integer.signed; 
        
      case .FLOAT;
        // we already checked that runtime size matches
        return true; 
        
      case .POINTER;
        t1_pointer := t1.(*Type_Info_Pointer);
        t2_pointer := t2.(*Type_Info_Pointer);
        return are_types_equivalent(t1_pointer.pointer_to, t2_pointer.pointer_to);
    }
    
    return false;
}

// ========== Enums ==========

enum_name_to_value :: (type: *Type_Info, name: string) -> s64, bool {
    enum_info := assert_base_type(type, .ENUM);
    for enum_info.names  if name == it  return enum_info.values[it_index], true;
    return 0, false;
}

enum_value_to_name :: (type: *Type_Info, value: s64) -> string {
    enum_info := assert_base_type(type, .ENUM);
    // assert(enum_info.enum_type_flags & .FLAGS == 0);
    for enum_info.values  if value == it  return enum_info.names[it_index];
    return "";
}

get_enum_valid_range :: (type: *Type_Info) -> (min: int, max: int) {
    enum_info := assert_base_type(type, .ENUM);
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

is_union :: inline (struct_info: *Type_Info_Struct) -> bool {
    return struct_info.textual_flags & .UNION == .UNION;
}

is_tagged_union :: inline (struct_info: *Type_Info_Struct) -> bool {
    return struct_info.textual_flags & (.UNION | .UNION_IS_TAGGED) == (.UNION | .UNION_IS_TAGGED);
}

get_member_info :: (struct_info: *Type_Info_Struct, name: string, recursive := false) -> (member_info: *Type_Info_Struct_Member, member_offset: int) {
    member_info:    *Type_Info_Struct_Member;
    member_offset:  int;
    
    for *struct_info.members {
        if it.name == name {
            member_info   = it;
            member_offset = it.offset_in_bytes;
            break;
        }
    }
    
    if recursive && member_info == null {
        for *struct_info.members {
            // TODO: is it possible to have a using on a constant member?
            // TODO: redundant condition here? it.type.type == .STRUCT
            if it.flags & .USING {
                base_type := get_variant_base_type(it.type);
                if base_type.type != .STRUCT  continue;
                
                member_info, member_offset = get_member_info(xx base_type, name, true);
                if member_info {
                    // NOTE: member_offset is ignored if member is constant, so this is ok to do unconditionally
                    member_offset += it.offset_in_bytes; 
                    break;
                }
            }
        }
    }
    
    if member_info == null            return null, 0;
    if member_info.flags & .CONSTANT  return member_info, -1;
    return member_info, member_offset;
}

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

union_contains_type :: (union_type: *Type_Info, member_type: *Type_Info) -> bool {
    union_info := assert_base_type(union_type);
    assert(is_union(union_info));
    
    // If it is a tagged union, skip the first member (as this will be the tag)
    members := union_info.members;
    if is_tagged_union(union_info) {
        members.data  += 1;
        members.count -= 1; 
    }
    
    for members  if it.type == member_type  return true;
    return false;
}

get_union_tag_type :: (union_type: *Type_Info) -> *Type_Info {
    union_info := assert_base_type(union_type);
    assert(is_tagged_union(union_info), "union_info did not refer to a tagged union type!");
    return union_info.members[0].type;
}


// ========== Creating Type Infos ==========

/*
    If you decide to throw temperance to the wayside and do some really crazy stuff, 
    maybe you'll find yourself in the business of creating new type infos at runtime.
    
    In that case, here's a few helpful functions.
*/

make_fixed_array_type_info :: (count: int, element_type: *Type_Info) -> Type_Info_Array {
    return .{
        type         = .ARRAY,
        runtime_size = count * element_type.runtime_size,
        element_type = element_type,
        array_type   = .FIXED,
        array_count  = count,
    };
}

make_array_view_type_info :: (element_type: *Type_Info) -> Type_Info_Array {
    return .{
        type         = .ARRAY,
        runtime_size = count * element_type.runtime_size,
        element_type = size_of(Array_View_64),
        array_type   = .VIEW,
        array_count  = -1,
    };
}

make_resizable_array_type_info :: (element_type: *Type_Info) -> Type_Info_Array {
    return .{
        type         = .ARRAY,
        runtime_size = size_of(Resizable_Array),
        element_type = element_type,
        array_type   = .RESIZABLE,
        array_count  = -1,
    };
}

make_pointer_type_info :: (pointer_to: *Type_Info) -> Type_Info_Pointer {
    return .{
        type         = .POINTER,
        runtime_size = size_of(*void),
        pointer_to   = pointer_to,
    };
}

