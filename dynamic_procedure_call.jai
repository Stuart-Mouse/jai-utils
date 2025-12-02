/*
    call_procedure is used to provide a generic interface for calling any type of procedure, 
        where arguments and return values are provided as `[] Any`.
    Using `#procedure_of_call call_procedure(null.(P), .[], .[])` we can generate a thin wrapper proc 
        that will handle the marshalling of arguments and return types from/to the provided Anys.
    This generated procedure will match the interface of a `Procedure_Call_Wrapper`,
        allowing us to store all the generated wrapper procs in a single array.
    We need only generate one such wrapper for each type of procedure, so if two procedures have the same exact signature,
        then we will not add bloat the code by baking the target procedure's specific pointer.
    
    All things considered, this is actually relatively simple. 
    Getting the argument and return types could be simplified, but other than that it's not too bad to generate the calling code for any given procedure type.
    If the compiler never gets built-in dyncall functionality, but does one day get a runtime "compile and link" sort of system, I could probably get by with using this code in addition to a few other little helper procs.
    Although, I still think it would be dumb for the compiler to do all that and not have a simple dyncall-like facility which is exposed to the user.
    If we can compile and link in arbitrary code, we should certainly be able to generate some simple procedure-calling code.
    
    
*/

typecheck_procedure_call :: (procedure_info: *Type_Info_Procedure, arguments: [] Any, return_values: [] Any) -> bool {
    if arguments.count != procedure_info.argument_types.count {
        log("Error: incorrect number of arguments provided in dynamic procedure call. Expected %, but got %.", procedure_info.argument_types.count, arguments.count);
        return false;
    }
    for 0..arguments.count-1 {
        formal := procedure_info.argument_types[it];
        actual := arguments[it].type;
        if formal != actual {
            log("Incorrect type provided for argument % in dynamic procedure call. Expected type %, but got %.", it, as_type(formal), as_type(actual));
            return false;
        }
    }
    
    if return_values.count != procedure_info.return_types.count {
        log("Error: incorrect number of return values provided in dynamic procedure call. Expected %, but got %.", procedure_info.return_types.count, return_values.count);
        return false;
    }
    for 0..return_values.count-1 {
        formal := procedure_info.return_types[it];
        actual := return_values[it].type;
        if formal != actual {
            log("Incorrect type provided for return value % in dynamic procedure call. Expected type %, but got %.", it, as_type(formal), as_type(actual));
            return false;
        }
    }
    
    return true;
}

Procedure_Call_Wrapper :: #type (procedure: *void, arguments: [] Any, return_values: [] Any) -> bool;

call_procedure :: call_procedure_checked;
call_procedure :: call_procedure_unchecked;

call_procedure_checked :: ($P: Type, procedure: P, arguments: [] Any, return_values: [] Any) -> bool #modify {
    return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type.";
} {
    PROCEDURE_TYPE_INFO :: type_info(P);
    
    if !typecheck_procedure_call(PROCEDURE_TYPE_INFO, arguments, return_values)  return false;
    
    #insert -> string {
        builder: String_Builder;
        
        /*
            Because we don't want to rely on simply printing types into the generated code (which won't work for procedure types),
              we need to first do some nonsense to unpack the types as constants we can easily reference.
            Perhaps this sort of thing would be a lot easier to do if the compiler just collapsed the distinction between `Type` and `*Type_Info`, 
              or allowed casting between the two to be an implicitly comptime operation. 
        */
        for PROCEDURE_TYPE_INFO.argument_types {
            print(*builder, "Ta%1 :: #run (*PROCEDURE_TYPE_INFO.argument_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        for PROCEDURE_TYPE_INFO.return_types {
            print(*builder, "Tr%1 :: #run (*PROCEDURE_TYPE_INFO.return_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        
        
        /*
            NOTE: This is such a ridiculous hack, and it makes me sad.
            
            What's going on here is that we have to basically reconstruct the same procedure type
            so that we can shake the invisible extra data that the compiler holds about a type.
            In particular, we need to make sure that the compiler forgets about varargs, 
            since otherwise, our call will pass the arguments incorrectly below.
        */
        append(*builder, "__procedure := procedure.(");
        append(*builder, "#type (");
        for 0..PROCEDURE_TYPE_INFO.argument_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "Ta%1", it);
        }
        append(*builder, ")");
        if PROCEDURE_TYPE_INFO.return_types {
            append(*builder, " -> ");
        }
        for 0..PROCEDURE_TYPE_INFO.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "Tr%1", it);
        }
        append(*builder, ");\n");
        
        
        // and now the actual procedure call
        for 0..PROCEDURE_TYPE_INFO.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "return_values[%1].value_pointer.(*Tr%1).*", it);
        }
        if PROCEDURE_TYPE_INFO.return_types.count > 0 {
            append(*builder, " = ");
        }
        append(*builder, "__procedure(");
        for 0..PROCEDURE_TYPE_INFO.argument_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "arguments[%1].value_pointer.(*Ta%1).*", it);
        }
        append(*builder, ");");
        
        return builder_to_string(*builder);
    }
    
    return true;
}

/*
    This is mostly the same logic as call_procedure, but of course there's no typechecking.
    Using this version may be preferable if you're already doing your own typechecking and don't want the additional overhead of passing an Any.
    I don't currently have the registration system below set up to work with this version, but it should be straightforward to adapt it.
*/
call_procedure_unchecked :: ($P: Type, procedure: P, arguments: [] *void, return_values: [] *void) #modify {
    return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type.";
} {
    PROCEDURE_TYPE_INFO :: type_info(P);
    
    #insert -> string {
        builder: String_Builder;
        
        for PROCEDURE_TYPE_INFO.argument_types {
            print(*builder, "Ta%1 :: #run (*PROCEDURE_TYPE_INFO.argument_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        for PROCEDURE_TYPE_INFO.return_types {
            print(*builder, "Tr%1 :: #run (*PROCEDURE_TYPE_INFO.return_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        
        
        append(*builder, "__procedure := procedure.(");
        append(*builder, "#type (");
        for 0..PROCEDURE_TYPE_INFO.argument_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "Ta%1", it);
        }
        append(*builder, ")");
        if PROCEDURE_TYPE_INFO.return_types {
            append(*builder, " -> ");
        }
        for 0..PROCEDURE_TYPE_INFO.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "Tr%1", it);
        }
        append(*builder, ");\n");
        
        
        for 0..PROCEDURE_TYPE_INFO.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "return_values[%1].(*Tr%1).*", it);
        }
        if PROCEDURE_TYPE_INFO.return_types.count > 0 {
            append(*builder, " = ");
        }
        
        append(*builder, "__procedure(");
        for 0..PROCEDURE_TYPE_INFO.argument_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "arguments[%1].(*Ta%1).*", it);
        }
        append(*builder, ");");
        
        return builder_to_string(*builder);
    }
}






Dynamic_Callback :: struct {
    using procedure_info:   Procedure_Info;
    procedure_pointer:      *void;
    
    argument_values:        [] Any;
    
    // TODO: attach optional return values array as well?
    // TODO: maybe allow user to attach pointer to context to use when we execute the dynamic call
    
    // TODO: how to store context modifications and set/restore at call site?
    // context_modifications:  [] Context_Modification;
    // Context_Modification :: struct {
    //     offset_in_bytes:    int;
    //     type:               Type;
    //     value_pointer:      *void;
    // }
}

call :: (callback: Dynamic_Callback, return_values: ..Any) -> bool {
    return callback.dynamic_call_wrapper(callback.procedure_pointer, callback.argument_values, return_values);
}

call :: (callback: Dynamic_Callback, allocator := temp) -> bool, [] Any {
    type := get_type_info(callback.procedure_info);
    
    return_values := NewArray(type.return_types.count, Any,, allocator);
    for type.return_types {
        return_values[it_index] = New_Any(it, initialized = false);
    }
    
    return callback.call_wrapper(callback.procedure_pointer, callback.argument_values, return_values), return_values;
}


make_dynamic_callback :: ($procedure_call_expression: Code, $scope := #caller_code, $$allocator := temp) -> *Dynamic_Callback #expand {
    /* 
        TODO: any way to simplify this?
        we can't just do the following, because #procedure_of_call evaluates before the #insert:
            PROCEDURE :: #procedure_of_call #insert procedure_call_expression;
        so instead we have to do a wonky scoped insert that contains the #procedure_of_call internally
        (also, the above code actually crashes the compiler, so that's fun)
    */
    // TODO: we really need to code node version, since this one formats floats with zero-removal by default, which changes the type...
    PROCEDURE :: #insert,scope(scope) -> string {
        builder: String_Builder;
        append(*builder, "#procedure_of_call ");
        Program_Print.print_expression(*builder, compiler_get_nodes(procedure_call_expression));
        append(*builder, ";\n");
        return builder_to_string(*builder);
    }
    
    // TODO: mail this version in. something really weird is going on here...
    //       when using print, get a 'not all control paths return a value' warning, and get a junk value returned.
    // PROCEDURE :: #insert -> Code {
    //     call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
    //     assert(call.kind == .PROCEDURE_CALL && call.resolved_procedure_expression != null);
    //     return compiler_get_code(call.resolved_procedure_expression);
    // }
    
    PROCEDURE_INFO      :: #run get_procedure_info(PROCEDURE);
    PROCEDURE_TYPE      :: type_of(PROCEDURE);
    PROCEDURE_TYPE_INFO :: type_info(PROCEDURE_TYPE);
    
    log("PROCEDURE_TYPE_INFO: %", PROCEDURE_TYPE_INFO.*);
    
    callback := Dynamic_Callback.{
        procedure_info     = PROCEDURE_INFO,
        procedure_pointer  = xx PROCEDURE,
    };
    
    arguments_provided_array_size := PROCEDURE_INFO.arguments.count * size_of(Any);
    allocation_size := size_of(Dynamic_Callback) + arguments_provided_array_size;
    
    // I hate to put even more garbage into temp, but not sure how else to handle this gracefully, 
    // without having to do some kind of pre-pass to determine allocation size up front (which I am not sure if that is even actually possible).
    data_to_copy: [..] Any;
    data_to_copy.allocator = temp;
    
    copy_any :: (a: Any) -> Any #expand {
        any: Any = ---;
        any.type = a.type;
        any.value_pointer = alloc(a.type.runtime_size,, temp);
        memcpy(any.value_pointer, a.value_pointer, a.type.runtime_size);
        
        if allocator.proc != temporary_allocator_proc {
            array_add(*data_to_copy, any);
            allocation_size += a.type.runtime_size;
        }
        return any;
    }
    
    copy_value :: ($T: Type, $value_expression: Code) -> Any #expand {
        value := New(T,, temp);
        value.* = #insert,scope(scope) value_expression;
        if allocator.proc != temporary_allocator_proc {
            array_add(*data_to_copy, value.*);
            allocation_size += size_of(T);
        }
        return value.*;
    }
    
    // TODO: make this actually useful
    copy_array :: (values: [] $E) -> Any #expand {
    // copy_array :: ($E: Type, values: [] E) -> Any #expand {
        return copy_any(values);
    }
    
    ARGUMENT_TYPE_COUNT :: #run PROCEDURE_TYPE_INFO.argument_types.count;
    ARGUMENT_TYPES :: #run () -> [ARGUMENT_TYPE_COUNT] Type {
        types: [ARGUMENT_TYPE_COUNT] Type;
        for PROCEDURE_TYPE_INFO.argument_types types[it_index] = get_type(it);
        return types;
    }();
    
    // #insert -> string {
    //     procedure_call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
    //     arguments_array_members := NewArray(procedure_call.arguments_sorted.count, *Code_Node);
    //     for argument: procedure_call.arguments_sorted {
    //         if argument.kind == {
    //           case .MAKE_VARARGS;
    //             "ARGUMENT_%1_ELEMENT_TYPE :: #run get_type(type_info(ARGUMENT_TYPES[%1]).element_type));\n", it_index);
    //             "ARGUMENT_%1_ELEMENT_DUMMY: ARGUMENT_%1_ELEMENT_TYPE;\n", it_index);
    //         }
    //     }
    // }
    
    // #run log("procedure type: %", type_of(PROCEDURE));
    
    argument_values := #insert -> Code {
        procedure_call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
        
        assert(procedure_call.context_modification == null, "Context modifications are not supported in make_dynamic_callback.");

        procedure_header := get_procedure_header(procedure_call);
        assert(procedure_header != null, "Error: unable to get procedure header from code:\n%", sprintx(procedure_call));
        
        // Attempted to use this to get a type inst for T that resolved inside the procedure body's scope. does not seem to work
        // procedure_body_code := code_of(procedure_call_expression);
        
        arguments_array_members := NewArray(procedure_call.arguments_sorted.count, *Code_Node);
        for argument: procedure_call.arguments_sorted {
            argument_type_expression := compiler_get_nodes(#code ARGUMENT_TYPES[0]);
            argument_type_expression.(*Code_Binary_Operator).right.(*Code_Literal)._s64 = it_index;
            
            if argument.kind == {
              case .MAKE_VARARGS;
                make_varargs := argument.(*Code_Make_Varargs);
                varargs_element_type_inst := get_type_inst_from_declaration(procedure_header.arguments[it_index]).array_element_type;
                assert(varargs_element_type_inst != null, "Error while trying to get varargs element type instantiation.");
                
                // TODO: this approach does not seem to work either, but maybe we can try just getting an identifier to resolve to this type?
                // varargs_element_type_inst = compiler_get_nodes(compiler_get_code(varargs_element_type_inst, procedure_body_code)).(*Code_Type_Instantiation);
                // log("kind: %", varargs_element_type_inst.kind);
                
                // TODO: got no idea what to do about the below error... 
                // Error: Unable to resolve this identifier because it is blocked by an inserter that could not complete. (get_type)
                // insert_type := New(Code_Directive_Insert);
                // insert_type.expression = make_string_literal(tprint("#run,stallable get_type(type_info(ARGUMENT_TYPES[%1]).element_type);", it_index));
                
                varargs_array_members := NewArray(make_varargs.expressions.count, *Code_Node);
                for make_varargs.expressions {
                    if make_varargs.element_type.type == .ANY {
                        varargs_array_members[it_index] = make_procedure_call("copy_any", .{ expression = it });
                    } else {
                        varargs_array_members[it_index] = it;
                    }
                }
                varargs_array_literal := make_array_literal(varargs_element_type_inst, varargs_array_members);
                
                arguments_array_members[it_index] = make_procedure_call("copy_array", 
                    // .{ expression = insert_type },
                    .{ expression = varargs_array_literal },
                );
                
              case;
                arguments_array_members[it_index] = make_procedure_call("copy_value",
                    .{ expression = argument_type_expression },
                    .{ expression = argument },
                );
            }
        }
        
        arguments_array_literal := make_array_literal(get_type_inst_from_declaration_code(#code dummy: Any), arguments_array_members);
        // log("arguments_array_literal: %", sprintx(arguments_array_literal));
        return compiler_get_code(arguments_array_literal);
    };
    
    callback.argument_values = array_copy(argument_values,, temp);
    
    if allocator.proc != temporary_allocator_proc {
        memory := alloc(allocation_size,, allocator);
        dst := memory;
        
        memcpy(dst, *callback, size_of(Dynamic_Callback)); 
        dst += size_of(Dynamic_Callback);
        
        for *data_to_copy {
            memcpy(dst, it.value_pointer, it.type.runtime_size);
            it.value_pointer = dst; // overwrite value pointers in place so that when we copy argument_values we are pointing at the right place
            dst += it.type.runtime_size;
        }
        
        memcpy(dst, *callback.argument_values.data, arguments_provided_array_size); 
        callback.argument_values.data = dst;
        dst += arguments_provided_array_size;
        
        return xx memory;
    }
    
    callback_copy := New(Dynamic_Callback);
    memcpy(callback_copy, *callback, size_of(Dynamic_Callback));
    return callback_copy;
}


sprintx :: (root: *Code_Node) -> string {
    debug_builder: String_Builder;
    Program_Print.print_expression(*debug_builder, root);
    return builder_to_string(*debug_builder);
}

// TODO: make this less dumb
// try_infer_type_inst_from_expression :: (code: *Code_Node) -> *Code_Type_Instantiation {
//     // TODO: if code node type is some literal, we should be able to get type inst from that
//     if code.kind == {
//       case .LITERAL;
        
//     }
    
//     if code.type.type == {
//       case .ANY;
        
        
//       case .BOOL;
//         return compiler_get_nodes(#code dummy: bool).(*Code_Declaration).type_inst;
        
//       case .INTEGER;
//         integer_info := code.type.(*Type_Info_Integer);
//         if integer_info.signed {
//             if integer_info.runtime_size == {
//               case 1; return compiler_get_nodes(#code dummy: s\8).(*Code_Declaration).type_inst;
//               case 2; return compiler_get_nodes(#code dummy: s16).(*Code_Declaration).type_inst;
//               case 4; return compiler_get_nodes(#code dummy: s32).(*Code_Declaration).type_inst;
//               case 8; return compiler_get_nodes(#code dummy: s64).(*Code_Declaration).type_inst;
//             }
//         } else {
//             if integer_info.runtime_size == {
//               case 1; return compiler_get_nodes(#code dummy: u\8).(*Code_Declaration).type_inst;
//               case 2; return compiler_get_nodes(#code dummy: u16).(*Code_Declaration).type_inst;
//               case 4; return compiler_get_nodes(#code dummy: u32).(*Code_Declaration).type_inst;
//               case 8; return compiler_get_nodes(#code dummy: u64).(*Code_Declaration).type_inst;
//             }
//         }
        
//       case .FLOAT;
//         if code.type.runtime_size == {
//           case 4; return compiler_get_nodes(#code dummy: float32).(*Code_Declaration).type_inst;
//           case 8; return compiler_get_nodes(#code dummy: float64).(*Code_Declaration).type_inst;
//         }
//     }
    
//     return null;
// }

get_type_inst_from_declaration :: (decl: *Code_Declaration) -> *Code_Type_Instantiation {
    assert(decl.kind == .DECLARATION, "decl must be a declaration, but it was %", decl.kind);
    type_inst := decl.type_inst;
    if !decl.type_inst {
        // type_inst := try_infer_type_inst_from_expression(decl.expression);
        assert(type_inst != null, "Error: Unable to infer type instantiation from expression: %", sprintx(decl.expression,, temp));
    }
    return type_inst;
}

get_type_inst_from_declaration_code :: (decl_code: Code) -> *Code_Type_Instantiation {
    decl := compiler_get_nodes(decl_code).(*Code_Declaration);
    assert(decl.kind == .DECLARATION, "decl must be a declaration, but it was %", decl.kind);
    assert(decl.type_inst != null, "decl must have a type instantiation");
    return decl.type_inst;
}

make_array_literal :: (element_type: *Code_Type_Instantiation, array_members: [] *Code_Node) -> *Code_Literal {
    literal := New(Code_Literal);
    literal.value_type = .ARRAY;
    literal.array_literal_info = New(Code_Array_Literal_Info);
    literal.array_literal_info.element_type  = element_type;
    literal.array_literal_info.array_members = array_members;
    return literal;
};

get_nodes_from_codes :: (codes: ..Code) -> [] *Code_Node {
    nodes := NewArray(codes.count, *Code_Node);
    for *nodes  it.* = compiler_get_nodes(codes[it_index]);
    return nodes;
}

make_identifier :: (name: string) -> *Code_Ident {
    ident := New(Code_Ident);
    ident.name = name;
    return ident;
}

// TODO: make a version where we can just pass arguments as code in name = value format, or just as values
//       maybe this is a trivial convenience, but it would be kinda nice
make_procedure_call :: (procedure_name: string, arguments: ..Code_Argument) -> *Code_Procedure_Call {
    call := New(Code_Procedure_Call);
    call.procedure_expression = make_identifier(procedure_name);
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}

make_procedure_call :: (procedure_expression: *Code_Node, arguments: ..Code_Argument) -> *Code_Procedure_Call {
    call := New(Code_Procedure_Call);
    call.procedure_expression = procedure_expression;
    call.arguments_unsorted   = array_copy(arguments);
    return call;
}


#scope_file

#import "Compiler";
Program_Print :: #import "Program_Print";

