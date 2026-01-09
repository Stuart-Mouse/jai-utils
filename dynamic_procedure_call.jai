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
    using procedure_info:   *Procedure_Info;
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
    return callback.call_wrapper(callback.procedure_pointer, callback.argument_values, return_values);
}

call :: (callback: Dynamic_Callback, allocator := temp) -> bool, [] Any {
    type := callback._type_info;
    
    return_values := NewArray(type.return_types.count, Any,, allocator);
    for type.return_types {
        return_values[it_index] = New_Any(it, initialized = false);
    }
    
    return callback.call_wrapper(callback.procedure_pointer, callback.argument_values, return_values), return_values;
}


make_dynamic_callback :: ($procedure_call_code: Code, $copy_any_values := true, $$allocator := temp) -> *Dynamic_Callback #expand {
    __copy_value :: (value: $T) -> Any #expand {
        #if type_info(T).type == .ANY {
            #if copy_any_values {
                if allocator.proc != temporary_allocator_proc {
                    array_add(*data_to_copy, any);
                    allocation_size += any.type.runtime_size;
                }
            }
        } else {
            if allocator.proc != temporary_allocator_proc {
                array_add(*data_to_copy, value);
                allocation_size += size_of(T);
            }
        }
        return value;
    }
    
    // The serves two purposes in our code generation below:
    //   1. Provide a type expression (that is not a Code_Type_Instantiation) so that the compiler can infer the type of the generated array literal.
    //   2. Implicitly convert the generated array literal to an array view.
    // We could theoretically get a Code_Type_Instantiation for these varargs arrays, but that would complicate code generation.
    // So this is the most simple solution for the time being...
    __copy_array :: ($E: Type, values: [] E) -> Any #expand {
        return __copy_value(values);
    }
    
    // TODO: test that type cast works properly here and does what we want it to do.
    //       create test case that fails with old code and passes with new code so we can demonstrate this.
    // NOTE: Technically, we should not be able to cast to struct types, but compiler seems to be ok with it here, 
    //       probably because it is already that type. But noting this since it could potentially be a concern later on if we see issues.
    make_argument_inserter :: (argument: *Code_Node, type_expression: *Code_Node) -> *Code_Node #compile_time {
        root := compiler_get_nodes(#code (#insert,scope(procedure_call_code) #code 0).(int));
        
        root.(*Code_Cast)
            .expression.(*Code_Directive_Insert)
            .expression.(*Code_Directive_Code)
            .expression = argument;
        
        root.(*Code_Cast).target_type.* = .{ type_valued_expression = type_expression };
        
        return root;
    }
    
    // We do a sort of manual #procedure_of_call here by setting the RETURNS_PROCEDURE_POINTER_ONLY flag.
    PROCEDURE :: #insert -> Code {
        procedure_call := compiler_get_nodes(procedure_call_code).(*Code_Procedure_Call);
        procedure_call.flags |= .RETURNS_PROCEDURE_POINTER_ONLY;
        return compiler_get_code(procedure_call, procedure_call_code);
    }
    
    PROCEDURE_INFO      :: #run get_procedure_info(PROCEDURE, procedure_call_code);
    PROCEDURE_TYPE      :: type_of(PROCEDURE);
    PROCEDURE_TYPE_INFO :: type_info(PROCEDURE_TYPE);
    
    // Dumping all the argument types here is necessary for our code generation below.
    ARGUMENT_TYPE_COUNT :: #run PROCEDURE_TYPE_INFO.argument_types.count;
    ARGUMENT_TYPES :: #run () -> [ARGUMENT_TYPE_COUNT] Type {
        types: [ARGUMENT_TYPE_COUNT] Type;
        for PROCEDURE_TYPE_INFO.argument_types {
            // NOTE: It's a bit of a hack, but for varargs arguments we save the element type instead of the array type.
            //       This is necessary for our code generation done below.
            types[it_index] = get_type(
                ifx PROCEDURE_INFO.arguments[it_index].flags & .IS_VARARGS
                then it.(*Type_Info_Array).element_type else it
            );
        }
        return types;
    }();
    
    callback := Dynamic_Callback.{
        procedure_info     = *PROCEDURE_INFO,
        procedure_pointer  = xx PROCEDURE,
    };
    
    
    arguments_values_array_size := PROCEDURE_INFO.arguments.count * size_of(Any);
    allocation_size := size_of(Dynamic_Callback) + arguments_values_array_size;
    
    // I hate to put even more garbage into temp, but not sure how else to handle this gracefully, 
    // without having to do some kind of pre-pass to determine allocation size up front (which I am not sure if that is even actually possible).
    data_to_copy: [..] Any;
    data_to_copy.allocator = temp;
    
    argument_values := #insert -> Code {
        procedure_call := compiler_get_nodes(procedure_call_code).(*Code_Procedure_Call);
        
        assert(procedure_call.context_modification == null, "Context modifications are not supported in make_dynamic_callback.");
        
        procedure_header := get_procedure_header(procedure_call);
        assert(procedure_header != null, "Error: unable to get procedure header from code:\n%", sprintx(procedure_call));
        
        arguments_array_members := NewArray(procedure_call.arguments_sorted.count, *Code_Node);
        for argument: procedure_call.arguments_sorted {
            argument_type_expression := compiler_get_nodes(#code ARGUMENT_TYPES[0]);
            argument_type_expression.(*Code_Binary_Operator).right.(*Code_Literal)._s64 = it_index;
            
            if argument.kind == {
              case .MAKE_VARARGS;
                make_varargs := argument.(*Code_Make_Varargs);
                
                varargs_array_members := NewArray(make_varargs.expressions.count, *Code_Node);
                for make_varargs.expressions {
                    if make_varargs.element_type.type == .ANY {
                        varargs_array_members[it_index] = make_procedure_call("__copy_value", 
                            .{ expression = it }
                        );
                    } else {
                        varargs_array_members[it_index] = make_argument_inserter(it, argument_type_expression);
                    }
                }
                varargs_array_literal := make_array_literal(null, varargs_array_members);
                
                arguments_array_members[it_index] = make_procedure_call("__copy_array", 
                    .{ expression = argument_type_expression },
                    .{ expression = varargs_array_literal },
                );
                
              case;
                arguments_array_members[it_index] = make_procedure_call("__copy_value",
                    .{ expression = make_argument_inserter(argument, argument_type_expression) },
                );
            }
        }
        
        arguments_array_literal := make_array_literal(get_type_inst_from_declaration_code(#code dummy: Any), arguments_array_members);
        // log("arguments_array_literal: %", sprintx(arguments_array_literal));
        return compiler_get_code(arguments_array_literal, null);
    };
    
    callback.argument_values = array_copy(argument_values,, temp);
    
    if allocator.proc != temporary_allocator_proc {
        memory := alloc(allocation_size,, allocator);
        dst    := memory + size_of(Dynamic_Callback);
        
        for *data_to_copy {
            memcpy(dst, it.value_pointer, it.type.runtime_size);
            it.value_pointer = dst; // overwrite value pointers in place so that when we copy argument_values we are pointing at the right place
            dst += it.type.runtime_size;
        }
        
        memcpy(dst, callback.argument_values.data, arguments_values_array_size); 
        callback.argument_values.data = dst;
        dst += arguments_values_array_size;
        
        memcpy(memory, *callback, size_of(Dynamic_Callback)); 
        return xx memory;
    }
    
    callback_copy := New(Dynamic_Callback,, temp);
    memcpy(callback_copy, *callback, size_of(Dynamic_Callback));
    return callback_copy;
}


sprintx :: (root: *Code_Node) -> string {
    debug_builder: String_Builder;
    Program_Print.print_expression(*debug_builder, root);
    return builder_to_string(*debug_builder);
}

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
}

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

make_if_case :: (condition: *Code_Node, cases: ..*Code_Case) -> *Code_If {
    if_node := New(Code_If);
    if_node.condition = condition;
    if_node.then_block = New(Code_Block);
    
    if_node.then_block.statements = cast,force([]*Code_Node) cases;
    for cases {
        it.then_block.node_flags = .IS_PARENTHESIZED;
        it.then_block.owning_statement = cases[0];
    }
    
    if_node.if_flags = .IS_SWITCH_STATEMENT;
    return if_node;
}

// TODO: test inserting the procedure as a string using program print. I reckon that will fail also

// Basic insert fails here.
// insert_procedure :: ($procedure_code: Code) {
//     PROCEDURE :: #insert procedure_code;
//     PROCEDURE(5);
// }

// insert,scope() fails also, whether using procedure_code or other, separately provided scope.
// insert_procedure :: ($procedure_code: Code, $scope := #caller_code) {
//     PROCEDURE :: #insert,scope(scope) procedure_code;
//     PROCEDURE(5);
// }

// Getting the nodes and then calling compiler_get_code with the target scope fares no better.
// insert_procedure :: ($procedure_code: Code, $scope := #caller_code) {
//     PROCEDURE :: #insert -> Code {
//         root := compiler_get_nodes(procedure_code);
//         return compiler_get_code(root, scope);
//     }
//     PROCEDURE(5);
// }

// Yet, this one works. It's almost like the compiler is just not propagating the scope through procedure headers for some reason, 
// but if we remove the argument expression from its context and insert it alone, then it gets the proper scope.
get_default_value_of_first_procedure_argument :: ($procedure_code: Code, $scope := #caller_code) {
    DEFAULT_VALUE :: #insert -> Code {
        root := compiler_get_nodes(procedure_code).(*Code_Procedure_Header);
        return compiler_get_code(root.arguments[0].expression, scope);
    };
    log("%", DEFAULT_VALUE);
}



log_arguments :: ($procedure_call_code: Code) #expand {
    __array_view :: ($E: Type, values: [] E) -> [] E #expand { return values; }
    
    #insert -> Code {
        block := New(Code_Block);
        block.block_type = .IMPERATIVE;
        
        statements: [..] *Code_Node;
        
        procedure_call := compiler_get_nodes(procedure_call_code).(*Code_Procedure_Call);
        assert(procedure_call.kind == .PROCEDURE_CALL);
        
        procedure_header := get_procedure_header(procedure_call);
        assert(procedure_header != null, "Error: unable to get procedure header from code:\n%", sprintx(procedure_call));
        
        for argument, argument_index: procedure_call.arguments_sorted {
            log("argument %: %", argument_index, argument);
            format_string := make_string_literal(tprint("argument %: \%", argument_index));
            log("format_string: %", format_string);
            
            // if argument.kind == {
            //   case .MAKE_VARARGS;
            //     make_varargs := argument.(*Code_Make_Varargs);
                
            //     varargs_array_members := NewArray(make_varargs.expressions.count, *Code_Node);
            //     for make_varargs.expressions {
            //         // if make_varargs.element_type.type == .ANY {
            //         //     varargs_array_members[it_index] = make_procedure_call("__copy_value", 
            //         //         .{ expression = argument_type_expression },
            //         //         .{ expression = it }
            //         //     );
            //         // } else {
            //             varargs_array_members[it_index] = it;
            //         // }
            //     }
            //     varargs_array_literal := make_procedure_call("__array_view", 
                    
            //         make_array_literal(null, varargs_array_members)
            //     );
                
            //     log_call := make_procedure_call("log", 
            //         .{ expression = format_string },
            //         .{ expression = varargs_array_literal },
            //     );
            //     array_add(*statements, log_call);
                
            //   case;
                log_call := make_procedure_call("log", 
                    .{ expression = format_string },
                    .{ expression = argument },
                );
                array_add(*statements, log_call);
            // }
        }
        
        array_add(*statements, procedure_call);
        
        block.statements = xx statements;
        
        log("%", sprintx(block));
        
        return compiler_get_code(block, procedure_call_code);
    }
}






#scope_file

#import "Compiler";
Program_Print :: #import "Program_Print";

