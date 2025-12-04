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
    type := callback._type_info;
    
    return_values := NewArray(type.return_types.count, Any,, allocator);
    for type.return_types {
        return_values[it_index] = New_Any(it, initialized = false);
    }
    
    return callback.call_wrapper(callback.procedure_pointer, callback.argument_values, return_values), return_values;
}


/*
    Trying to solve the issue of scoping between this macro and caller scope is relly tricky
        It seems I would need some way to insert,scope(scope) with references to elements of the make_dynamic_callback scope
        Which is something like trying to reference an inner scope from an outer scope, which may simply not be possible
        
        I fixed the issue with referencing the helper macros by just factoring them out
            but that required removing the reallocation stuff, so now there's no point to them being macros anyhow
            (I can't add back in the reallocation stuff because that would need ot reference data_to_copy)
                I can maybe just use some context member for the allocator here, but I probably don't want to use context.allocator,
                and I also don't want to add some new context member just for this
            and now we just have the same issue with not being bale to refernece the argument types, 
                and we can't just add the argument types tothe inserted code without also adding the logic to get PROCEDURE, PROCEDURE_INFO, etc
                so then we're just back in the situation where we now want to basically insert the whole body of the macro into the caller scope
                but if we do that, then we don't have access to the arguments! so like what do I do here?
            I really want to be able to just poke certain names into the block I am inserting so that 
*/
make_dynamic_callback :: ($procedure_call_expression: Code, $scope := #caller_code, $copy_any_values := true, $$allocator := temp) -> *Dynamic_Callback #expand {
    // TODO: Maybe figure out how to remove the need for the temporary allocation here.
    //       This will probably require totally changing the allocaiton strategy here.
    __copy_any :: (any: Any) -> Any #expand {
        // #if copy_any_values {
            new := Any.{ any.type, alloc(any.type.runtime_size,, temp) };
            memcpy(new.value_pointer, any.value_pointer, any.type.runtime_size);
            any = new;
            
            // if allocator.proc != temporary_allocator_proc {
            //     array_add(*data_to_copy, any);
            //     allocation_size += any.type.runtime_size;
            // }
        // }
        return any;
    }
    
    // __copy_value :: ($T: Type, $value_expression: Code) -> Any #expand {
    //     #if type_info(T).type == .ANY {
    //         any: Any = #insert,scope(scope) value_expression;
    //         return __copy_any(any);
    //     } else {
    //         value := New(T,, temp);
    //         value.* = #insert,scope(scope) value_expression;
    //         // if allocator.proc != temporary_allocator_proc {
    //         //     array_add(*data_to_copy, value.*);
    //         //     allocation_size += size_of(T);
    //         // }
    //         return value.*;
    //     }
    // }
    
    // The serves two purposes in our code generation below:
    //   1. Provide a type expression (that is not a Code_Type_Instantiation) so that the compiler can infer the type of the generated array literal.
    //   2. Implicitly convert the generated array literal to an array view.
    // We could theoretically get a Code_Type_Instantiation for these varargs arrays, but that would complicate code generation.
    // So this is the most simple solution for the time being...
    __copy_array :: ($E: Type, values: [] E) -> Any #expand {
        return __copy_any(values);
    }
    
    // TODO: maybe pass type instantiation as well so that we can ensure the proper type is inserted here. (A cast may be necessary in some cases).
    make_argument_inserter :: (argument: *Code_Node) -> *Code_Node #compile_time {
        root := compiler_get_nodes(#code __copy_any(#insert,scope(scope) #code 0));
        
        root.(*Code_Procedure_Call)
            .arguments_unsorted[0].expression.(*Code_Directive_Insert)
            .expression.(*Code_Directive_Code)
            .expression = argument;
        
        log("expression: %",
        root.(*Code_Procedure_Call)
            .arguments_unsorted[0].expression.(*Code_Directive_Insert)
            .expression.(*Code_Directive_Code)
            .expression.*
        );
        
        // TODO: insert cast here
        
        return root;
    }
    
    /* 
        TODO: any way to simplify this?
        we can't just do the following, because #procedure_of_call evaluates before the #insert:
            PROCEDURE :: #procedure_of_call #insert procedure_call_expression;
        so instead we have to do a wonky scoped insert that contains the #procedure_of_call internally
        (also, the above code actually crashes the compiler, so that's fun)
        
        TODO: we really need to code node version, since this one formats floats with zero-removal by default, which changes the type...
    */
    PROCEDURE :: #insert,scope(scope) -> string {
        builder: String_Builder;
        append(*builder, "#procedure_of_call ");
        Program_Print.print_expression(*builder, compiler_get_nodes(procedure_call_expression));
        append(*builder, ";\n");
        return builder_to_string(*builder);
    }
    
    PROCEDURE_INFO      :: #run get_procedure_info(PROCEDURE);
    PROCEDURE_TYPE      :: type_of(PROCEDURE);
    PROCEDURE_TYPE_INFO :: type_info(PROCEDURE_TYPE);
    
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
    // #run log("procedure type: %", type_of(PROCEDURE));
    
    callback := Dynamic_Callback.{
        procedure_info     = PROCEDURE_INFO,
        procedure_pointer  = xx PROCEDURE,
    };
    
    argument_values := #insert -> Code {
        procedure_call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
        
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
                            .{ expression = argument_type_expression },
                            .{ expression = it }
                        );
                    } else {
                        varargs_array_members[it_index] = it;
                    }
                }
                varargs_array_literal := make_array_literal(null, varargs_array_members);
                
                arguments_array_members[it_index] = make_procedure_call("__copy_array", 
                    .{ expression = argument_type_expression },
                    .{ expression = varargs_array_literal },
                );
                
              case;
                // arguments_array_members[it_index] = make_procedure_call("__copy_value",
                //     .{ expression = argument_type_expression },
                //     .{ expression = argument },
                // );
                arguments_array_members[it_index] = make_argument_inserter(argument);
            }
        }
        
        arguments_array_literal := make_array_literal(get_type_inst_from_declaration_code(#code dummy: Any), arguments_array_members);
        // log("arguments_array_literal: %", sprintx(arguments_array_literal));
        return compiler_get_code(arguments_array_literal);
    };
    
    callback.argument_values = array_copy(argument_values,, temp);
    
    callback_copy := New(Dynamic_Callback);
    memcpy(callback_copy, *callback, size_of(Dynamic_Callback));
    return callback_copy;
}


sprintx :: (root: *Code_Node) -> string {
    debug_builder: String_Builder;
    Program_Print.print_expression(*debug_builder, root);
    return builder_to_string(*debug_builder);
}

// get_type_inst_from_declaration :: (decl: *Code_Declaration) -> *Code_Type_Instantiation {
//     assert(decl.kind == .DECLARATION, "decl must be a declaration, but it was %", decl.kind);
//     type_inst := decl.type_inst;
//     if !decl.type_inst {
//         // type_inst := try_infer_type_inst_from_expression(decl.expression);
//         assert(type_inst != null, "Error: Unable to infer type instantiation from expression: %", sprintx(decl.expression,, temp));
//     }
//     return type_inst;
// }

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


#scope_file

#import "Compiler";
Program_Print :: #import "Program_Print";

