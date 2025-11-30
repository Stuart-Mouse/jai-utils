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
    
    PROCEDURE_INFO :: #run get_procedure_info(PROCEDURE);
    PROCEDURE_TYPE_INFO :: type_info(type_of(PROCEDURE));
    
    callback := Dynamic_Callback.{
        procedure_info     = PROCEDURE_INFO,
        procedure_pointer  = xx PROCEDURE,
        argument_values    = NewArray(PROCEDURE_INFO.arguments.count, Any,, temp)
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
    
    copy_array :: ($E: Type, values: ..E) -> Any #expand {
        // TODO: make this actually useful
        return copy_any(values);
    }
    
    /* 
        TODO: can we do this all with code nodes?
        We currently have to do this really ugly inner insert,scope for all of the argument value expressions,
        since these need to have access to the outer scope, but the other code in the insert needs access to this procedure's scope.
        There has to be a better way to accomplish this...
    */
    #insert -> string {
        proc_call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
        
        assert(proc_call.context_modification == null, "Context modifications are not supported in make_dynamic_callback.");
        
        builder: String_Builder;
        
        for PROCEDURE_TYPE_INFO.argument_types {
            print(*builder, "Ta%1 :: #run (*PROCEDURE_TYPE_INFO.argument_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        
        for argument: proc_call.arguments_sorted {
            print(*builder, "// %: %\n", PROCEDURE_INFO.arguments[it_index].name, as_type(PROCEDURE_TYPE_INFO.argument_types[it_index]));
            
            if argument.kind == {
              case .MAKE_VARARGS;
                // NOTE: This does not cover the case that a user manually spreads some array of Any into the varargs.
                //       In that case, we will not automatically copy the values inside of the array.
                // TODO: probably need to get type expression for the varargs element type instead of relying on simply printing the element type as we do below.
                //       may also need to make sure that this type is inserted with scope... uuughh
                //       or we could do some goofy #run with a cast from array_info.element_type to type, sort of like the code to unpack the argument types above
                code_make_varargs := argument.(*Code_Make_Varargs);
                print(*builder, "callback.argument_values[%] = ", it_index);
                print(*builder, "copy_array(%, ", as_type(code_make_varargs.element_type));
                for code_make_varargs.expressions {
                    if it_index != 0  append(*builder, ", ");
                    if code_make_varargs.element_type.type == .ANY {
                        print(*builder, "copy_any(#insert,scope(scope) #string JAI\n");
                        Program_Print.print_expression(*builder, it);
                        append(*builder, ";\nJAI)");
                    } else {
                        Program_Print.print_expression(*builder, it);
                    }
                }
                append(*builder, ");\n");
                continue argument;
            }
            
            print(*builder, "callback.argument_values[%1] = copy_value(Ta%1, ", it_index);
            Program_Print.print_expression(*builder, argument);
            append(*builder, ");\n");
        }
        
        // str := builder_to_string(*builder); write_string(str); return str;
        return builder_to_string(*builder);
    }
    
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



#scope_file

#import "Compiler";
Program_Print :: #import "Program_Print";

