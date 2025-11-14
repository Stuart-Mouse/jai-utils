/*
    call_procedure is used to provide a generic interface for calling any type of procedure, 
        where arguments and return values are provided as `[] Any`.
    Using `#procedure_of_call call_procedure(null.(P), .[], .[])` we can generate a thin wrapper proc 
        that will handle the marshalling of arguments and return types from/to the provided Anys.
    This generated procedure will match the interface of a `Wrapped_Procedure_Call`,
        allowing us to store all the generated wrapper procs in a single array.
    We need only generate one such wrapper for each type of procedure, so if two procedures have the same exact signature,
        then we will not add bloat the code by baking the target procedure's specific pointer.
    
    All things considered, this is actually relatively simple. 
    Getting the argument and return types could be simplified, but other than that it's not too bad to generate the calling code for any given procedure type.
    If the compiler never gets built-in dyncall functionality, but does one day get a runtime "compile and link" sort of system, I could probably get by with using this code in addition to a few other little helper procs.
    Although, I still think it would be dumb for the compiler to do all that and not have a simple dyncall-like facility which is exposed to the user.
    If we can compile and link in arbitrary code, we should certainly be able to generate some simple procedure-calling code.
    
    TODO: I can probably work the extended procedure info stuff in here eventually, and provide some way for users to get the benefits of that without needing to pull the necessary parts of of Lead Sheets themselves.
*/

call_procedure :: (procedure: $P, arguments: [] Any, return_values: [] Any, check_types := true) -> bool #modify {
    return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type.";
} {
    proc_info :: type_info(P);
    
    if check_types {
        if arguments.count != proc_info.argument_types.count {
            log("Error: incorrect number of arguments provided in dynamic procedure call. Expected %, but got %.", proc_info.argument_types.count, arguments.count);
            return false;
        }
        for 0..arguments.count-1 {
            formal := proc_info.argument_types[it];
            actual := arguments[it].type;
            if formal != actual {
                log("Incorrect type provided for argument % in dynamic procedure call. Expected type %, but got %.", it, formal, actual);
                return false;
            }
        }
        
        if return_values.count != proc_info.return_types.count {
            log("Error: incorrect number of return values provided in dynamic procedure call. Expected %, but got %.", proc_info.argument_types.count, arguments.count);
            return false;
        }
        for 0..return_values.count-1 {
            formal := proc_info.return_types[it];
            actual := return_values[it].type;
            if formal != actual {
                log("Incorrect type provided for return value % in dynamic procedure call. Expected type %, but got %.", it, formal, actual);
                return false;
            }
        }
    }
    
    #insert -> string {
        builder: String_Builder;
        
        // Because we don't want to rely on simply printing types into the generated code (which won't work for procedure types),
        //   we need to first do some nonsense to unpack the types as constants we can easily reference.
        // Perhaps this sort of thing would be a lot easier to do if the compiler just collapsed the distinction between `Type` and `*Type_Info`, 
        //   or allowed casting between the two to be an implicitly comptime operation. 
        
        for 0..proc_info.argument_types.count-1 {
            print(*builder, "Ta%1 :: #run (*proc_info.argument_types[%1]).(*Type).*;\n", it);
        }
        for 0..proc_info.return_types.count-1 {
            print(*builder, "Tr%1 :: #run (*proc_info.return_types[%1]).(*Type).*;\n", it);
        }
        
        for 0..proc_info.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "return_values[%1].value_pointer.(*Tr%1).*", it);
        }
        if proc_info.return_types.count > 0 {
            append(*builder, " = ");
        }
        
        append(*builder, "procedure(");
        for 0..proc_info.argument_types.count-1 {
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
call_procedure_unsafe :: (procedure: $P, arguments: [] *void, return_values: [] *void) #modify {
    return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type.";
} {
    proc_info :: type_info(P);
    
    #insert -> string {
        builder: String_Builder;
        
        for 0..proc_info.argument_types.count-1 {
            print(*builder, "Ta%1 :: #run (*proc_info.argument_types[%1]).(*Type).*;\n", it);
        }
        for 0..proc_info.return_types.count-1 {
            print(*builder, "Tr%1 :: #run (*proc_info.return_types[%1]).(*Type).*;\n", it);
        }
        
        for 0..proc_info.return_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "return_values[%1].(*Tr%1).*", it);
        }
        if proc_info.return_types.count > 0 {
            append(*builder, " = ");
        }
        
        append(*builder, "procedure(");
        for 0..proc_info.argument_types.count-1 {
            if it != 0  append(*builder, ", ");
            print(*builder, "arguments[%1].(*Ta%1).*", it);
        }
        append(*builder, ");");
        
        return builder_to_string(*builder);
    }
}

/*
    To be honest, there's probably better ways to keep track of the wrappers for each procedure type that will better suit different projects,
        but this is what I have in place for Lead Sheets, so that's what provided for the time being.
*/

register_procedure_type_for_dynamic_call :: inline ($P: Type) #modify {
    return P.(*Type_Info).type == .PROCEDURE, "P must be a procedure type."; 
} {
    for procedure_wrappers if it.type == xx P  return;
    array_add(*procedure_wrappers, .{ xx P, xx #procedure_of_call call_procedure(null.(P), .[], .[]) });
}

Wrapped_Procedure_Call :: #type (pointer: *void, arguments: [] Any, return_values: [] Any, bool) -> bool;

procedure_wrappers: [..] struct { type: *Type_Info; wrapper: Wrapped_Procedure_Call; };

try_calling_procedure_with_wrapper :: (proc_info: *Type_Info_Procedure, proc_ptr: *void, arguments: [] Any, return_values: [] Any) -> bool {
    for procedure_wrappers {
        if it.type == proc_info {
            return it.wrapper(proc_ptr, arguments, return_values, true);
        }
    }
    log("Error: % has not been registered for use in a dynamic call.", (*proc_info).(*Type).*);
    return false;
}
