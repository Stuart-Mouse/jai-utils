
/*
    Procedure Info
    
    Questions to answer:
        What kinds of node nodes will we need to deal with other than identifier?
            Probably #procedure_of_call or #bake_parameters as well...
        When will argument names actually be available, and when will they not?
        Do we really care about return value names?
    
    TODO: 
        create several test cases using different types of procedure expressions
        access through struct members, array indexing, #bake_arguments, #procedure_of_call
            does the struct member case actually matter if the member is nonconstant?
            array case probably does not matter at all, or at least it's not very important
        
        write a procedure to just dump code nodes to the console so that I can see what particular expressions look like as code nodes
    
    NOTE:
        Looking at Code_Procedure_Call, and how #procedure_of_call actually works, it has me wondering:
        Could we use the argument values provided in procedure_of_call as an override on the default values that are provided to Lead Sheets?
        Technically, also, the user could just optionally provide a their own get_default_argument_value for a procedure.
            This is probably the way to go, and we just provide some procedures to create a procedure_info struct in a somewhat pre-checked way
            user can already just bake parameters if they want to, and manually providing default values seems a bit smarter, tbh
    
    TODO: 
        think about what things we could actually do with return value names
        maybe would be useful for something similar to or_return?
*/

get_procedure_header :: (node: *Code_Node) -> *Code_Procedure_Header #compile_time {
    if !node  return null;
    
    if node.kind == {
      case .PROCEDURE_HEADER;
        return xx node;
        
      case .IDENT;
        ident := node.(*Compiler.Code_Ident);
        return get_procedure_header(ident.resolved_declaration);
        
      case .DIRECTIVE_BAKE;
        bake := node.(*Code_Directive_Bake);
        return get_procedure_header(bake.procedure_call);
        
      case .DECLARATION;
        declaration := node.(*Code_Declaration);
        return get_procedure_header(declaration.expression);
        
      case .PROCEDURE_CALL;
        call := node.(*Code_Procedure_Call);
        if (call.flags & .RETURNS_PROCEDURE_POINTER_ONLY)
        || (call.flags & 0x400) {  // this means procedure_of_call was used
            return get_procedure_header(call.resolved_procedure_expression);
        }
    }
    return null;
}

Procedure_Info :: struct {
    procedure_expression:         string;
    arguments:                    [] Argument;
    returns:                      [] Argument;
    get_default_argument_value:   #type (int) -> bool, Any;
    
    Argument :: struct {
        name:   string;
        flags:  Flags;
        
        Flags :: enum_flags { 
            NONE :: 0; 
            HAS_DEFAULT_VALUE :: 1; 
            IS_VARARGS;
        }
    }
}

get_procedure_info :: ($procedure_expression: Code) -> Procedure_Info {
    // dumb helper for generating procedure_expression string
    remove_trailing_newline :: (s: string) -> string {
        return ifx s && s[s.count-1] == "\n" then string.{ s.count-1, s.data } else s;
    }

    #insert -> string {
        builder: String_Builder;
        
        root := Compiler.compiler_get_nodes(procedure_expression);
        proc_header := get_procedure_header(root);
        
        if proc_header == null {
            append(*builder, "Warning: Unable to resolve procedure header from expression: ");
            PP.print_expression(*builder, root);
            log(builder_to_string(*builder));
            return "return .{};";
        }
        
        assert(proc_header.procedure_flags & .POLYMORPHIC == 0, "Cannot get procedure info for a polymorphic procedure.");
        
        proc_info := proc_header.type.(*Type_Info_Procedure);
        append(*builder, "procedure := #insert procedure_expression;\n");
        append(*builder, "proc_info :: type_info(type_of(procedure));\n");
        for 0..proc_info.argument_types.count-1 {
            print(*builder, "Ta%1 :: #run (*proc_info.argument_types[%1]).(*Type).*;\n", it);
        }
        
        
        append(*builder, "procedure_expression_string :: #run remove_trailing_newline(#string __PROCEDURE_EXPRESSION__\n");
        PP.print_expression(*builder, root);
        append(*builder, "\n__PROCEDURE_EXPRESSION__);\n");
        
        // generate argument names array
        {
            append(*builder, "arguments :: Argument.[ ");
            for proc_header.arguments {
                print(*builder, ".{ name = \"%\", ", it.name);
                
                // TODO: make flags printing less hacky
                append(*builder, "flags = .NONE");
                if it.expression {
                    append(*builder, " | .HAS_DEFAULT_VALUE");
                    do_OR = true;
                }
                if it.type_inst && (it.type_inst.inst_flags & .VARARGS) {
                    append(*builder, " | .IS_VARARGS");
                }
                
                append(*builder, "},\n")
                // if it_index != proc_header.arguments.count-1 {
                //     append(*builder, ", ");
                // }
            }
            append(*builder, " ];\n");
        }
        
        // generate return value names array
        {
            append(*builder, "returns :: Argument.[ ");
            for proc_header.returns {
                print(*builder, ".{ name = \"%\" },\n", it.name);
            }
            append(*builder, " ];\n");
        }
        
        // generate get_default_argument_value for the procedure
        {
            append(*builder, "get_default_argument_value :: (index: int) -> bool, Any {\n");
            append(*builder, "ret: Any;\n");
            append(*builder, "if index == {\n");
            for proc_header.arguments {
                // TODO: check if this can actually be null. I am just assuming that it could be if we are given a polymorphic procedure.
                assert(it.type != null, "Type was not present on argument declaration node!");
                
                print(*builder, "case %;\n", it_index);
                if it.expression {
                    print(*builder, "ret = New_Any(xx Ta%, initialized = false);\n", it_index);
                    print(*builder, "ret.value_pointer.(*Ta%).* = ", it_index);
                    PP.print_expression(*builder, it.expression);
                    append(*builder, ";\n");
                } else {
                    append(*builder, "return false, Any.{};\n");
                }
            }
            append(*builder, "}\n");
            append(*builder, "return ret.value_pointer != null, ret;\n}\n");
        }
        
        append(*builder, "return .{ procedure_expression_string, argument_names, return_value_names, get_default_argument_value };\n");
        
        str := builder_to_string(*builder);
        // log(str);
        return str;
    }
}

get_default_argument_value :: (info: Procedure_Info, argument_name: string) -> bool, Any {
    for info.argument_names {
        if it == argument_name { 
            ok, value := info.get_default_argument_value(it_index);
            return ok, value;
        }
    }
    return false, Any.{};
}
