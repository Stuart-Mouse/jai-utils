
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
        ident := node.(*Code_Ident);
        return get_procedure_header(ident.resolved_declaration);
        
      case .DIRECTIVE_BAKE;
        bake := node.(*Code_Directive_Bake);
        return get_procedure_header(bake.procedure_call);
        
      case .DECLARATION;
        declaration := node.(*Code_Declaration);
        return get_procedure_header(declaration.expression);
        
      case .DIRECTIVE_INSERT;
        insert := node.(*Code_Directive_Insert);
        return get_procedure_header(insert.expansion);
        
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
    procedure_expression:       string;
    type:                       Type;
    arguments:                  [] Argument;
    returns:                    [] Argument;
    
    // TODO: maybe #if to make this optional
    dynamic_call_wrapper:       Wrapped_Procedure_Call;
    
    Argument :: struct {
        name:           string;
        flags:          Flags;
        get_default:    #type () -> Any;
        
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
        
        root := compiler_get_nodes(procedure_expression);
        proc_header := get_procedure_header(root);
        
        if proc_header == null {
            append(*builder, "Warning: Unable to resolve procedure header from expression: ");
            Program_Print.print_expression(*builder, root);
            log(builder_to_string(*builder));
            return "return .{};";
        }
        
        assert(proc_header.procedure_flags & .POLYMORPHIC == 0, "Cannot get procedure info for a polymorphic procedure.");
        
        proc_type := proc_header.type.(*Type_Info_Procedure);
        append(*builder, "procedure :: #insert procedure_expression;\n");
        append(*builder, "proc_type :: type_info(type_of(procedure));\n");
        for 0..proc_type.argument_types.count-1 {
            print(*builder, "Ta%1 :: #run (*proc_type.argument_types[%1]).(*Type).*;\n", it);
        }
        
        
        append(*builder, "procedure_expression_string :: #run remove_trailing_newline(#string __PROCEDURE_EXPRESSION__\n");
        Program_Print.print_expression(*builder, root);
        append(*builder, "\n__PROCEDURE_EXPRESSION__);\n");
        
        // generate arguments array
        {
            append(*builder, "arguments :: Procedure_Info.Argument.[\n");
            for proc_header.arguments {
                print(*builder, ".{ name = \"%\", ", it.name);
                
                // TODO: make flags printing less hacky
                append(*builder, "flags = .NONE");
                if it.expression {
                    append(*builder, " | .HAS_DEFAULT_VALUE");
                }
                if it.type_inst && (it.type_inst.inst_flags & .VARARGS) {
                    append(*builder, " | .IS_VARARGS");
                }
                append(*builder, ", ");
                
                if it.expression {
                    append(*builder, "get_default = () -> Any { ");
                    print(*builder, "ret := New_Any(xx Ta%, initialized = false); ", it_index);
                    print(*builder, "ret.value_pointer.(*Ta%).* = ", it_index);
                    Program_Print.print_expression(*builder, it.expression);
                    append(*builder, "; return ret; }, ");
                }
                
                append(*builder, "},\n");
                // if it_index != proc_header.arguments.count-1 {
                //     append(*builder, ", ");
                // }
            }
            append(*builder, "];\n");
        }
        
        // generate return values array
        {
            append(*builder, "returns :: Procedure_Info.Argument.[ ");
            for proc_header.returns {
                print(*builder, ".{ name = \"%\" },\n", it.name);
            }
            append(*builder, " ];\n");
        }
        
        append(*builder, "dynamic_call_wrapper :: #procedure_of_call call_procedure(null.(type_of(procedure)), .[], .[]);\n");
        
        append(*builder, #string JAI
        return .{ 
            procedure_expression_string, 
            type_of(procedure), 
            arguments, 
            returns, 
            xx dynamic_call_wrapper 
        };
        JAI);
        
        str := builder_to_string(*builder);
        return str;
    }
}

get_default_argument_value :: (info: Procedure_Info, argument_name: string) -> bool, Any {
    for info.arguments {
        if it.name == argument_name { 
            if !(it.flags & .HAS_DEFAULT_VALUE)  return false, Any.{};
            return true, it.get_default();
        }
    }
    return false, Any.{};
}



Dynamic_Procedure_Callback :: struct {
    using procedure_info:   Procedure_Info;
    procedure_pointer:      *void;
    arguments_provided:     [] Any;
}

call :: (callback: Dynamic_Procedure_Callback, returns: [] Any) -> bool {
    arguments := array_copy(callback.arguments_provided);
    for callback.procedure_info.arguments {
        // if value pointer is null, that means we need a default value
        if arguments[it_index].value_pointer == null {
            if it.get_default == null {
                log("Error: Expected a valid get_default() for argument '%' in dynamic procedure callback.", );
            }
            arguments[it_index] = it.get_default();
        }
    }
    return callback.dynamic_call_wrapper(callback.procedure_pointer, arguments, returns, true);
}

// TODO: We should have a valid code path for when procedure call expression does not allow us to get a Procedure_Info,
//       since we still want to be able to make a dynamic procedure call where arguments are passed purely by index.
// TODO: check arguments_sorted, see what this provides. are unfilled formal arguments null? are they 
// TODO: also check what happend if we have varargs arguments in procedure_call_expression. do varargs get grouped into array before we get the nodes, or does that happen later?
make_dynamic_procedure_callback :: ($procedure_call_expression: Code) -> Dynamic_Procedure_Callback #expand {
    /* 
        TODO: any way to simplify this? This is really gross...
        we can't just do the following, because #procedure_of_call evaluates before the #insert:
            PROCEDURE_OF_CALL :: #procedure_of_call #insert procedure_call_expression;
        so instead we have to do a wonky scoped insert that contains the #procedure_of_call internally
    */
    PROCEDURE_OF_CALL :: #insert,scope(procedure_call_expression) -> string {
        builder: String_Builder;
        append(*builder, "#procedure_of_call ");
        Program_Print.print_expression(*builder, compiler_get_nodes(procedure_call_expression));
        append(*builder, ";\n");
        return builder_to_string(*builder);
    }
    
    PROCEDURE_INFO :: #run get_procedure_info(PROCEDURE_OF_CALL);
    PROCEDURE_TYPE_INFO :: type_info(#run PROCEDURE_INFO.type);
    
    callback := Dynamic_Procedure_Callback.{
        procedure_info     = PROCEDURE_INFO,
        procedure_pointer  = xx PROCEDURE_OF_CALL,
        arguments_provided = NewArray(PROCEDURE_INFO.arguments.count, Any)
    };
    
    #insert -> string {
        proc_call := compiler_get_nodes(procedure_call_expression).(*Code_Procedure_Call);
        
        // TODO: allow matching arguments by index
        for provided: proc_call.arguments_unsorted {
            assert(provided.name != null, "Arguments to make_dynamic_procedure_callback must be provided by name.");
        }
        
        // TODO: by using arguments_sorted, we should be able to avoid a lot of complicaiton around matching parameters manually, because the ocmpiler just does that
        // so we should leverage that here, but still will need to manually check if context dereferneces are used as default values
        // and will probably need special formatting for make_varargs case as well
        
        builder: String_Builder;
        for formal, formal_index: PROCEDURE_INFO.arguments {
            for provided: proc_call.arguments_unsorted {
                if provided.name.name == formal.name {
                    print(*builder, "callback.arguments_provided[%] = ", formal_index);
                    
                    do_cast := is_numeric_type(provided.expression.type);
                    if do_cast  append(*builder, "(");
                    Program_Print.print_expression(*builder, provided.expression);
                    if do_cast  print(*builder, ").(%)", as_type(provided.expression.type));
                    
                    append(*builder, ";\n");
                    continue formal;
                }
            }
            
            // NOTE: We check that a default value exists here, but the actual call to get_default will occur at the call site.
            //       This is done so that we have the correct context when evaluating context-dependent defaults values.
            assert(formal.flags & .HAS_DEFAULT_VALUE != 0, "Argument '%' was not provided to make_dynamic_procedure_callback, and there is no default value.", formal.name);
        }
        return builder_to_string(*builder);
    }
    
    return callback;
}




#scope_file

#import "Compiler";
Program_Print :: #import "Program_Print";


// with_builder :: ($code: Code) -> string {
//     builder: String_Builder;
    
//     // find builder argument and pass this builder, then insert code nodes
//     #insert -> Code {
        
//     }
    
//     return builder_to_string(*builder);
// }

print_code_nodes :: (builder: *String_Builder, node: *Code_Node, indent := 0) {
    if node == null  return;

    put :: (format: string, args: ..Any, newline := true) #expand {
        for 1..indent  append(builder, "  ");
        print(builder, format, ..args);
        if newline  append(builder, "\n");
    }
    
    put("kind: %", node.kind);
    put("type: %", as_type(node.type));
    // put("expression: ", newline = false);
    // Program_Print.print_expression(builder, node);
    // append(builder, "\n");
    
    if node.kind == {
      case .LITERAL;
        literal := node.(*Code_Literal);
        put("value_type: %", literal.value_type);
        // TODO: get_literal_value_as_any, print that here. 
        
      case .IDENT;
        ident := node.(*Code_Ident);
        put("name: %", ident.name);
        put("resolved_declaration: %", ident.resolved_declaration);
        print_code_nodes(builder, ident.resolved_declaration, indent + 1);
        // TODO: maybe we should collect all resolved declarations and display them at the end
        //       could just show the pointer or serial of the resolved declaration here
        
      case .DECLARATION;
        decl := node.(*Code_Declaration);
        print_code_nodes(builder, decl.expression, indent + 1);
        
        // Program_Print.print_expression(builder, decl.expression);
        
      case .DIRECTIVE_INSERT;
        insert := node.(*Code_Directive_Insert);
        print_code_nodes(builder, insert.expansion, indent + 1);
        
      case .UNARY_OPERATOR;
        op := node.(*Code_Unary_Operator);
        str := operator_to_string(xx op.operator_type);
        str = ifx str else string.{ 1, xx *op.operator_type };
        put("operator_type: %", );
        put("subexpression:");
        print_code_nodes(builder, op.subexpression, indent + 1);
        
      case .BINARY_OPERATOR;
        op := node.(*Code_Binary_Operator);
        str := operator_to_string(xx op.operator_type);
        str = ifx str else string.{ 1, xx *op.operator_type };
        put("operator_type: %", str);
        put("left:");
        print_code_nodes(builder, op.left, indent + 1);
        put("right:");
        print_code_nodes(builder, op.right, indent + 1);
        
      case .PROCEDURE_CALL;
        call := node.(*Code_Procedure_Call);
        put("procedure_expression: %", call.procedure_expression);
        print_code_nodes(builder, call.procedure_expression, indent + 1);
        put("resolved_procedure_expression: %", call.resolved_procedure_expression);
        print_code_nodes(builder, call.resolved_procedure_expression, indent + 1);
        // put("overloads:");
        // for overloads.* {
            // print_code_nodes(builder, it, indent + 1);
        // }
        if call.flags {
            put("flags: %", call.flags);
            if call.flags & .INLINE_YES { put("  INLINE_YES"); }
            if call.flags & .INLINE_NO { put("  INLINE_NO"); }
            if call.flags & .RETURNS_PROCEDURE_POINTER_ONLY { put("  RETURNS_PROCEDURE_POINTER_ONLY"); }
            if call.flags & .NO_DEBUG { put("  NO_DEBUG"); }
            if call.flags & .IS_MODULE_PARAMETERS { put("  IS_MODULE_PARAMETERS"); }
        }
        
      case .DIRECTIVE_BAKE;
        bake := node.(*Code_Directive_Bake);
        put("procedure_call:");
        print_code_nodes(builder, bake.procedure_call, indent + 1);
    }
}
