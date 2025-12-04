
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
        // if (call.flags & .RETURNS_PROCEDURE_POINTER_ONLY)
        // || (call.flags & 0x400) {  // this means procedure_of_call was used
            return get_procedure_header(call.resolved_procedure_expression);
        // }
    }
    return null;
}

Procedure_Info :: struct {
    type:           Type;
    arguments:      [] Argument;
    returns:        [] Argument;
    call_wrapper:   Procedure_Call_Wrapper;
    
    #overlay(type) _type_info: *Type_Info_Procedure;
    
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

get_procedure_info :: ($procedure_expression: Code) -> Procedure_Info #expand {
    // We bake these arguments for each argument that has a default value.
    get_default :: ($T: Type, $code: Code) -> Any {
        ret := New_Any(xx T, initialized = false);
        ret.value_pointer.(*T).* = #insert,scope(procedure_expression) code;
        return ret;
    }
    
    #insert -> string {
        builder: String_Builder;
        
        root := compiler_get_nodes(procedure_expression);
        procedure_header := get_procedure_header(root);
        
        if procedure_header == null {
            append(*builder, "Warning: Unable to resolve procedure header from expression: ");
            Program_Print.print_expression(*builder, root);
            log(builder_to_string(*builder));
            return "return .{};";
        }
        
        assert(procedure_header.procedure_flags & .POLYMORPHIC == 0, "Cannot get procedure info for a polymorphic procedure.");
        
        procedure_type_info := procedure_header.type.(*Type_Info_Procedure);
        append(*builder, "procedure :: #insert procedure_expression;\n");
        append(*builder, "procedure_type_info :: type_info(type_of(procedure));\n");
        for procedure_type_info.argument_types {
            print(*builder, "Ta%1 :: #run (*procedure_type_info.argument_types[%1]).(*Type).*; // %\n", it_index, as_type(it));
        }
        
        // generate arguments array
        {
            append(*builder, "arguments :: Procedure_Info.Argument.[\n");
            for procedure_header.arguments {
                print(*builder, "    .{ name = \"%\", ", it.name);
                
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
                    handled := false;
                    if it.expression.kind == {
                      case .DIRECTIVE_LOCATION;
                        code_directive_location := it.expression.(*Code_Directive_Location);
                        if code_directive_location.is_caller_location { 
                            print(*builder, "get_default = #bake_arguments get_default(T = Ta%, code = #location(procedure_expression))", it_index);
                            handled = true;
                        }
                    }
                    if !handled {
                        print(*builder, "get_default = #bake_arguments get_default(T = Ta%, code = #code ", it_index);
                        Program_Print.print_expression(*builder, it.expression);
                        append(*builder, ")");
                    }
                }
                
                append(*builder, "},\n");
            }
            append(*builder, "];\n");
        }
        
        // generate return values array
        {
            append(*builder, "returns :: Procedure_Info.Argument.[\n");
            for procedure_header.returns {
                print(*builder, "    .{ name = \"%\" },\n", it.name);
            }
            append(*builder, "];\n");
        }
        
        return builder_to_string(*builder);
    }
    
    call_wrapper :: #bake_arguments call_procedure_checked(P = type_of(procedure));
    
    return .{ 
        type_of(procedure), 
        arguments, 
        returns, 
        xx call_wrapper 
    };
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


#scope_module

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

