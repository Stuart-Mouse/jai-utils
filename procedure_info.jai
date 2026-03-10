
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
        
    TODO: eventually move this to the Reflect module
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
    __type:         Type;
    #overlay(__type) using __type_info: *Type_Info_Procedure;
    
    arguments:      [] Argument;
    returns:        [] Argument;
    
    // TODO: maybe we should have separate arrays for each rather than an array of structs
    //       main reason being that we already have argument/return types in a separate array in _type_info
    Argument :: struct {
        name:   string;
        flags:  Flags;
        
        Flags :: enum_flags { 
            NONE :: 0; 
            HAS_DEFAULT_VALUE :: 1; 
            IS_VARARGS;
        }
    }
    
    get_default_argument_value: (index: int, provided_storage := null) -> bool, Any;
    
    notes: [] string;
}

/*
    Generates a constant Procedure_Info and returns a pointer to that struct.
    
*/
get_procedure_info :: ($procedure_expression: Code, scope := #caller_code, loc := #caller_location) -> *Procedure_Info #expand {
    PROCEDURE           :: #insert procedure_expression;
    PROCEDURE_TYPE      :: type_of(PROCEDURE);
    PROCEDURE_TYPE_INFO :: type_info(PROCEDURE_TYPE);
    
    // Dumping all the argument types here is necessary for our code generation below.
    ARGUMENT_TYPE_COUNT :: #run PROCEDURE_TYPE_INFO.argument_types.count;
    ARGUMENT_TYPES :: #run () -> [ARGUMENT_TYPE_COUNT] Type {
        types: [ARGUMENT_TYPE_COUNT] Type;
        for PROCEDURE_TYPE_INFO.argument_types {
            types[it_index] = get_type(it);
        }
        return types;
    }();
    
    ARGUMENTS, RETURNS, NOTES :: #run () -> ([] Procedure_Info.Argument, [] Procedure_Info.Argument, [] string) {
        root := compiler_get_nodes(procedure_expression);
        procedure_header := get_procedure_header(root);
        
        arguments := NewArray(procedure_header.arguments.count, Procedure_Info.Argument,, temp);
        for procedure_header.arguments {
            flags: Procedure_Info.Argument.Flags;
            if it.expression {
                flags |= .HAS_DEFAULT_VALUE;
            }
            if it.type_inst && (it.type_inst.inst_flags & .VARARGS) {
                flags |= .IS_VARARGS;
            }
            arguments[it_index] = .{ it.name, flags };
        }
        
        returns := NewArray(procedure_header.returns.count, Procedure_Info.Argument,, temp);
        for procedure_header.returns {
            returns[it_index] = .{ it.name, .NONE };
        }
        
        notes := NewArray(procedure_header.notes.count, string,, temp);
        for procedure_header.notes {
            notes[it_index] = it.text;
        }
        
        return arguments, returns, notes;
    }();
    
    get_default_argument_value :: (index: int, provided_storage := null) -> bool, Any {
        #insert -> Code {
            root := compiler_get_nodes(procedure_expression);
            procedure_header := get_procedure_header(root);
            
            if procedure_header == null {
                builder: String_Builder;
                append(*builder, "Warning: Unable to resolve procedure header from expression: ");
                Program_Print.print_expression(*builder, root);
                
                error_string := builder_to_string(*builder);
                assert(false, error_string);
            }
            
            if procedure_header.procedure_flags & .POLYMORPHIC {
                compiler_report("Cannot get procedure info for a polymorphic procedure. Try using #procedure_of_call or #bake_arguments to monomorph the procedure before passing it to get_procedure_info.", loc = loc);
            }
            
            cases: [..] *Code_Case;
            cases.allocator = temp;
            array_reserve(*cases, procedure_header.arguments.count);
            
            for procedure_header.arguments {
                if it.expression {
                    argument_type_expression := compiler_get_nodes(#code ARGUMENT_TYPES[0]);
                    argument_type_expression.(*Code_Binary_Operator).right.(*Code_Literal)._s64 = it_index;
                    
                    code_case := New(Code_Case);
                    code_case.condition = make_integer_literal(it_index);
                    
                    code_case.then_block = compiler_get_nodes(#code {
                        ret := New(void, initialized = false);
                        ret.* = #insert,scope(scope) #code 0;
                        return true, ret.*;
                    }).(*Code_Block);
                    
                    // replace `void` with actual argument type
                    code_case.then_block
                        .statements[0].(*Code_Declaration)
                        .expression.(*Code_Procedure_Call)
                        .arguments_unsorted[0].expression = argument_type_expression;
                    
                    // we need to specially replace the #caller_location directive, since it cannot be used as a general expression
                    if it.expression.kind == .DIRECTIVE_LOCATION
                    && it.expression.(*Code_Directive_Location).is_caller_location {
                        code_case.then_block
                            .statements[1].(*Code_Binary_Operator)
                            .right = compiler_get_nodes(#code #location(procedure_expression));
                    } else {
                        // replace `#code 0` in insert with it.expression
                        code_case.then_block
                            .statements[1].(*Code_Binary_Operator)
                            .right.(*Code_Directive_Insert)
                            .expression.(*Code_Directive_Code)
                            .expression = it.expression;
                    }
                    
                    array_add(*cases, code_case);
                }
            }
            
            code_if := New(Code_If);
            code_if.if_flags = .IS_SWITCH_STATEMENT;
            
            code_if.condition = New(Code_Ident);
            code_if.condition.(*Code_Ident).name = "index";
            
            code_if.then_block = ifx 1 {
                block := New(Code_Block);
                block.block_type       = .IMPERATIVE;
                block.owning_statement = code_if;
                block.statements       = cast,force([]*Code_Node) cases;
                for cases {
                    it.then_block.owning_statement = it.then_block.statements[0];
                    it.then_block.parent = block;
                }
                block;
            };
            
            return compiler_get_code(code_if);
        }
        
        return false, Any.{};
    }
    
    PROCEDURE_INFO :: Procedure_Info.{
        __type                      = PROCEDURE_TYPE, 
        arguments                   = ARGUMENTS, 
        returns                     = RETURNS, 
        get_default_argument_value  = get_default_argument_value,
        notes                       = NOTES,
    };
    
    return *PROCEDURE_INFO;
}

// get_default_argument_value :: (info: Procedure_Info, argument_name: string) -> bool, Any {
//     for info.arguments {
//         if it.name == argument_name { 
//             if !(it.flags & .HAS_DEFAULT_VALUE)  return false, Any.{};
//             return true, it.get_default();
//         }
//     }
//     return false, Any.{};
// }


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

// TODO: move to some compiler utils module, eventually
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

