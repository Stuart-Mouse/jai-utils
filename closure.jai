
// Using a polymorphic struct here to hopefully reduce code bloat since the compiler should deduplicate based on procedure type.
// Otherwise we would re-generate the Closure struct every time we create a new closure.
// This also allows other polymorphic procedures to accept a Closure rather than a $T.
// Closure :: struct(PROCEDURE_TYPE: Type) {
//     proc:   PROCEDURE_TYPE; // pointer to procedure
//     data:   *void;          // pointer to generated capture struct
// }

Closure :: struct {
    proc:   *void; // pointer to procedure
    data:   *void; // pointer to generated capture struct
}

/*
    closure_data :: struct {
        vtable: struct {
            init_captures: (*void);
            override_capture: (name: string, data: *void);
        },
        
        captures: struct {
            // ...
        }
    }
    
    closure := make_closure({ foo = *foo }, (i: int) -> int, #code {
        foo.* += i;
        log("foo is %", foo.*);
    });
*/

make_closure :: ($signature: Code, $captures: Code, $body: Code) -> Closure #expand {
    #insert,scope(body) -> Code {
        push_allocator(temp);
        auto_release_temp();
        
        signature_header := compiler_get_nodes(signature).(*Code_Procedure_Header);
        assert(signature_header.kind == .PROCEDURE_HEADER);
        
        procedure_decl := New(Code_Declaration);
        {
            header := New(Code_Procedure_Header);
            body   := New(Code_Procedure_Body);
            block  := New(Code_Block);
            
            header.body_or_null = body;
            body.header = header;
            body.block = block;
            
            procedure_decl.name = "PROCEDURE";
            procedure_decl.expression = header;
            procedure_decl.flags |= .IS_CONSTANT;
            
            header.arguments = NewArray(signature_header.arguments.count + 1, *Code_Declaration);
            header.arguments[0] = New(Code_Declaration);
            header.arguments[0].name = "closure";
            header.arguments[0].type_inst = make_type_inst_pointer_to(make_type_inst_from_ident("Closure"));
            
            for signature_header.arguments {
                header.arguments[it_index+1] = it;
            }
            
            header.returns = signature_header.returns;
            
            block.statements = NewArray(2, Code_Node);
            
            // R1, R2, ... := closure.proc(A1, A2, ...);
            block.statements[0] = ifx 1 {
                decl := New(Code_Compound_Declaration);
                decl.comma_separated_assignment = New(Code_Comma_Separated_Arguments);
                decl.comma_separated_assignment.arguments = NewArray(header.returns.count, Code_Comma_Separated_Argument);
                for header.returns {
                    decl.comma_separated_assignment.arguments[it_index] = make_ident(tprint("R%", it_index));
                }
                
                call_arguments: NewArray(header.arguments.count, Code_Argument);
                call_arguments[0] = make_ident("closure");
                for header.arguments {
                    call_arguments[it_index+1].expression = make_ident(it.name);
                }
                
                decl.declaration_properties = New(Code_Declaration);
                decl.declaration_properties.expression = make_procedure_call(
                    make_dot_dereference(make_ident("closure"), "proc"),
                    ..call_arguments,
                );
                
                decl;
            }
            
            // return R1, R2, ...;
            block.statements[1] = ifx 1 {
                return_statement := New(Code_Return);
                return_statement.arguments_unsorted = NewArray(header.returns.count, Code_Argument);
                for header.returns {
                    return_statement.arguments_unsorted[it_index] = make_ident(tprint("R%", it_index));
                }
                
                return_statement;
            }
        }
        
        
        // init_capture_decl := New(Code_Declaration);
        // {
        //     header := New(Code_Procedure_Header);
        //     body   := New(Code_Procedure_Body);
        //     block  := New(Code_Block);
            
        //     header.body_or_null = body;
        //     body.header = header;
        //     body.block = block;
            
        //     init_capture_decl.name = "init_capture";
        //     init_capture_decl.expression = header;
        //     init_capture_decl.flags |= .IS_CONSTANT;
            
            
            
        // }
        
        
        result := compiler_get_nodes(#code using struct #type_info_none { });
        result.(*Code_Using).expression.(*Code_Struct).block.statements[0] = make_block(procedure_decl);
        
        // Uncomment this to see what the generated code looks like before it is inserted.
        sb: String_Builder;
        print_expression(*sb, result);
        write_string("----------------------------\n");
        write_string(builder_to_string(*sb));
        write_string("\n");
        
        return compiler_get_code(result);
    };
    
    closure: Closure(type_of(PROCEDURE));
    closure.init_capture(*closure);
    return closure;
}



/*

#import "Utils";
main :: () {
    sum: int;
    
    closure := make_closure({ sum = *sum }, #code (i: int) -> (int), #code {
        sum.* += i;
        log("foo is %", sum.*);
    });
    
    
    closure.call(closure);
}

*/