
Scanner :: struct {
    file: string;
    using location: Source_Code_Location;
}

init_scanner :: (scanner: *Scanner, file: string, file_path: string) {
    scanner.file     = file;
    scanner.location = .{ file_path, 1, 1 };
}

make_scanner :: (file: string, file_path: string) -> Scanner {
    scanner: Scanner;
    init_scanner(scanner, file, file_path);
    return scanner;
}

// advances UP TO the amount specified, returns false on EOF
advance :: inline (using scanner: *Scanner, amount := 1) -> bool {
    assert(amount >= 0);
    _amount := min(amount, file.count);
    
    for 0.._amount-1 {
        if file[it] == #char "\n" {
            location.line_number += 1;
            location.character_number = 1;
        } else {
            location.character_number += 1;
        }
    }
    
    file.data  += _amount;
    file.count -= _amount;
    
    return file.count > 0; // return false when we hit EOF
}

is_hex_digit :: (c: u8) -> bool {
    return c >= #char "0" && c <= #char "9"
        || c >= #char "a" && c <= #char "f"
        || c >= #char "A" && c <= #char "F";
}

/*
    NOTE:
        permits a leading '-' or '+' for sign
        currently does not report any kind of unexpected EOF or unexpected character error if a number suddenyl ends after some base specifier digit or a decimal point
            this appears to be how Jai lexes things, so I've copied that behaviour for consistency
            (although, jai does seem to make an exception for 0h hex floats, which will report an error if not followed by some hex digits. This is presumably because these numbers cannot be of an arbitrary length (must be 4, 8, or 16 digits) and so are parsed more rigorously)
        permits underscores anywhere in a number (except as first character, obviously), including directly after the decimal point
        
    TODO:
        handle underscores
            only in int? or float also? copy what jai permits
        maybe remove handling for sign, let caller handle if they wish
        probably just return empty string on error and no second bool return
            see what jai does when we have some weird thing like number ending with dot and no following digits
        
*/
try_lex_number :: (using scanner: *Scanner) -> string {
    if file && (is_digit(file[0]) || (file.count > 1 && (file[0] == #char "+" || file[0] == #char "-") && is_digit(file[1]))) {
        text := string.{ 0, file.data };
        
        if file[0] == #char "0" && advance(scanner) {
            if file[0] == {
              case #char "b"; 
                if advance(scanner) {
                    while (file[0] == #char "0" || file[0] == #char "1" || file[0] == #char "_") && advance(scanner) {}
                }
                text.count = file.data - text.data;
                return text;                
                
              case #char "h"; #through;
              case #char "x"; 
                if advance(scanner) {
                    while (is_hex_digit(file[0]) || file[0] == #char "_") && advance(scanner) {}
                }
                text.count = file.data - text.data;
                return text;
            }
        }
        
        while (is_digit(file[0]) || file[0] == #char "_") && advance(scanner) {};
        
        if file && file[0] == #char "." && advance(scanner) {
            while (is_digit(file[0]) || file[0] == #char "_") && advance(scanner) {};
        }
        
        text.count = file.data - text.data;
        return text;
    }
    return "";
}


// ========== Escape Sequences ==========

// TODO: figure out some way to avoid all the use of tprint. 
// We can probably just have user provide a fixed-size buffer for us to work with here.
parse_escape_sequence :: (str: string, quote_char: u8 = #char "\"") -> value: string, success: bool, sequence_length: int {
    if str.count < 2 || str[0] != #char "\\" {
        return "", false, 0;
    }
    
    if str[1] == quote_char {
        // this is silly
        return copy_string(string.{ 1, *quote_char },, temp), true, 2;
    }
    
    if str[1] == {
      case #char "e";   return "\e", true, 2;
      case #char "n";   return "\n", true, 2;
      case #char "r";   return "\r", true, 2;
      case #char "t";   return "\t", true, 2;
      case #char "0";   return "\0", true, 2;
      case #char "\\";  return "\\", true, 2;
      
      case #char "x";
        if str.count < 4 {
            log("Error: unexpected EOF in hexadecimal escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 4)), true, 4;
        
      case #char "d";
        if str.count < 5 {
            log("Error: unexpected EOF in decimal escape sequence: '%'", str);
            return "", false, 0;
        }
        val, ok, remainder := string_to_u64(slice(str, 2, 3));
        if !ok || remainder {
            log("Error: invalid decimal escape sequence: '%'", str);
            return "", false, 0;
        }
        if val > 255 {
            log("Error: value of decimal escape sequence cannot be greater than 255: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 5)), true, 5;
        
      case #char "u";
        if str.count < 6 {
            log("Error: unexpected EOF in 16-bit unicode escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 6)), true, 6;
        
      case #char "U";
        if str.count < 10 {
            log("Error: unexpected EOF in 32-bit unicode escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 10)), true, 10;
    }
    
    log("Error: invalid escape sequence beginning with: '%'", slice(str, 0, 2));
    return "", false, 0;
}

parse_escape_sequence :: (scanner: *Scanner) -> value: string, success: bool {
    value, ok, len := parse_escape_sequence(scanner.file);
    if ok  advance(scanner, len);
    return value, ok;
}


// Feels poorly named, but not sure what else to call it.
convert_escape_sequences :: (builder: *String_Builder, text: string, quote_char: u8 = #char "\"") -> bool {
    _text := text;
    while _text {
        if _text[0] == #char "\\" {
            str, ok, len := parse_escape_sequence(_text, quote_char);
            if !ok  return false;
            append(builder, str);
            advance(*_text, len);
        } else {
            append(builder, _text[0]);
            advance(*_text, 1);
        }
    }
    return true;
}

convert_escape_sequences :: (text: string) -> string, bool {
    builder: String_Builder;
    ok := convert_escape_sequences(*builder, text);
    if !ok {
        free_buffers(*builder);
        return "", false;
    }
    return builder_to_string(*builder), true;
}

// Feels poorly named, but not sure what else to call it.
revert_escape_sequences :: (builder: *String_Builder, text: string, quote_char: u8 = #char "\"") -> bool {
    if !text  return true;
    
    // for text {
    for :utf8_iter text {
        if it == {
          case #char "\e";  append(builder, "\\e");  continue;
          case #char "\n";  append(builder, "\\n");  continue;
          case #char "\r";  append(builder, "\\r");  continue;
          case #char "\t";  append(builder, "\\t");  continue;
          case #char "\0";  append(builder, "\\0");  continue;
          case #char "\\";  append(builder, "\\\\"); continue;
        }
        
        if it == quote_char {
            buf := u8.[ #char "\\", quote_char ];
            append(builder, xx buf);
            continue;
        }
        
        // if outside of printable ascii range, append as hex escape sequence
        if it < 0x20 || it == 0x7f {
            append(builder, "\\x");
            append(builder, xx byte_to_ascii_hex(it.(u8)));
            continue;
        }
        
        // TODO: append only "printable" utf8 characters as-is, without escaping
        //       may have to consult what Go considers to be the printable subset of utf8
        // TODO: also figure out how to properly convert utf32 value back into \u or \U codepoint if not printable
        //       I think this will require reversing the byte order before doing bytes_to_asii_hex
        buffer: [8] u8;
        str := string.{ 0, buffer.data };
        character_utf32_to_utf8(it, *str);
        append(builder, str);
    }
    
    return true;
}

revert_escape_sequences :: (text: string) -> string, bool {
    builder: String_Builder;
    ok := revert_escape_sequences(*builder, text);
    if !ok {
        free_buffers(*builder);
        return "", false;
    }
    return builder_to_string(*builder), true;
}


// ========== Number Parsing ==========

/*
    string_to_u64 and parse_number are primarily written for use with Convert.jai, 
    but I don't really want to put them in that module right now since they're just a little too general.
    At the very least, they probably won't stay in this file forever...
    
    Instead of using a polymorphic procedure like string_to_int, we parse to u64 as our fundamental integer type 
    and then do whatever we need to do to downcast in the calling procedure. I think this approach makes 
    more sense in this module since the dst type for the integer is probably not statically known anyhow.
        
    Note that we do not have any special procedures in this module for string_to_float, 
    since there's no point in not just using what's provided in string_to_float.jai.
    
    See parse_number below this to see how this procedure is used in context, and how we then downcast to the other integer types.
*/


string_to_u64 :: (text: string, base: u64 = 10) -> result: u64, success: bool, remainder: string {
    assert(base == 16 || base <= 10);
    if !text  return 0, false, "";
    
    s := text;
    sum: u64 = 0;
    cursor := 0;
    
    if base == 16 {
        while cursor < s.count {
            defer cursor += 1;
            c := s[cursor];
            if c == #char "_"  continue;

            digit: u8 = ---;
            if is_digit(c) {
                digit = c - #char "0";
            } else if (c >= #char "a") && (c <= #char "f") {
                digit = c - #char "a" + 10;
            } else if (c >= #char "A") && (c <= #char "F") {
                digit = c - #char "A" + 10;
            } else {
                break;
            }
            
            sum *= base;
            sum += digit.(u64);
        }
    } else {
        while cursor < s.count {
            defer cursor += 1;
            c := s[cursor];
            if c == #char "_"  continue;

            if !is_digit(c) break;
            
            digit := c - #char "0";
            if digit >= base break;
            
            prev := sum;
            
            #no_aoc {
                sum *= base; 
                sum += digit.(u64);
            }
            
            if sum < prev {
                log("Error: arithmetic overflow in string_to_u64!");
                return 0, false, "";
            }
        }
    }
    
    success := (cursor != 0);
    advance(*s, cursor);
    
    return sum, success, s;
}


/*
    The result of parse_number will always be either float64, u64, or s64
        float64 is used if the input string contains a dot
        s64 will be used as the default integer type
        u64 will be used only if the u64 value parsed by string_to_u64 would not fit inside of an s64
    
    Note to self: 
        Convert.jai has its own more constrained version (string_to_int) and a more general use case set_value_from_string procedure.
        I see this procedure as being most useful for parsers like GON or Lead Sheets, where you really need the Any_Number as an intermediate value for an expented period of time.
        
    TODO: support octal numbers
    TODO: support hexadecimal floats
*/
parse_number :: (s: string) -> result: Any_Number, success: bool, remainder: string {    
    if !s {
        log("Error: empty string in parse_number!");
        return .{}, false, "";
    }
    
    // if we've got a decimal point in there, we will prefer float64
    // else we parse and store it as an integer below
    if contains(s, #char ".") {
        val, ok, remainder := string_to_float64(s);
        if !ok  log("Error: failed to parse float from '%'.", s);
        return Any_Number.from(val), ok, remainder;
    }
    
    str := s;
    
    negate := false;
    if str && str[0] == #char "-" {
        negate = true;
        advance(*str, 1);
    }
    
    base: u64 = 10;
    if str[0] == #char "0" && str.count >= 2 {
        if str[1] == {
          case #char "b"; 
            advance(*str, 2);
            base = 2;
            
          case #char "x"; 
            advance(*str, 2);
            base = 16;
            
          // case #char "h";
          //   advance(*str, 2);
          //   base = 16;
          //   is_hex_float = true;
          //   // verify that input string is exactly 4, 8, or 16 characters
        }
    }
    
    u64_value, ok, remainder := string_to_u64(str, base);
    if !ok {
        log("Error: failed to parse integer from '%'.", s);
        return .{}, false, remainder;
    }
    
    if negate {
        if u64_value > S64_MAX.(u64) + 1 {
            log("Error: cannot negate parsed u64 value, as it would be out of range for s64.");
            return .{}, false, remainder;
        }
        s64_value := -(u64_value-1).(s64) - 1;
        return Any_Number.from(s64_value), true, remainder;
    } else {
        // use s64 unless u64 value would be out of range
        ret := Any_Number.from(u64_value);
        if u64_value <= S64_MAX.(u64) then ret.type = type_info(s64);
        return ret, true, remainder;
    }
}


// ========== ASCII Hexadecimal to Binary Conversions ==========

ascii_hex_char_to_byte :: (c: u8) -> u8 {
    if c >= #char "0" && c <= #char "9" then return  c - #char "0";
    if c >= #char "A" && c <= #char "F" then return (c - #char "A") + 10;
    if c >= #char "a" && c <= #char "f" then return (c - #char "a") + 10;
    return 0;
}

// // TODO: this procedure is too big and complicated, with too many parameters!
// //       we should split it up into multiple more focused procedures
// ascii_hex_to_bytes_buf :: (
//     dst:        []u8, 
//     src:        string, 
//     pad_type:   Data_Pad_Type = .NONE, 
//     pad_byte:   u8            = 0, 
//     big_endian := false // endianness of dst
// ) -> int {
//     src := transmute([]u8) src
    
//     pad_type := pad_type

//     src_len_is_odd := (len(src) & 1) == 1
//     if src_len_is_odd {
//         if pad_type == .NONE {
//             return -1
//         }
//     } else {
//         pad_type = .NONE
//     }
    
//     len_bytes := len(src) / 2 + int(src_len_is_odd)
//     if len(dst) < len_bytes {
//         return -1
//     }
    
//     nib1_shift        : uint = 4
//     nib2_shift        : uint = 0
//     nib2_store_offset :  int = 0
    
//     end := len_bytes
    
//     #partial switch pad_type {
//       case .LEFT:
//         // for left pad, switch the shifts of each source nibble and store second nibble in the next byte
//         nib1_shift = 0
//         nib2_shift = 4
//         nib2_store_offset = 1
        
//         dst[0] = (0xF0 & pad_byte)
//         fallthrough
            
//       case .RIGHT:
//         // for both left and right cases, stop one byte short of the end and write the last byte manually
//         end -= 1
//     }
    
//     src_i := 0
//     dst_i := 0
//     if big_endian {
//         dst_i = end - 1
//         for dst_i >= 0 {
//             nib1 := ascii_hex_char_to_int(src[src_i  ])
//             nib2 := ascii_hex_char_to_int(src[src_i+1])
//             dst[dst_i                  ] |= nib1 << nib1_shift
//             dst[dst_i+nib2_store_offset] |= nib2 << nib2_shift
            
//             src_i += 2
//             dst_i -= 1
//         }
//     } else {
//         for dst_i < end {
//             nib1 := ascii_hex_char_to_int(src[src_i  ])
//             nib2 := ascii_hex_char_to_int(src[src_i+1])
//             dst[dst_i                  ] |= nib1 << nib1_shift
//             dst[dst_i+nib2_store_offset] |= nib2 << nib2_shift
            
//             src_i += 2
//             dst_i += 1
//         }
//     }
    
//     #partial switch pad_type {
//       case .LEFT:
//         // OR in the last nibble from src
//         dst[dst_i] |= (0x0F & ascii_hex_char_to_int(src[src_i]))
            
//       case .RIGHT:
//         // high nibble is the last nibble from src, low nibble is pad
//         nib1 := ascii_hex_char_to_int(src[src_i])
//         nib2 := (0x0F & pad_byte)
//         dst[dst_i] = (nib1 << 4) | nib2
//     }
    
//     return len_bytes
// }

// ascii_hex_to_bytes :: (src: string, pad_type: Data_Pad_Type = .NONE, pad_byte: u8 = 0, allocator := context.allocator) -> ([]u8, bool) {
//     len_bytes := nibble_len_to_byte_len(len(src))
//     dst := make([]u8, len_bytes, allocator = allocator)
    
//     bytes_written := ascii_hex_to_bytes_buf(dst, src, pad_type, pad_byte)
//     if bytes_written != len(dst) {
//         delete(dst)
//         return {}, false
//     }
    
//     return dst, true
// }

ASCII_HEX_NIBBLES :: "0123456789ABCDEF";

byte_to_ascii_hex :: inline (byte: u8) -> [2] u8 {
    return .[
        ASCII_HEX_NIBBLES[0x0F & (byte >> 4)],
        ASCII_HEX_NIBBLES[0x0F &  byte      ],
    ];
}

// dst buffer must be at least large enough for the src data
bytes_to_ascii_hex :: (dst: [] u8, src: string) -> (bytes_written: int) {   
    output_size := src.count * 2;
    if output_size > dst.count  return -1;
    
    src_char, dst_char: *u8;
    while dst_i < dst.data + output_size {
        dst[dst_i    ] = nibble_to_ascii_hex[0x0F & (src[src_i] >> 4)];
        dst[dst_i + 1] = nibble_to_ascii_hex[0x0F &  src[src_i]      ];
        
        src_char += 1;
        dst_char += 2;
    }
    return output_size;
}

// Technically this can just be an inline proc, but because we are explicitly relying on the
// buffer's storage belonging to the caller's scope, it feels better for this to be a macro.
// with_buffer :: ($BYTE_COUNT: int, $proc: ([]u8, string) -> int, src: string) -> string #expand {
//     #assert(proc != null);
//     buffer: [BYTE_COUNT] u8;
//     bytes_written := proc(buffer, src);
//     assert(bytes_written >= 0 && bytes_written <= BYTE_COUNT);
//     return string.{ bytes_written, *buffer };
// }
