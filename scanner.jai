
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

// modified version of string_to_int from Simple_String.jai
// basically just hardcoded for u64, since we can always convert from there down to any other int type
// honestly not sure why one would want that version to be polymorphic in the first place?
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
    advance(*s, cursor);    // shouldn't this be illegal? modifying an immutable argument?
    
    return sum, success, s;
}


/*
    TODO: support octal numbers
    TODO: support hexadecimal floats
*/
parse_number :: (s: string) -> result: Small_Any, success: bool, remainder: string {    
    if !s {
        log("Error: empty string in parse_number!");
        return .{}, false, "";
    }
    
    // if we've got a decimal point in there, we will prefer float64
    // else we parse and store it as an integer below
    if contains(s, #char ".") {
        val, ok, remainder := string_to_float64(s);
        if !ok  log("Error: failed to parse float from '%'.", s);
        return Small_Any.from(val), ok, remainder;
    }
    
    str := s;
    
    negate := false;
    if str && str[0] == #char "-" {
        negate = true;
        advance(*str, 1);
    }
    
    base: u64 = 10;
    // if number begins with 0b, 0h, 0x, etc..
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
        return Small_Any.from(s64_value), true, remainder;
    } else {
        // use s64 unless u64 value would be out of range
        ret := Small_Any.from(u64_value);
        if u64_value <= S64_MAX.(u64) then ret.type = type_info(s64);
        return ret, true, remainder;
    }
}

#import "String";
#import "Math";


parse_escape_sequence :: (str: string) -> value: string, success: bool, sequence_length: int {
    if str.count < 2 || str[0] != #char "\\" {
        return "", false, 0;
    }
    
    if str[1] == {
      case #char "e";   return "\e", true, 2;
      case #char "n";   return "\n", true, 2;
      case #char "r";   return "\r", true, 2;
      case #char "t";   return "\t", true, 2;
      case #char "0";   return "\0", true, 2;
      case #char "\"";  return "\"", true, 2;
      case #char "\\";  return "\\", true, 2;
      
      case #char "x";
        if str.count < 4 {
            log("Error: unexpected EOF in hexadecimal escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 4)), true, 2;
        
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
        return tprint(slice(str, 0, 5)), true, 2;
        
      case #char "u";
        if str.count < 6 {
            log("Error: unexpected EOF in 16-bit unicode escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 4)), true, 2;
        
      case #char "U";
        if str.count < 6 {
            log("Error: unexpected EOF in 32-bit unicode escape sequence: '%'", str);
            return "", false, 0;
        }
        return tprint(slice(str, 0, 6)), true, 2;
    }
    
    log("Error: invalid escape sequence beginning with: '%'", slice(str, 0, 2));
    return "", false, 0;
}

parse_escape_sequence :: (scanner: *Scanner) -> value: string, success: bool {
    value, ok, len := parse_escape_sequence(scanner.file);
    if ok  advance(scanner, len);
    return value, ok;
}

