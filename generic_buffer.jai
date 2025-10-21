
// NOTE: when I actually reimplement this, it should be independent from resiable array or array view
//       any configurable functionality will just be flags or paramters on the Generic_Buffer itself

// Generic_Buffer :: struct {
//     flags: enum_flags { ALLOW_RESIZE; }
    
    
    
//     written:        int;
//     last_get_size:  int;
    
//     from :: make_generic_buffer;
// }

// get_bytes :: (buffer: *Data_Packer, size: int) -> *void {
//     last_get_size = size;
    
//     if packer.buffer_type == {
//       case .VIEW;
//         if buffer.written + size > buffer.view.count  return null;
//         result := buffer.view.data + buffer.written;
//         buffer.written += size;
//         return result;
        
//       case .RESIZABLE;
//         result := array.data;
//         array_resize(buffer.resizable, buffer.resizable.count + size);
//         buffer.written += size;
//         return result;
//     }
//     unreachable();
// }

// get_bytes_as_view :: inline (buffer: *Data_Packer, size: int) -> [] u8 {
//     data := get_bytes(buffer, size);
//     result: [] u8;
//     result.data  = data;
//     result.count = ifx data then size;
//     return result;
// }

// make_generic_buffer :: (view: [] u8) -> Generic_Buffer {
//     return .{ kind = .VIEW, view = view };
// }

// make_generic_buffer :: (view: [] u8) -> Generic_Buffer {
//     return .{ kind = .VIEW, view = view };
// }

// skip_bytes :: (buffer: *Generic_Buffer, size: int) -> int {
//     data := get_bytes(buffer, size);
//     if data == null  return -1;
    
//     base: *u8;
//     if buffer.kind == {
//       case .VIEW;       base = buffer.view.data;
//       case .RESIZABLE;  base = buffer.resizable.data;
//     }
//     return data - base;
// }

// get_appended_slice :: (array: *[..]$T, count: int) -> []T {
//     result: [] T;
//     result.data  = array.data + array.count;
//     result.count = count;
//     array_resize(array, array.count + count);
//     return result;
// }
