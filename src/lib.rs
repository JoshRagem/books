#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate libc;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));


#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::slice;
    use std::str;
    use std::ffi::CString;
    use std::ptr::null_mut;
    use std::os::raw::c_void;

    #[test]
    fn it_works() {
        unsafe {
            let mut env: MDB_env = mem::zeroed();
            let mut eptr: *mut MDB_env = &mut env;
            let res = mdb_env_create(&mut eptr as *mut _);
            mdb_env_set_maxdbs(eptr, 10);
            assert_eq!(0, res);
            let path = CString::new("/tmp/mdb/").unwrap();
            let res_open = mdb_env_open(eptr, path.as_ptr(), 0, 700);
            // perror(path.as_ptr());
            assert_eq!(0, res_open);
            let mut txn: MDB_txn = mem::zeroed();
            let mut tptr: *mut MDB_txn = &mut txn;
            let res_txn = mdb_txn_begin(eptr, null_mut(), 0, &mut tptr as *mut _);
            assert_eq!(0, res_txn);
            let mut dbi: MDB_dbi = mem::zeroed();
            let db_name = CString::new("test").unwrap();
            let res_dbi = mdb_dbi_open(tptr, db_name.as_ptr(), MDB_CREATE, &mut dbi as *mut _);
            assert_eq!(0, res_dbi);
            let mut crs: MDB_cursor = mem::zeroed();
            let mut cptr: *mut MDB_cursor = &mut crs;
            let res_crs = mdb_cursor_open(tptr, dbi, &mut cptr as *mut _);
            assert_eq!(0, res_crs);
            let mut crs2: MDB_cursor = mem::zeroed();
            let mut cptr2: *mut MDB_cursor = &mut crs2;
            let res_crs2 = mdb_cursor_open(tptr, dbi, &mut cptr2 as *mut _);
            assert_eq!(0, res_crs2);
            let keystr = CString::new("hello").unwrap();
            let keylen: usize = keystr.to_bytes().len();
            let keyptr: *mut c_void = keystr.as_ptr() as *mut c_void;
            let mut keyval: MDB_val = MDB_val { mv_size: keylen, mv_data: keyptr };
            let valstr = "garbage";
            let vallen: usize = mem::size_of_val(valstr);
            let valptr: *mut c_void = valstr.as_ptr() as *mut c_void;
            let mut valval: MDB_val = MDB_val { mv_size: vallen, mv_data: valptr };
            let res_put = mdb_cursor_put(cptr, &mut keyval as *mut _, &mut valval as *mut _, MDB_NOOVERWRITE);
            assert_eq!(0, res_put);
            mdb_cursor_close(cptr);
            mdb_cursor_close(cptr2);
            mdb_txn_commit(tptr);
            let res_txn2 = mdb_txn_begin(eptr, null_mut(), 0, &mut tptr as *mut _);
            assert_eq!(0, res_txn2);
            let mut valnew: MDB_val = MDB_val { mv_size: 0, mv_data: null_mut() };
            mdb_cursor_open(tptr, dbi, &mut cptr as *mut _);
            let res_get = mdb_cursor_get(cptr, &mut keyval as *mut _, &mut valnew as *mut _, MDB_cursor_op::MDB_FIRST);
            assert_eq!(0, res_get);
            assert_eq!(valval.mv_size, valnew.mv_size);
            //let junk = &mut *(valval.mv_data as *mut u8);
            //let junk2 = &mut *(valnew.mv_data as *mut u8);
            //assert_eq!(junk, junk2);
            let junk: &[u8] = slice::from_raw_parts(valval.mv_data as *const u8, valval.mv_size);
            let junkstr = str::from_utf8(junk).unwrap();
            assert_eq!(valstr, junkstr);
            for mut i in 1..100 {
                let mut keyval: MDB_val = MDB_val { mv_size: 1, mv_data: &mut i as *mut _ as *mut c_void };
                let mut valval: MDB_val = MDB_val { mv_size: 1, mv_data: &mut i as *mut _ as *mut c_void };
                let res = mdb_cursor_put(cptr, &mut keyval as *mut _, &mut valval as *mut _, MDB_NOOVERWRITE);
                assert_eq!(0, res);
            }
            let mut rangekey: MDB_val = MDB_val { mv_size: 1, mv_data: &mut 1 as *mut _ as *mut c_void };
            let mut valnew2: MDB_val = MDB_val { mv_size: 0, mv_data: null_mut() };
            let res_get2 = mdb_cursor_get(cptr, &mut rangekey as *mut _, &mut valnew2 as *mut _, MDB_cursor_op::MDB_SET_RANGE);
            assert_eq!(0, res_get2);
            assert_eq!(1, valnew2.mv_size);
            let range_val: u8 = *(valnew2.mv_data as *const u8);
            assert_eq!(1, range_val);
            let res_get3 = mdb_cursor_get(cptr, &mut rangekey as *mut _, &mut valnew2 as *mut _, MDB_cursor_op::MDB_FIRST);
            assert_eq!(0, res_get3);
            assert_eq!(1, valnew2.mv_size);
            let range_val_first: u8 = *(valnew2.mv_data as *const u8);
            assert_eq!(1, range_val_first);
            for i in 2..100 {
                let res = mdb_cursor_get(cptr, &mut rangekey as *mut _, &mut valnew2 as *mut _, MDB_cursor_op::MDB_NEXT);
                assert_eq!(0, res);
                let new_val: u8 = *(valnew2.mv_data as *const u8);
                assert_eq!(i, new_val);
            }
            mdb_cursor_close(cptr);
            let res_cls = mdb_txn_commit(tptr);
            assert_eq!(0, res_cls);
            mdb_env_close(eptr);
        }
    }
}
