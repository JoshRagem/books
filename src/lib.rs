#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate libc;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;
    use std::ffi::CString;
    use std::ptr::null_mut;
    //use libc::perror;

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
            //perror(path.as_ptr());
            assert_eq!(0, res_open);
            let mut txn: MDB_txn = mem::zeroed();
            let mut tptr: *mut MDB_txn = &mut txn;
            let res_txn = mdb_txn_begin(eptr, null_mut(), 0, &mut tptr as *mut _);
            assert_eq!(0, res_txn);
            let mut dbi: MDB_dbi = mem::zeroed();
            let db_name = CString::new("test").unwrap();
            let res_dbi = mdb_dbi_open(tptr, db_name.as_ptr(), MDB_CREATE, &mut dbi as *mut _);
            assert_eq!(0, res_dbi);
        }
    }
}
