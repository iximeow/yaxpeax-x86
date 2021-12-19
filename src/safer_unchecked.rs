use core::slice::SliceIndex;

pub trait GetSaferUnchecked<T> {
    unsafe fn get_kinda_unchecked<I>(&self, index: I) -> &<I as SliceIndex<[T]>>::Output
    where
        I: SliceIndex<[T]>;
}

impl<T> GetSaferUnchecked<T> for [T] {
    unsafe fn get_kinda_unchecked<I>(&self, index: I) -> &<I as SliceIndex<[T]>>::Output
    where
        I: SliceIndex<[T]>,
    {
        if cfg!(debug_assertions) {
            &self[index]
        } else {
            self.get_unchecked(index)
        }
    }
}

pub unsafe fn unreachable_kinda_unchecked() -> ! {
    if cfg!(debug_assertions) {
        panic!("UB: Unreachable unchecked was executed")
    } else {
        core::hint::unreachable_unchecked()
    }
}
