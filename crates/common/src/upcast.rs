pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
