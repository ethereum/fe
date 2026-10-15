`std::evm::random::shuffle_slice` is now safe: it takes the slice as `mut MemSlice<T>` and indexes it with bounds checks, instead of being an `unsafe fn` that wrote through a read-only view.
