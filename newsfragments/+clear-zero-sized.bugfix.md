Clearing a zero-sized place, as `TSlot<Unit>::clear`, `StorageMap<K, Unit>::remove` and `clear(mut unit)` do, compiles to nothing instead of failing as an unsupported extern call.
