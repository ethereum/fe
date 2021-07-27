# Memory

Only sequence types can be stored in memory.

The first memory slot (`0x00`) is used to keep track of the lowest available memory slot. Newly
allocated segments begin at the value given by this slot. When more memory has been allocated,
the value stored in `0x00` is increased.

We do not free memory after it is allocated.