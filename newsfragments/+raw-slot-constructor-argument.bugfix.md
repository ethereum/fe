A raw-slot constructor such as `StorageMap::at(slot)`, or a generic one built on `target_mut`, no longer fails with an internal error (`16-0004`) when its slot is a variable rather than a literal.
