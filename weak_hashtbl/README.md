# Weak_hashtbl

A single-module library with a hashtable that keeps a weak pointer to
each key's data and uses a finalizer to detect when the data is no
longer referenced (by any non-weak pointers).
