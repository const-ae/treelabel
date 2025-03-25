# treelabel 0.0.7

* Add new function `test_abundance_changes`.
* Allow access to values inside vector with dollar (`vec$node`)
* Allow `.p` in `tl_tree_filter` to be a character vector (in addition to be a function)
* Fix bug in `tl_tree_filter` if .p returned an empty vector

# treelabel 0.0.5

* Enable combining treelabel vectors where one is build around a numeric score matrix and the other
is based on a logical score matrix.
* Rename `tl_modify_tree` to `tl_tree_modify`
* Add `tl_tree_filter` and `tl_tree_cut` for modifying the underlying tree
* Make `tl_eval` more flexible about the output.
